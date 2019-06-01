;;; bilist.el ---                     -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2019  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: extensions lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A "bilist" is a bi-directional linked list with a couple features
;; to make it suitable for storing user movement history in Emacs.

;; Each element has an index.  0 is canonically the first element, and
;; will be referenced in this library's code as its "head" element.
;; The element with the highest index is referred to as the "tail"
;; element.

;; Each list has an additional "pointer" that can point to any of its
;; elements, or just past the last element, which is where the pointer
;; starts.  This was created to keep track of where the user is
;; navigating through history, as the user navigates back and forth
;; through history.

;; As elements are inserted or deleted from a list, the pointer is
;; shifted as necessary so that it always points to a valid element in
;; the list.

;; Bilists also have an optional maximum length, and an expiration
;; policy.  When adding elements that would cause the bilist's maximum
;; length to be exceeded, some elements are deleted from the list,
;; which is referred to as "expiring elements".  Elements may be
;; expired from the head, the tail, or expired in such a way as to try
;; to keep a certain ratio of elements before and after the pointer.
;; This final option may make sense when storing history: if the user
;; has navigated around in their history such that the pointer is in
;; the middle of the history, you may want to keep an equal number of
;; elements for navigating "forward" as well as "back", rather than
;; always just expiring previous or following locations in the
;; history.  (Your author has implemented this feature, and is using
;; it, but remains skeptical that it makes sense, let alone that it is
;; useful.)

;; Compatibility with seq.el is provided.

;; Implementation notes:

;; Internally, bilists are implemented as circular linked lists, per
;; Cormen et al.'s suggestion.  This greatly simplified the
;; implementation, I think.

;; When you suspect a bug, calling `bilist--install-advice' may be
;; helpful.  It will advise all functions in this library that change
;; a bilist and assert pre- and post-conditions about the integrity of
;; a bilist on each call.  This advice is somewhat costly, so running
;; with it on normally is probably not great.

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'macroexp)

(define-error 'bilist-error "generic bilist error")
(define-error 'bilist-out-of-range "bilist args out of range" 'bilist-error)

(defconst bilist--sentinel-value (make-symbol "bilist-sentinel-sym")
  "Internal value used on sentinel cells.
This is currently only used to make it easier for humans to
identify these cells when printing them.")

(cl-defstruct (bilist
                (:constructor nil)
                (:constructor bilist--st-new)
                (:copier blist--st-copy)
                (:conc-name bilist--st-))
  sentinel
  len
  max-len
  expire-policy
  ptr
  ptr-idx)

(defmacro bilist--with-slots (slot-bindings struct-type inst &rest body)
  "An incomplete version of `cl-with-slots' for use with structs.
SLOT-BINDINGS can be of the form SLOT, which binds the slot to a
variable with the same name, or (VAR SLOT), which binds VAR to
the value of SLOT.

Note that modifying the bound variables does not change the
slot's value in INST."
  (declare (indent 3) (debug (sexp sexp sexp body)))
  (let ((inst-var (gensym)))
    `(let* ,(cons (list inst-var inst)
                  (mapcar (lambda (binding)
                            (let (slot var)
                              (if (consp binding)
                                  (setq slot (cdr binding) var (car binding))
                                (setq slot binding var binding))
                              (list var
                                    `(cl-struct-slot-value ,struct-type ',slot
                                                           ,inst-var))))
                          slot-bindings))
       ,@body)))

(defmacro bilist--st-with-slots (slot-bindings inst &rest body)
  "Bind slots from a `bilist' struct into local vars, poorly.
See `bilist--with-slots'."
  (declare (indent 2) (debug (sexp sexp body)))
  `(bilist--with-slots ,slot-bindings 'bilist ,inst ,@body))

(defun bilist--could-be-cell-p (cell)
  (and (consp cell)
       (consp (cadr cell))
       (consp (cddr cell))))

(defun bilist--check (bilist &rest _args)
  "Check various aspects of BILIST.  Used for debugging.
ARGS is the same ARGS from `bilist--check-around-advice', passed
in to this function to ease debugging a bit."
  (bilist--st-with-slots (sentinel len max-len ptr ptr-idx) bilist
    (cl-assert (>= len 0))
    (cl-assert (or (null max-len) (>= max-len 0)) t)
    (cl-assert (or (null max-len) (<= len max-len)) t)
    (cl-assert (>= ptr-idx 0))
    (cl-assert (<= ptr-idx len))
    (cl-assert (bilist--could-be-cell-p sentinel) t)
    (cl-assert (eq (car sentinel) bilist--sentinel-value))
    (cl-assert (bilist--could-be-cell-p ptr) t)
    (cl-assert (eq (eq ptr sentinel) (eq ptr-idx len)) nil
               "ptr-idx=%d len=%d ptr%ssentinel" ptr-idx len
               (if (eq ptr sentinel) "==" "!="))
    (cl-assert (eq (eq (car ptr) bilist--sentinel-value) (eq ptr sentinel)))
    (cl-assert (eq (zerop len) (eq sentinel (cddr sentinel))))
    (cl-assert (eq (zerop len) (eq sentinel (cadr sentinel))))
    (let ((check-len 0)
          check-ptr-idx)
      (cl-loop
         for cell = (cddr sentinel) then (cddr cell)
         do (when (eq cell ptr) (setq check-ptr-idx check-len))
         until (eq cell sentinel)
         do (cl-incf check-len))
      (cl-assert (= len check-len) t)
      (cl-assert (= ptr-idx check-ptr-idx) t))))

(defun bilist--find-mutator-name-from-call-stack ()
  "Return name of function in call stack advised with
`bilist--check-around-advice.'"
  (catch 'found
    (mapbacktrace (lambda (_evald func _args _flags)
                    (when (and (symbolp func)
                               (advice-member-p #'bilist--check-around-advice
                                                func))
                      (throw 'found func))))
    nil))

(defun bilist--check-around-advice (orig-fun bilist &rest args)
  (condition-case err
      (bilist--check bilist args)
    (error
     (message "bilist already invalid before %S: %S"
              (cons (bilist--find-mutator-name-from-call-stack) args) err)))
  (unwind-protect
       (apply orig-fun bilist args)
    (condition-case err
        (bilist--check bilist args)
      (error
       (message "bilist is invalid at end of %S: %S"
                (cons (bilist--find-mutator-name-from-call-stack) args) err)))))

(defvar bilist--mutator-functions nil
  "Internal list of functions known to mutate bilists.
All functions in this list must take a bilist as their first
argument.  See also `bilist--install-check-advice' and
`bilist--remove-check-advice'.")

(defun bilist--register-mutator (function)
  "Register FUNCTION as a function that mutates bilists.
This is an internal convenience function.  See also
`bilist--mutator-functions'."
  (cl-pushnew function bilist--mutator-functions))

(defun bilist--install-check-advice ()
  "Add `bilist--check-around-advice' to functions that mutate bilists.
See also variable `bilist--mutator-functions'."
  (unless (and (numberp print-level) (<= 2 print-level))
    (warn "Setting `print-level' to 5 and turning on `print-circle'")
    (setq print-circle t print-level 5))
  (dolist (func bilist--mutator-functions)
    (advice-add func :around #'bilist--check-around-advice)))

(defun bilist--remove-check-advice ()
  "Remove `bilist--check-around-advice' from functions that mutate bilists.
See also variable `bilist--mutator-functions'."
  (dolist (func bilist--mutator-functions)
    (advice-remove func #'bilist--check-around-advice)))

(defun bilist--check-expire-policy (expire-policy)
  (unless (or (memq expire-policy '(head tail))
              (and (>= expire-policy 0.0) (<= expire-policy 1.0)))
    (signal 'bilist-error (format "Invalid expire-policy: %S" expire-policy))))

(defun bilist-new (&optional values max-len expire-policy)
  "Return a new bilist containing VALUES.

The bilist's max length is set to MAX-LEN.  If MAX-LEN is nil,
there is no limit on the number of items that can be stored in
the bilist.

EXPIRE-POLICY dictates how elements are expired (removed) if
MAX-LEN is non-nil and elements are added to the list such that
its length exceeds MAX-LEN.  This argument can be:

* `head', which means to always expire elements starting at index
  0 (\"the head\")

* `tail', which means to expire elements starting at the last
  index (\"the tail\")

* A number in the range [0.0, 1.0] which indicates the percentage
  of the elements in the list to try and keep before the current
  pointer.  For example, in a list of 10 elements with the
  pointer at index 2 and an EXPIRE-POLICY of 0.5, elements will
  be expired from the tail first, in an attempt to achieve the
  desired ratio 0.5.  (Of course, the desired ratio cannot be
  achieved unless you also either insert some elements at head
  and/or advance the pointer.)

This option for a ratio centered around the pointer was
originally intended for when bilist is used as a history of
movements (i.e. positions in buffers), new history records are
inserted at the pointer, and the user is equally likely to move
backwards as well as forwards in history.  In this case, an
EXPIRE-POLICY of 0.5 is hypothesized to at least try to maximize
the user's options to move both backwards and forwards in
history.

`head' is used if EXPIRE-POLICY is nil."
  (unless (or (null max-len) (> max-len 0))
    (signal 'bilist-error (format "Invalid maximum length: %S" max-len)))
  (when (null expire-policy)
    (setq expire-policy 'head))
  (bilist--check-expire-policy expire-policy)
  (let* ((sentinel (cl-list* bilist--sentinel-value nil nil))
         (prev sentinel)
         (len 0)
         bilist)
    (setf (cddr sentinel) sentinel)
    (seq-doseq (value values)
      (let ((new-cell (cl-list* value prev sentinel)))
        (setf (cddr prev) new-cell
              prev new-cell)
        (cl-incf len)))
    (setf (cadr sentinel) prev
          bilist (bilist--st-new :sentinel sentinel
                                 :len len
                                 :max-len max-len
                                 :expire-policy expire-policy
                                 :ptr sentinel
                                 :ptr-idx len))
    (bilist--make-room bilist 0)
    bilist))

(defun bilist-copy (bilist)
  "Return a shallow copy of BILIST.
All values are copied to the new bilist, as is BILIST's maximum
length, its expire policy, and the position of its pointer."
  (let ((new-bilist (bilist-new nil
                                (bilist--st-max-len bilist)
                                (bilist--st-expire-policy bilist))))
    (bilist-insert-bilist new-bilist bilist)
    (bilist-set-ptr-idx bilist (bilist--st-ptr-idx bilist))
    new-bilist))

(defun bilist-len (bilist)
  "Return the number of values in BILIST."
  (bilist--st-len bilist))

(defun bilist-empty-p (bilist)
  "Return non-nil if BILIST is empty."
  (zerop (bilist--st-len bilist)))

(defun bilist-expire-policy (bilist)
  "Return the expiration policy for BILIST.
This will return `head', `tail', or a number in the range [0.0,
1.0].  See `bilist-new' for a discussion of what these values
mean."
  (bilist--st-expire-policy bilist))

(defun bilist-set-expire-policy (bilist expire-policy)
  "Set the expiration policy of BILIST to EXPIRE-POLICY.
See `bilist-expire-policy'."
  (bilist--check-expire-policy expire-policy)
  (setf (bilist--st-expire-policy bilist) expire-policy))

(gv-define-simple-setter bilist-expire-policy bilist-set-expire-policy)

(defun bilist-max-len (bilist)
  "Return the maximum number of values BILIST is configured to hold.
This can be used as a place form."
  (bilist--st-max-len bilist))

(defun bilist-set-max-len (bilist max-len)
  "Change the maximum number of values BILIST can hold to MAX-LEN.
If MAX-LEN is nil then the bilist has no limit on the number of
values it can hold.  Otherwise, MAX-LEN must be at least 1.

If the list currently holds more than MAX-LEN values, it will be
immediately truncated according to its configured expiration
policy."
  (unless (or (null max-len) (>= max-len 1))
    (signal 'bilist-error (format "Invalid maximum length: %S" max-len)))
  (setf (bilist--st-max-len bilist) max-len)
  (when max-len
    (bilist--make-room bilist 0))
  max-len)

(bilist--register-mutator 'bilist-set-max-len)

(gv-define-simple-setter bilist-max-len bilist-set-max-len)

(defun bilist-ptr-idx (bilist)
  "Return the current pointer index of BILIST.
This will always be a number between 0 and the length of BILIST."
  (bilist--st-ptr-idx bilist))

(defun bilist-set-ptr-idx (bilist idx &optional from-ptr)
  "Set the current pointer index of BILIST to IDX.

If FROM-PTR is nil, IDX is relative to index 0.  Otherwise, IDX
is taken to be relative to the current pointer index.  IDX may be
negative.

Returns the new pointer index as a positive number relative to
index 0."
  (let* ((abs-idx (bilist--resolve-index bilist idx from-ptr t))
         (cell (bilist--cell-ref bilist abs-idx)))
    (setf (bilist--st-ptr bilist) cell
          (bilist--st-ptr-idx bilist) abs-idx)))

(bilist--register-mutator 'bilist-set-ptr-idx)

(gv-define-setter bilist-ptr-idx (idx bilist &optional from-ptr)
  `(bilist-set-ptr-idx ,bilist ,idx ,from-ptr))

(defun bilist-ptr-at-end-p (bilist)
  "Return non-nil if BILIST's pointer is positioned after the last element."
  (eq (bilist--st-ptr bilist) (bilist--st-sentinel bilist)))

(defun bilist--resolve-index (bilist idx from-ptr tail-ok)
  "Return a positive, absolute index into BILIST from IDX.

If FROM-PTR is nil then IDX is relative to index 0.  Otherwise,
IDX is relative to the current pointer index.

If IDX is nil then it defaults to 0.
If IDX is t then it is taken to be the length of BILIST.

If IDX is negative then this function counts backwards from the
anchor point, either index 0 or the pointer index.

If the computed index does not exist in the bilist, this function
signals `bilist-out-of-range'.  If the computed index refers to
the position after the last value in BILIST (that is, the
computed index equals the length of BILIST), then this function
returns nil if TAIL-OK is non-nil, or else it signals
`bilist-out-of-range'."
  (let* ((len (bilist--st-len bilist))
         (abs-idx (cond
                    ((null idx) 0)
                    ((eq idx t) len)
                    (from-ptr (+ (bilist--st-ptr-idx bilist) idx))
                    ((< idx 0) (+ len idx))
                    (t idx))))
    (unless (and (>= abs-idx 0) (<= abs-idx (if tail-ok len (1- len))))
      (signal 'bilist-out-of-range (cons bilist idx)))
    abs-idx))

(defun bilist--resolve-offset (bilist abs-idx offset)
  "Return an absolute index OFFSET elements from ABS-IDX on BILIST.

This may return the index just past the last element in
BILIST (which is the same as returning the length of BILIST).

This function is typically used by functions which take a
starting index plus a length from that index.  This function is
used to compute the absolute ending index."
  (if (null offset)
      (bilist--st-len bilist)
    (let ((abs-end (+ abs-idx offset)))
      (unless (and (<= abs-end abs-idx) (<= abs-end (bilist--st-len bilist)))
        (signal 'bilist-out-of-range (cons bilist offset)))
      abs-end)))

(defun bilist--cell-ref (bilist abs-idx &optional other-abs-idx other-cell)
  "Return the cell at ABS-IDX on BILIST.

As an optimization, if you have already retrieved another cell
from bilist, you may pass its absolute index and the cell as
OTHER-ABS-IDX and OTHER-CELL, respectively.  This function may
then search for ABS-IDX from that OTHER-CELL, if that will
require the fewest operations."
  (cl-assert (eq (not other-abs-idx) (not other-cell)))
  (bilist--st-with-slots (sentinel len ptr ptr-idx) bilist
    (cl-assert (and (>= abs-idx 0) (<= abs-idx len)))
    (let* ((head-delta abs-idx)
           (tail-delta (- len abs-idx))
           (ptr-delta (- abs-idx ptr-idx))
           (other-delta (and other-abs-idx (- other-abs-idx abs-idx)))
           shortest-path smallest-delta
           cell num-moves move-fn)
      (setq shortest-path 'head smallest-delta head-delta)
      (when (< tail-delta smallest-delta)
        (setq shortest-path 'tail smallest-delta tail-delta))
      (when (< (abs ptr-delta) smallest-delta)
        (setq shortest-path 'ptr smallest-delta ptr-delta))
      (when (and other-delta (< (abs other-delta) smallest-delta))
        (setq shortest-path 'other smallest-delta other-delta))
      (cl-ecase shortest-path
        (head (setq cell (cddr sentinel)
                    num-moves abs-idx
                    move-fn #'cddr))
        (tail (setq cell sentinel
                    num-moves tail-delta
                    move-fn #'cadr))
        (ptr (setq cell ptr
                   num-moves (abs ptr-delta)
                   move-fn (if (< ptr-delta 0) #'cadr #'cddr)))
        (other (setq cell other-cell
                     num-moves (abs other-delta)
                     move-fn (if (< other-delta 0) #'cadr #'cddr))))
      (while (> num-moves 0)
        (setq cell (funcall move-fn cell))
        (cl-decf num-moves))
      cell)))

(defun bilist--copy-range (start end)
  "Return a copy of the cells from START, inclusive, to END, exclusive.
The returned cell is the copy of START.  The cell prior to START
will be a sentinel cell."
  (let* ((sentinel (cl-list* bilist--sentinel-value nil nil))
         (prev sentinel)
         (cell start))
    (setf (cddr sentinel) sentinel)
    (while (not (eq cell end))
      (let ((new-cell (cl-list* (car cell) prev sentinel)))
        (setf (cddr prev) new-cell
              prev new-cell
              cell (cddr cell))))
    (setf (cadr sentinel) prev)
    (cddr sentinel)))

(defun bilist--clamp (n min max)
  "Clamp N to between MIN and MAX, inclusive."
  (min (max n min) max))

(defun bilist--make-room (bilist how-much-room &optional cell-abs-idx)
  "Expire (remove) cells from BILIST to make room for HOW-MUCH-ROOM new cells.

Cells are expired according to BILIST's expiration policy.  See
`bilist-expire-policy'.

This function will always expire cells if the BILIST's length is
greater than its configured maximum length, no matter the value
of HOW-MUCH-ROOM.

HOW-MUCH-ROOM may be greater than BILIST's maximum length, in
which case all cells will be expired.

If CELL-ABS-IDX is non-nil then it specifies that the caller
needs to know if the index of this cell moves due to expiration.
If the cell is moved, its new absolute index is returned by this
function.  If the cell at CELL-ABS-IDX is expired by this
function then this function returns 0 if the cell was expired
from the head, or else the new length of BILIST if cell was
expired from the tail.  If CELL-ABS-IDX is not moved by this
function then CELL-ABS-IDX is returned.  (Note that this last
case can only happen when no cells are expired, or when only
cells after CELL-ABS-IDX are expired from the tail.)

If CELL-ABS-IDX is nil then this function returns nil."
  (cl-assert (>= how-much-room 0))
  (bilist--st-with-slots (len max-len expire-policy sentinel ptr-idx) bilist
    (let* ((num-to-remove (and max-len (- (+ len how-much-room) max-len))))
      (cond
        ((or (null num-to-remove) (<= num-to-remove 0))
         cell-abs-idx)
        ((>= num-to-remove len)
         (bilist--delete-cells bilist 0 (cddr sentinel) len sentinel)
         0)
        (t
         (let* ((num-to-remove-head
                 (cl-case expire-policy
                   (head num-to-remove)
                   (tail 0)
                   (t (bilist--clamp (- ptr-idx
                                        ;; This is the ideal pointer
                                        ;; index based on the given
                                        ;; ratio.
                                        (round (* (- len num-to-remove)
                                                  expire-policy)))
                                     0 num-to-remove))))
                (num-to-remove-tail (- num-to-remove num-to-remove-head))
                (head-end (bilist--cell-ref bilist num-to-remove-head))
                (tail-abs-idx (- len num-to-remove-tail))
                (tail-start (bilist--cell-ref bilist tail-abs-idx)))
           ;; Remove from tail before head, since removing from
           ;; head first would then require the additional
           ;; complication of adjusting indexes around tail to
           ;; account for items removed at head.
           (bilist--delete-cells bilist tail-abs-idx tail-start
                                 len sentinel)
           (bilist--delete-cells bilist 0 (cddr sentinel)
                                 num-to-remove-head head-end)
           (cond
             ((null cell-abs-idx)
              cell-abs-idx)
             ((>= cell-abs-idx (- len num-to-remove-tail))
              (- len num-to-remove))
             (t
              (max (- cell-abs-idx num-to-remove-head) 0)))))))))

(bilist--register-mutator 'bilist--make-room)

(defun bilist--insert-cells (dst dst-abs-idx src-start src-last range-len)
  "Insert cells into bilist DST.

Cells are being inserted at absolute index DST-ABS-IDX, and
DST-AFTER is the cell at that index.  The cells to be inserted
are the cells between SRC-START and SRC-LAST, inclusive.  These
cells will be modified.  RANGE-LEN is the number of cells between
SRC-START and SRC-LAST, inclusive.

If necessary, this function will expire existing values from DST
*before* inserting the new ones (see `bilist--make-room').  If
there are still too many values in DST after inserting values,
some of the new values will be immediately expired.

Returns the absolute index of the existing element before which
the new cells were inserted, which may be different from
DST-ABS-IDX if that index changed due to expiring cells."
  (setq dst-abs-idx (bilist--make-room dst range-len dst-abs-idx))
  ;; Purposely capturing `ptr-idx' after `bilist--make-room' call,
  ;; since that call could move `ptr-idx'.
  (bilist--st-with-slots (ptr-idx) dst
    (let* ((dst-after (bilist--cell-ref dst dst-abs-idx))
           (dst-before (cadr dst-after)))
      (setf (cddr dst-before) src-start
            (cadr src-start) dst-before
            (cddr src-last) dst-after
            (cadr dst-after) src-last)
      (cl-incf (bilist--st-len dst) range-len)
      (when (>= ptr-idx dst-abs-idx)
        (cl-incf (bilist--st-ptr-idx dst) range-len))))
  (bilist--make-room dst 0 dst-abs-idx))

(bilist--register-mutator 'bilist--insert-cells)

(defun bilist--delete-cells (bilist abs-idx start abs-end end)
  "Delete cells between START, inclusive, and END, exclusive, from BILIST.
ABS-IDX is the absolute index of start, ABS-END is the absolute
index of END.

When the pointer was on one of the deleted cells, the pointer is
moved to END.

Returns the number of cells deleted."
  (let ((before (cadr start))
        (ptr-idx (bilist--st-ptr-idx bilist))
        (range-len (- abs-end abs-idx)))
    ;; It is important, at least to `bilist-insert-bilist' and maybe
    ;; some other functions, that the cells being removed are not
    ;; modified.
    (setf (cddr before) end
          (cadr end) before)
    (cl-decf (bilist--st-len bilist) range-len)
    (when (>= ptr-idx abs-idx)
      (if (< ptr-idx abs-end)
          (setf (bilist--st-ptr bilist) end
                (bilist--st-ptr-idx bilist) (- abs-end range-len))
        (cl-decf (bilist--st-ptr-idx bilist) range-len)))
    range-len))

(bilist--register-mutator 'bilist--delete-cells)

(cl-defun bilist-insert-bilist (dst src
                                &key
                                  dst-idx dst-from-ptr
                                  src-idx src-len src-from-ptr
                                  move)
  "Insert values from bilist SRC into bilist DST.

Insertion into DST happens prior to DST-IDX, which is relative to
index 0, unless DST-FROM-PTR is non-nil in which case DST-IDX is
relative to DST's pointer index.

SRC-LEN values from SRC are copied/moved starting from SRC-IDX,
which is relative to index 0, unless SRC-FROM-PTR is non-nil in
which case SRC-IDX is relative to SRC's pointer index.

If MOVE is nil, the values are copied from SRC to DST, and SRC is
not modified.  If MOVE is non-nil, the values are moved out of
SRC and into DST.

Values are expired from DST if necessary.  Note that this may
cause DST's pointer to be moved.

DST's pointer will keep pointing at the same value, though the
pointer index may change if the values are inserted prior to the
pointer.

When moving cells from SRC, if SRC's pointer is on one of the
cells being moved, SRC's pointer will be moved to the position
following the last value moved out of SRC.

Returns the number of values copied/moved."
  (let* ((src-abs-idx (bilist--resolve-index src src-idx src-from-ptr t))
         (src-abs-end (bilist--resolve-offset src src-abs-idx src-len))
         (range-len (- src-abs-end src-abs-idx))
         (src-start (bilist--cell-ref src src-abs-idx))
         (src-end (bilist--cell-ref src src-abs-end src-abs-idx src-start))
         src-last
         (dst-abs-idx (bilist--resolve-index dst dst-idx dst-from-ptr t)))
    (unless (eq src-start src-end)
      (if move
          (progn
            (setq src-last (cadr src-end))
            (bilist--delete-cells src src-abs-idx src-start
                                  src-abs-end src-end))
        (setq src-start (bilist--copy-range src-start src-end)
              src-end (cadr src-start)
              src-last (cadr src-end)))
      (bilist--insert-cells dst dst-abs-idx src-start src-last
                            range-len))
    range-len))

(bilist--register-mutator 'bilist-insert-bilist)

(cl-defun bilist-delete-range (bilist &optional idx len from-ptr)
  "Delete LEN values from BILIST starting at index IDX.

If FROM-PTR is nil then IDX is relative to index 0, otherwise it
is relative to the current pointer index.

Returns the number of values deleted."
  (let* ((abs-idx (bilist--resolve-index bilist idx from-ptr t))
         (abs-end (bilist--resolve-offset bilist abs-idx len))
         (start (bilist--cell-ref bilist abs-idx))
         (end (bilist--cell-ref bilist abs-end abs-idx start)))
    (bilist--delete-cells bilist abs-idx start abs-end end)))

(bilist--register-mutator 'bilist-delete-range)

(defun bilist-insert (bilist idx value &optional from-ptr move-ptr)
  "Insert VALUE into BILIST at position IDX.

IDX is taken to be relative to index 0, unless FROM-PTR is
non-nil in which case IDX is taken relative to BILIST's current
pointer index.

If MOVE-PTR is nil, BILIST's pointer will still point at the same
element it did before this call, though that element's index may
have been changed by insertion and/or expiration.

If MOVE-PTR is non-nil, BILIST's pointer is moved to the position
of the newly-inserted element.

If BILIST is at its maximum length, an existing value will be
expired before inserting VALUE.  Note that this may cause
BILIST's pointer to be moved, even if MOVE-PTR is nil.

Returns VALUE."
  (let* ((abs-idx (bilist--resolve-index bilist idx from-ptr t))
         (new-cell (cl-list* value nil nil)))
    (setq abs-idx (bilist--insert-cells bilist abs-idx new-cell new-cell 1))
    (when move-ptr
      (setf (bilist--st-ptr bilist) (bilist--cell-ref bilist abs-idx)
            (bilist--st-ptr-idx bilist) abs-idx)))
  value)

(bilist--register-mutator 'bilist-insert)

(defun bilist-append (bilist value &optional move-ptr)
  "Append VALUE to BILIST.

If MOVE-PTR is nil, BILIST's pointer will still point at the same
element it did before this call, though that element's index may
have been changed by insertion and/or expiration.

If MOVE-PTR is non-nil, BILIST's pointer is moved to the position
of the newly-inserted element.

If BILIST is at its maximum length, an existing value will be
expired before appending VALUE.  Note that this may cause
BILIST's pointer to be moved even if MOVE-PTR is nil.

Returns VALUE."
  (bilist-insert bilist t value nil move-ptr))

(bilist--register-mutator 'bilist-append)

(defun bilist-delete (bilist idx &optional from-ptr)
  "Delete the value at IDX from BILIST.

IDX is relative to index 0, unless FROM-PTR is non-nil, in which
case IDX is relative to BILIST's pointer index.

If MOVE-PTR is non-nil, the pointer is unconditionally moved to
the position after the deleted element.

If MOVE-PTR is nil and the pointer was on the value being
deleted, the pointer is moved to the following position, which may
be past the last element in BILIST if the last element was
deleted.

Returns the deleted value."
  (let* ((abs-idx (bilist--resolve-index bilist idx from-ptr nil))
         (cell (bilist--cell-ref bilist abs-idx)))
    (bilist--delete-cells bilist abs-idx cell (1+ abs-idx) (cddr cell))
    (car cell)))

(bilist--register-mutator 'bilist-delete)

(cl-defun bilist-ref (bilist idx &optional
                                   from-ptr move-ptr (default nil default-p))
  "Return the value at IDX on BILIST.

IDX is taken to be relative to index 0 if FROM-PTR is nil,
otherwise IDX is relative to BILIST's pointer index.

IDX may be negative, in which case it refers to the nth prior
element.

If MOVE-PTR is non-nil then BILIST's pointer is moved to point at
the requested element.

If a non-existent element is specified then this function signals
`bilist-out-of-range', unless DEFAULT is given, in which case
DEFAULT is returned.  DEFAULT may be nil, in which case an
invalid reference will return nil.

This function is also a place form."
  (condition-case err
      (let* ((abs-idx (bilist--resolve-index bilist idx from-ptr nil))
             (cell (bilist--cell-ref bilist abs-idx)))
        (when move-ptr
          (setf (bilist--st-ptr bilist) cell
                (bilist--st-ptr-idx bilist) abs-idx))
        (car cell))
    (bilist-out-of-range
     (if default-p
         default
       (signal (car err) (cdr err))))))

(bilist--register-mutator 'bilist-ref)

(cl-defun bilist-set (bilist idx value &optional from-ptr move-ptr)
  "Set index IDX on BILIST to VALUE.

IDX is taken to be relative to index 0, unless FROM-PTR is
non-nil in which case IDX is relative to BILIST's pointer index.

If MOVE-PTR is non-nil then BILIST's pointer will be moved to point at the
element being changed.

Returns VALUE."
  (let* ((abs-idx (bilist--resolve-index bilist idx from-ptr nil))
         (cell (bilist--cell-ref bilist abs-idx)))
    (when move-ptr
      (setf (bilist--st-ptr bilist) cell
            (bilist--st-ptr-idx bilist) abs-idx))
    (setcar cell value)))

(bilist--register-mutator 'bilist-set)

(gv-define-setter bilist-ref (value bilist idx &optional from-ptr move-ptr)
  `(bilist-set ,bilist ,idx ,value ,from-ptr ,move-ptr))

(defmacro bilist--with-gensyms (symbols &rest body)
  "Wrap BODY with every symbol in SYMBOLS bound to a unique symbol."
  "Execute BODY with symbols in SYMBOLS bound to unique symbols."
  (declare (indent 1) (debug (sexp body)))
  `(let ,(mapcar (lambda (sym) (list sym `(make-symbol ,(symbol-name sym))))
                 symbols)
     ,@body))

;; Indent property not sticking for some reason?
(put 'bilist--with-gensyms 'lisp-indent-function 1)

(cl-defmacro bilist-do ((var
                         bilist
                         &key
                         idx-var
                         idx
                         from-ptr
                         backward
                         move-ptr
                         result)
                        &rest body)
  (declare (indent 1) (debug (sexp body)))
  (let ((private-idx-var (when (or idx-var move-ptr) (gensym))))
    (bilist--with-gensyms (abs-idx cell step sentinel)
      (macroexp-let2* nil
          ((bilist bilist)
           (idx idx)
           (from-ptr from-ptr)
           (backward backward)
           (move-ptr move-ptr))
        `(let* ((,abs-idx (bilist--resolve-index ,bilist
                                                 (cond
                                                   (,idx)
                                                   (,from-ptr 0)
                                                   ((not ,backward) 0)
                                                   ;; This special
                                                   ;; case smells.
                                                   ((bilist-empty-p ,bilist) 0)
                                                   (t -1))
                                                 ,from-ptr t))
                (,cell (bilist--cell-ref ,bilist ,abs-idx))
                (,sentinel (bilist--st-sentinel ,bilist))
                ,var
                ,@(when private-idx-var
                    `((,private-idx-var ,abs-idx)
                      (,step (if ,backward -1 1))))
                ,@(when idx-var
                    `(,idx-var)))
           (while (not (eq ,cell ,sentinel))
             (setq ,var (car ,cell))
             (when ,move-ptr
               (setf (bilist--st-ptr ,bilist) ,cell
                     (bilist--st-ptr-idx ,bilist) ,private-idx-var))
             ,@(when idx-var
                 `((setq ,idx-var ,private-idx-var)))
             ,@body
             (setq ,cell (if ,backward
                             (cadr ,cell)
                           (cddr ,cell)))
             ,@(when private-idx-var
                 `((setq ,private-idx-var (+ ,private-idx-var ,step)))))
           ;; Just like `dolist'.
           (setq ,var nil)
           ,@(when idx-var `((setq ,idx-var nil)))
           ,result)))))

(cl-defmacro bilist--do-unsafe ((var bilist &optional result) &rest body)
  "Loop over BILIST, setting VAR to successive values of BILIST.
Simpler and somewhat cheaper than `bilist-do'.

DO NOT MODIFY VAR IN THE BODY.  That's why this function is
\"unsafe\"."
  (declare (indent 1) (debug (sexp body)))
  (bilist--with-gensyms (sentinel cell)
    `(let* ((,sentinel (bilist--st-sentinel ,bilist))
            (,cell (cddr ,sentinel))
            ,var)
       (while (not (eq ,cell ,sentinel))
         (setq ,var (car ,cell))
         ,@body
         (setq ,cell (cddr ,cell)))
       ,result)))

(defun bilist-delete-if (bilist predicate &optional idx len from-ptr)
  "Delete all values from BILIST for which PREDICATE returns non-nil."
  (let* ((abs-idx (bilist--resolve-index bilist idx from-ptr t))
         (abs-end (bilist--resolve-offset bilist abs-idx len))
         (cell (bilist--cell-ref bilist abs-idx))
         (end (bilist--cell-ref bilist abs-end abs-idx cell))
         (num-deleted 0)
         next)
    (while (not (eq cell end))
      ;; Saving the next cell before we (potentially) delete the
      ;; current one is not necessary as of the time of this writing,
      ;; since `bilist--delete-cells' doesn't modify the cells being
      ;; deleted, but it's low-cost future-proofing.
      (setq next (cddr cell))
      (if (not (funcall predicate (car cell)))
          (cl-incf abs-idx)
        (bilist--delete-cells bilist abs-idx cell (1+ abs-idx) next)
        (cl-incf num-deleted))
      (setq cell next))
    num-deleted))

(bilist--register-mutator 'bilist-delete-if)

(defun bilist-values (bilist &optional idx len from-ptr)
  "Return a list of the values in BILIST."
  (let* ((abs-idx (bilist--resolve-index bilist idx from-ptr t))
         (abs-end (bilist--resolve-offset bilist abs-idx len))
         (end (bilist--cell-ref bilist abs-end)))
    (cl-loop
       for cell = (bilist--cell-ref bilist abs-idx) then (cddr cell)
       until (eq cell end)
       collect (car cell))))

(cl-defmethod seqp ((_bilist bilist))
  t)

(cl-defmethod seq-elt ((bilist bilist) n)
  (bilist-ref bilist n))

(cl-defmethod seq-length ((bilist bilist))
  (bilist--st-len bilist))

(cl-defmethod seq-do (function (bilist bilist))
  (bilist--do-unsafe (val bilist bilist)
    (funcall function val)))

(cl-defmethod seq-subseq ((bilist bilist) start &optional end)
  (let ((new-bilist (bilist-new nil
                                (bilist--st-max-len bilist)
                                (bilist--st-expire-policy bilist))))
    (bilist-insert-bilist new-bilist bilist
                          :src-idx start :src-len (and end (- end start)))
    new-bilist))

(cl-defmethod seq-copy ((bilist bilist))
  (bilist-copy bilist))

(cl-defmethod seq-into ((bilist bilist) type)
  (cl-case type
    ((vector string)
     (let ((result (cl-ecase type
                     (vector (make-vector (bilist--st-len bilist) nil))
                     (string (make-string (bilist--st-len bilist) ?\0))))
           (idx 0))
       (bilist--do-unsafe (val bilist result)
         (aset result idx val)
         (cl-incf idx))))
    (list
     (bilist-values bilist))
    (otherwise
     (error "Not a sequence type name: %S" type))))

(provide 'bilist)
;;; bilist.el ends here
