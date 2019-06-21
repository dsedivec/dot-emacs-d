;;; nav-stack.el --- Navigate back and forth         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))

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

;; TODO: make a cool hydra and/or more binds, e.g. ones that skip to
;; other buffer

;; TODO: Probably break down and implement push predicates, because
;; *buffer-selection* probably doesn't ever want to be on my stack?

;; TODO: *Could* probably reap dead markers when traversing for moving
;; or composing a list.  Don't have to wait for compacting.  Top
;; concern is modifying bilist when caller doesn't expect it.

;; TODO: Don't auto-update nav-list buffers when they're not visible?

;; TODO: Significant movement shouldn't count invisible text, like
;; massive hidden subtrees in org buffers.  Maybe start measuring
;; [visible] lines moved instead?  There may be a function for that.
;; No idea how slow it is.

;; TODO: Maybe pop-mark should pop the top of nav-stack if it's the
;; same location that we're popping to?  Maybe same with the xref pop
;; command?

;; TODO: Maybe when moving *from* position at top of stack (N), *to*
;; position at N-1 on stack, realize that is manually popping and pop
;; rather than push?  Maybe conditional behavior?

;; TODO: Might like "go back *in this window*"

;; TODO: Maybe keep separate pointers into the stack for buffer,
;; window, and/or global (and maybe frame).  This way you can have one
;; command for "go to other thing in this buffer" without modifying
;; the global pointer.

;; TODO: M-- M-, repeatedly just bounces back and forth between same
;; point, growing stack.  Also, maybe the initial prefix should be
;; remembered for successive moves in this DWIM command M-,.

;;; Code:

(require 'cl-lib)
(require 'bilist)

(defgroup nav-stack nil
  "Navigate back and forth through your frame/window/point history."
  :prefix "nav-stack-"
  :group 'tools)

(defcustom nav-stack-quiet nil
  "Tell Nav-Stack to be less noisy."
  :type 'boolean)

(defcustom nav-stack-debug-enabled nil
  "Output extra debug messages when this variable is non-nil.
Alternatively, it can be a regular expression, and then only
message *templates* (see `nav-stack-debug') matching the
regexp will be output."
  :type '(choice boolean regexp))

;; This is not working sometimes, such as in nav-stack-ivy, for no
;; reason that I can find.  Need to ask on mailing list.  XXX

;; (define-inline nav-stack-debug (template &rest args)
;;   (inline-letevals (template . args)
;;     (inline-quote
;;      (when nav-stack-debug-enabled
;;        (when (or (not (stringp nav-stack-debug-enabled))
;;                  (string-match-p nav-stack-debug-enabled ,template))
;;          (let ((inhibit-message t))
;;            ;; Why doesn't this work?
;;            ,(cl-list* #'message
;;                       (if (inline-const-p template)
;;                           (concat "nav-stack: " template)
;;                         `(concat "nav-stack: " ,template))
;;                       args)))))))

(defmacro nav-stack-debug (template &rest args)
  (let ((msg (make-symbol "msg")))
    (macroexp-let2 nil template template
      `(when nav-stack-debug-enabled
         (let ((,msg (format-message ,template ,@args)))
           (unless (and (stringp nav-stack-debug-enabled)
                        (not (string-match-p nav-stack-debug-enabled ,msg)))
             (let ((inhibit-message t))
               (message "%s" ,msg))))))))

(defun nav-stack-toggle-debug (&optional regexp)
  (interactive
   (when current-prefix-arg
     (list (read-regexp "Message template regexp: "))))
  (cond
    (regexp
     (setq nav-stack-debug-enabled regexp)
     (message "Nav-Stack debugging on for messages matching %S" regexp))
    (t
     (setq nav-stack-debug-enabled (not nav-stack-debug-enabled))
     (message "Nav-Stack debugging is now %s"
              (if nav-stack-debug-enabled "on" "off")))))

(defcustom nav-stack-scope 'frame
  "Determines scope of the default navigation stack.
Can be the symbol `global', `frame', or `window'."
  :type '(choice
          (const 'global)
          (const 'frame)
          (const 'window)))

(defun nav-stack--get-frame (&optional frame-or-window)
  (cond
    ((null frame-or-window)
     (selected-frame))
    ((windowp frame-or-window)
     (window-frame frame-or-window))
    (t
     (cl-assert (framep frame-or-window) t)
     frame-or-window)))

(defun nav-stack--get-window (&optional frame-or-window)
  (cond
    ((null frame-or-window)
     (selected-window))
    ((framep frame-or-window)
     (frame-selected-window frame-or-window))
    (t
     (cl-assert (windowp frame-or-window) t)
     frame-or-window)))

(defun nav-stack--map-scopes (function)
  (interactive)
  (cl-ecase nav-stack-scope
    (global (funcall function nil))
    (frame (mapc function (frame-list)))
    (window (walk-windows function t t))))

(defvar nav-stack--scope-property-containers nil)

(defun nav-stack--scope-key (&optional frame-or-window)
  (cl-ecase nav-stack-scope
    (global t)
    (frame (nav-stack--get-frame frame-or-window))
    (window (nav-stack--get-window frame-or-window))))

(defun nav-stack--scope-property (property &optional frame-or-window)
  (let* ((container (gethash (nav-stack--scope-key frame-or-window)
                             nav-stack--scope-property-containers)))
    (and container (gethash property container))))

(defun nav-stack--set-scope-property (property value &optional frame-or-window)
  (let* ((scope-key (nav-stack--scope-key frame-or-window))
         (container (gethash scope-key nav-stack--scope-property-containers)))
    (unless container
      (setq container (puthash scope-key (make-hash-table)
                               nav-stack--scope-property-containers)))
    (puthash property value container)))

(gv-define-simple-setter nav-stack--scope-property
                         nav-stack--set-scope-property)

(defun nav-stack--new-bilist (&optional locations)
  (nav-stack-debug "creating new bilist")
  (bilist-new locations nav-stack-capacity 0.5))

(defun nav-stack--bilist (&optional frame-or-window)
  (or (nav-stack--scope-property 'bilist frame-or-window)
      (nav-stack--set-scope-property 'bilist (nav-stack--new-bilist)
                                     frame-or-window)))

(defun nav-stack--set-bilist (bilist &optional frame-or-window)
  (nav-stack--set-scope-property 'bilist bilist frame-or-window))

(defun nav-stack-reset-stack (&optional frame-or-window)
  (nav-stack--set-bilist (nav-stack--new-bilist) frame-or-window))

(defun nav-stack-reset-all-stacks ()
  (interactive)
  (nav-stack--map-scopes #'nav-stack-reset-stack))

(gv-define-setter nav-stack--bilist (bilist &optional frame-or-window)
  `(nav-stack--set-scope-property ,frame-or-window 'bilist ,bilist))

(defcustom nav-stack-capacity 500
  "Number of markers that will be stored on a navigation stack.
Note that markers are often pushed indiscriminately, though they
may be ignored while navigating through them (for example, see
`nav-stack-move-predicates'), so it typically makes
sense to set this to a number much higher than the actual
historical places you wish to be able to travel through."
  :type 'integer)

(defvar nav-stack--this-command-move-direction nil)

(defvar nav-stack--last-command-move-direction nil)

(defun nav-stack-this-command-is-move-p ()
  nav-stack--this-command-move-direction)

(defun nav-stack-repeated-move-p ()
  (and nav-stack--last-command-move-direction
       nav-stack--this-command-move-direction))

(defun nav-stack-repeated-move-same-direction-p ()
  (and nav-stack--last-command-move-direction
       (eq nav-stack--last-command-move-direction
           nav-stack--this-command-move-direction)))

(defmacro with-repeated-move (direction &rest body)
  (declare (indent 1) (debug (sexp body)))
  `(let ((nav-stack--this-command-move-direction ,direction)
         (nav-stack--last-command-move-direction
          nav-stack--this-command-move-direction))
     ,@body))

(defun nav-stack-make-location (&optional frame-or-window marker props)
  "Return a location for FRAME-OR-WINDOW and MARKER.
FRAME-OR-WINDOW defaults to the currently selected window.
MARKER defaults to a marker for point in the current buffer.

Returned \"location\" is a cons cell (WINDOW . MARKER)."
  (let ((window (nav-stack--get-window frame-or-window)))
    (unless marker
      (setq marker (with-current-buffer (or (minibuffer-selected-window)
                                            (window-buffer window))
                     (point-marker))))
    (cons marker (cons (cons 'window window) props))))

(defalias 'nav-stack-location-marker 'car)

(defun nav-stack-location-buffer (location)
  (marker-buffer (nav-stack-location-marker location)))

(defun nav-stack-location-property (location prop)
  (alist-get prop (cdr location)))

(gv-define-setter nav-stack-location-property (value location prop)
  `(setf (alist-get ,prop (cdr ,location)) ,value))

(defcustom nav-stack-after-move-hook nil
  "List of functions to be run after a successful move from the stack."
  :type 'hook)

(defun nav-stack-go-to-location (location &optional direction)
  "Go to the given LOCATION, which you got off the stack.

LOCATION must have a valid marker.

If RECORD is given then record that this command moved in a
particular direction `back' or `forward' on the stack.

Returns LOCATION."
  (nav-stack-debug "go to location %S" location)
  (let* ((marker (nav-stack-location-marker location))
         (buf (marker-buffer marker))
         (window (nav-stack-location-property location 'window)))
    (when (and (window-live-p window)
               (eq (window-buffer window) buf))
      (select-frame-set-input-focus (window-frame window))
      (select-window window))
    (switch-to-buffer buf)
    (goto-char marker))
  (cond
    ((and (null direction) (null nav-stack--this-command-move-direction))
     (setq nav-stack--this-command-move-direction t))
    (direction
     (setq nav-stack--this-command-move-direction direction)))
  (run-hooks 'nav-stack-after-move-hook)
  location)

(defun nav-stack-go-to-location-at-index (index &optional frame-or-window)
  (let* ((bilist (nav-stack--bilist frame-or-window))
         (ptr-idx (bilist-ptr-idx bilist)))
    (nav-stack-go-to-location (bilist-ref bilist index nil t)
                              (cond
                                ((< index ptr-idx) 'backward)
                                ((> index ptr-idx) 'forward)))))

;; Pretty sure Emacs has a bug where `sxhash-equal' on two markers
;; that point to the same place in the same buffer doesn't return the
;; same hash value.  This is a workaround that can eventually be taken
;; out when `sxhash-equal' is fixed.
;; XXX need to file bug

(defun nav-stack--sxhash-marker (marker)
  (logxor (sxhash-eq (marker-buffer marker))
          (marker-position marker)))

(define-hash-table-test 'nav-stack--marker-equal
    #'equal #'nav-stack--sxhash-marker)

(defun nav-stack-unique-move-predicate (location)
  (let ((visited-markers
         (or (nav-stack--scope-property 'visited-markers)
             (setf (nav-stack--scope-property 'visited-markers)
                   (make-hash-table :test 'nav-stack--marker-equal
                                    :weakness 'key)))))
    (let ((marker (nav-stack-location-marker location))
          (repeated-move-p (nav-stack-repeated-move-same-direction-p)))
      (if (and repeated-move-p
               (gethash marker visited-markers))
          (progn
            (nav-stack-debug "%S is not a unique marker, veoting" marker)
            nil)
        (if repeated-move-p
            (nav-stack-debug "permitting unique marker %S" marker)
          ;; (nav-stack-debug "new command, first and only unique marker now %S"
          ;;                      marker)
          (clrhash visited-markers))
        (puthash marker t visited-markers)))))

(defun nav-stack-not-current-location-predicate (location)
  (not (equal (nav-stack-location-marker location) (point-marker))))

(defun nav-stack-same-buffer-predicate (location)
  (let ((location-buffer (nav-stack-location-buffer location)))
    (eq location-buffer (or (minibuffer-selected-window)
                            (current-buffer)))))

(defun nav-stack-other-buffer-predicate (location)
  (let ((location-buffer (nav-stack-location-buffer location)))
    (not (eq location-buffer (or (minibuffer-selected-window)
                                 (current-buffer))))))

(defun nav-stack-specific-buffer-predicate (desired-buffer location)
  (eq (nav-stack-location-buffer location) desired-buffer))

(defun nav-stack-forbidden-modes-predicate (location)
  (let* ((buffer (nav-stack-location-buffer location))
         (buffer-mode (buffer-local-value 'major-mode buffer)))
    (not (provided-mode-derived-p buffer-mode
                                  'minibuffer-inactive-mode))))

(defcustom nav-stack-move-predicates
  '(
    nav-stack-not-current-location-predicate
    nav-stack-forbidden-modes-predicate
    nav-stack-unique-move-predicate
    )
  "List of functions that can reject a move to a particular location."
  :type '(repeat function))

(defun nav-stack--check-predicates (predicates &rest args)
  "Return the first predicate from PREDICATES that failed, or else nil.
Each predicate is called with ARGS."
  (catch 'failure
    (dolist (predicate predicates)
      (unless (apply predicate args)
        (throw 'failure predicate)))))

(defun nav-stack--check-location-predicates-and-log (description predicates
                                                     location
                                                     &optional extra-predicates)
  (let ((failed-predicate
         (if (not (nav-stack-location-buffer location))
             'marker-buffer
           (or (nav-stack--check-predicates predicates location)
               (nav-stack--check-predicates extra-predicates location)))))
    (if failed-predicate
        (nav-stack-debug "%s %S rejected by %S"
                         description location failed-predicate)
      (nav-stack-debug "%s %S accepted by all predicates" description location))
    (not failed-predicate)))

(defun nav-stack--check-move-predicates (location &optional extra-predicates)
  (nav-stack--check-location-predicates-and-log "location"
                                                nav-stack-move-predicates
                                                location extra-predicates))

(defun nav-stack-compact-all ()
  (interactive)
  (unless nav-stack-quiet
    (message "Compacting stacks..."))
  (let* ((num-removed 0))
    (nav-stack--map-scopes
     (lambda (frame-or-window)
       (cl-incf num-removed
                (bilist-delete-if (nav-stack--bilist frame-or-window)
                                  (lambda (location)
                                    (not
                                     (nav-stack-location-buffer location)))))))
    (unless nav-stack-quiet
      (message "Compacting stacks... done, removed %d item%s."
               num-removed (if (= num-removed 1) "" "s")))))

(defvar nav-stack--compaction-timer nil)

(defun nav-stack-update-compaction-timer ()
  (interactive)
  (when nav-stack--compaction-timer
    (cancel-timer nav-stack--compaction-timer)
    (setq nav-stack--compaction-timer nil))
  (when (and nav-stack-mode nav-stack-compaction-frequency)
    (setq nav-stack--compaction-timer
          (run-at-time nav-stack-compaction-frequency
                       nav-stack-compaction-frequency
                       #'nav-stack-compact-all))))

(defun nav-stack--custom-set-compaction-frequency (var value)
  (set var value)
  (nav-stack-update-compaction-timer))

(defcustom nav-stack-compaction-frequency 600
  "Minimum number of seconds between idle stack compaction.
Set to nil to disable automatic stack compaction."
  :type 'integer
  :set 'nav-stack--custom-set-compaction-frequency)

(defcustom nav-stack-push-policy 'insert-and-move
  "Where new locations are pushed onto the stack by `nav-stack-push'."
  :type '(choice
          (const :tag "Insert after the pointer, but don't move it"
           insert)
          (const :tag "Insert after the pointer and advance pointer"
           insert-and-move)
          (const :tag "Append to the end, don't move the pointer" append)
          (const :tag "Append to the end and move the pointer to the end"
           append-and-move)
          (const :tag "Truncate after the pointer, append, move pointer"
           truncate-and-move)))

(defun nav-stack--last-pushed-location (&optional frame-or-window)
  (let ((bilist (nav-stack--bilist frame-or-window)))
    (unless (bilist-empty-p bilist)
      (cl-ecase nav-stack-push-policy
        ((insert insert-and-move truncate-and-move)
         (bilist-ref bilist 0 t nil nil))
        ((append append-and-move)
         (bilist-ref bilist -1 nil nil nil)))))  )

;;;XXX questionable
(defun nav-stack-get-pointer (&optional frame-or-window)
  (bilist-ptr-idx (nav-stack--bilist frame-or-window)))

(defun nav-stack-push (&optional location frame-or-window log-info)
  (let ((location (or location (nav-stack-make-location frame-or-window)))
        (bilist (nav-stack--bilist frame-or-window))
        (last-location (nav-stack--last-pushed-location frame-or-window))
        idx from-ptr move-ptr)
    (if (equal (nav-stack-location-marker location)
               (nav-stack-location-marker last-location))
        (nav-stack-debug "push skipping duplicate location %S" location)
      (cl-ecase nav-stack-push-policy
        ((insert insert-and-move)
         (setq idx (if (bilist-ptr-at-end-p bilist) 0 1)
               from-ptr t
               move-ptr (eq nav-stack-push-policy 'insert-and-move)))
        ((append append-and-move)
         (setq idx t move-ptr (eq nav-stack-push-policy 'append-and-move)))
        (truncate-and-move
         (bilist-delete-range bilist 1 nil t)
         (setq idx t move-ptr t)))
      (setf (nav-stack-location-property location 'push-log) log-info)
      (bilist-insert bilist idx location from-ptr move-ptr)
      (nav-stack-debug "push %S now at %S/%S"
                       location
                       (bilist-ptr-idx bilist)
                       (bilist-len bilist)))))

(defun nav-stack-get-stack-position-message (&optional frame-or-window)
  (let ((bilist (nav-stack--bilist frame-or-window)))
    (format "stack position %d of %d"
            (bilist-ptr-idx bilist) (bilist-len bilist))))

(defun nav-stack-set-pointer (index &optional frame-or-window)
  (interactive (list (read-number "Enter new position: "
                                  (bilist-ptr-idx (nav-stack--bilist)))))
  (bilist-set-ptr-idx (nav-stack--bilist frame-or-window) index))

(defun nav-stack-get-location-at-index (index &optional frame-or-window)
  (bilist-ref (nav-stack--bilist frame-or-window) index nil nil nil))

(defun nav-stack-get-location-at-pointer (&optional offset frame-or-window)
  (bilist-ref (nav-stack--bilist frame-or-window) offset t nil nil))

(cl-defun nav-stack-get-next-move-locations (forward &optional
                                                       (num-locations 1)
                                                       frame-or-window
                                                       extra-predicates)
  (let* ((step (if forward 1 -1))
         (direction (if forward 'forward 'back))
         (bilist (nav-stack--bilist frame-or-window))
         (ptr-idx (bilist-ptr-idx bilist))
         (len (bilist-len bilist)))
    (cl-flet ((get-next-location ()
                (let (location
                      result)
                  (while (and (null result) (>= ptr-idx 0) (< ptr-idx len))
                    (setq location (bilist-ref bilist ptr-idx))
                    (when (nav-stack--check-move-predicates location
                                                            extra-predicates)
                      (setq result (cons ptr-idx location)))
                    (setq ptr-idx (+ ptr-idx step)))
                  result)))
      (when (> num-locations 0)
        (let ((first-location (get-next-location))
              locations
              locations-last)
          (when first-location
            (with-repeated-move direction
              (cl-loop
                 initially
                   (setq locations (list first-location)
                         locations-last locations)
                 repeat (1- num-locations)
                 for location = (get-next-location)
                 while location
                 do (setq locations-last
                          (setcdr locations-last (list location)))
                 finally return locations))))))))

(defun nav-stack-find-location-index (location &optional
                                                 last-seen-index
                                                 frame-or-window)
  (let* ((bilist (nav-stack--bilist frame-or-window)))
    (if (eq (and last-seen-index
                 (bilist-ref bilist last-seen-index nil nil nil))
            location)
        last-seen-index
      (catch 'found
        (nav-stack-debug "find-location-index search forward from %S"
                         last-seen-index)
        (bilist-do (candidate bilist :idx-var cand-index :idx last-seen-index)
          (when (eq candidate location)
            (throw 'found cand-index)))
        (nav-stack-debug "find-location-index search backward")
        (bilist-do (candidate
                    bilist
                    :idx-var cand-index
                    :idx last-seen-index
                    :backward t)
          (when (eq candidate location)
            (throw 'found cand-index)))
        (nav-stack-debug "find-location-index search failed")
        nil))))

(defun nav-stack--go-direction (forward
                                &optional
                                  frame-or-window extra-predicates
                                  no-move-on-err)
  (let ((direction (if forward 'forward 'back)))
    (setq nav-stack--this-command-move-direction direction)
    (pcase-let* ((bilist (nav-stack--bilist frame-or-window))
                 (`((,ptr-idx . ,location))
                  (nav-stack-get-next-move-locations forward 1 frame-or-window
                                                     extra-predicates)))
      (when (or location (not no-move-on-err))
        (bilist-set-ptr-idx bilist (or ptr-idx (if forward t 0))))
      (unless location
        (user-error "Can't go %s any further in history" direction))
      (nav-stack-go-to-location location)
      (unless nav-stack-quiet
        (message "Moved %s in history, now at %s"
                 direction
                 (nav-stack-get-stack-position-message frame-or-window))))))

(defun nav-stack-go-back (&optional frame-or-window extra-predicates)
  (interactive)
  (nav-stack--go-direction nil frame-or-window extra-predicates))

(defun nav-stack-go-forward (&optional frame-or-window extra-predicates)
  (interactive)
  (nav-stack--go-direction t frame-or-window extra-predicates))

(defun nav-stack-go-back-same-buffer (&optional frame-or-window)
  (interactive)
  (nav-stack--go-direction nil frame-or-window
                           '(nav-stack-same-buffer-predicate)))

(defun nav-stack-go-forward-same-buffer (&optional frame-or-window)
  (interactive)
  (nav-stack--go-direction t frame-or-window
                           '(nav-stack-same-buffer-predicate)))

(defun nav-stack-go-back-other-buffer (&optional frame-or-window)
  (interactive)
  (nav-stack--go-direction nil frame-or-window
                           '(nav-stack-other-buffer-predicate)))

(defun nav-stack-go-forward-other-buffer (&optional frame-or-window)
  (interactive)
  (nav-stack--go-direction t frame-or-window
                           '(nav-stack-other-buffer-predicate)))

(defun nav-stack--read-buffer ()
  (read-buffer "Buffer: "))

(defun nav-stack--go-direction-specific-buffer (buffer-or-name forward
                                                &optional frame-or-window)
  (let* ((buffer (get-buffer buffer-or-name))
         (pred (apply-partially #'nav-stack-specific-buffer-predicate buffer)))
    (unless buffer
      (user-error "No such buffer %S" buffer-or-name))
    (nav-stack--go-direction forward frame-or-window (list pred) t)))

(defun nav-stack-go-back-specific-buffer (buffer-or-name
                                          &optional frame-or-window)
  (interactive (list (nav-stack--read-buffer)))
  (nav-stack--go-direction-specific-buffer buffer-or-name nil
                                           frame-or-window))

(defun nav-stack-go-forward-specific-buffer (buffer-or-name
                                             &optional frame-or-window)
  (interactive (list (nav-stack--read-buffer)))
  (nav-stack--go-direction-specific-buffer buffer-or-name t
                                           frame-or-window))

(defun nav-stack-go-back-overload (&optional arg frame-or-window)
  (interactive "P")
  (pcase arg
    ('(4) (nav-stack-go-back-other-buffer frame-or-window))
    ('(16) (nav-stack-go-back-specific-buffer (nav-stack--read-buffer)
                                              frame-or-window))
    ('- (nav-stack-go-back-same-buffer frame-or-window))
    (_ (nav-stack-go-back frame-or-window))))

(defun nav-stack-go-forward-overload (&optional arg frame-or-window)
  (interactive "P")
  (pcase arg
    ('(4) (nav-stack-go-forward-other-buffer frame-or-window))
    ('(16) (nav-stack-go-forward-specific-buffer (nav-stack--read-buffer)
                                                 frame-or-window))
    ('- (nav-stack-go-forward-same-buffer frame-or-window))
    (_ (nav-stack-go-forward frame-or-window))))

;;XXX questionable
(defun nav-stack-get-navigable-locations (&optional frame-or-window
                                            extra-predicates unique)
  "Return a list of all navigable locations on the stack.
A navigable location is one that has a valid marker and passes
all functions in `nav-stack-move-predicates'.  If
EXTRA-PREDICATES is given then a location must pass each of those
predicate functions as well.

If UNIQUE is non-nil then only unique locations are returned.

The locations are from newest to oldest (AKA, highest index down
to index 0).

Each value in the returned list is (INDEX . LOCATION).  The index
is suitable for passing to `nav-stack-go-to-location-at-index' as
its INDEX argument, which see."
  (let* ((bilist (nav-stack--bilist frame-or-window))
         (locations (cons nil nil))
         (last locations)
         (seen (when unique (make-hash-table :test 'nav-stack--marker-equal))))
    ;; First assemble locations from pointer back to oldest.
    (bilist-do (location
                bilist
                :from-ptr t
                :idx 0
                :backward t
                :idx-var index)
      (when (and (nav-stack--check-move-predicates location extra-predicates)
                 (or (not unique)
                     (let ((marker (nav-stack-location-marker location)))
                       (if (gethash marker seen)
                           ;; Sometimes I want to throw a debug
                           ;; statement here, hence funny-looking
                           ;; (if).
                           nil
                         (puthash marker t seen)))))
        ;; Appending locations.
        (setq last (setcdr last (cons (cons index location) nil)))))
    ;; Now add locations after the pointer.
    (setq locations (cdr locations))
    (unless (bilist-ptr-at-end-p bilist)
      (bilist-do (location
                  bilist
                  :from-ptr t
                  :idx 1
                  :idx-var index)
        (when (and (nav-stack--check-move-predicates location extra-predicates)
                   (or (not unique)
                       (let ((marker (nav-stack-location-marker location)))
                         (if (gethash marker seen)
                             ;; Sometimes I want to throw a debug
                             ;; statement here, hence funny-looking
                             ;; (if).
                             nil
                           (puthash marker t seen)))))
          ;; Adding locations on the front.
          (push (cons index location) locations))))
    locations))

(defun nav-stack-kill-buffer-and-window (&optional window)
  (interactive)
  (quit-restore-window window 'kill))

(defun nav-stack--list-revert (_ignore-auto _noconfirm)
  (nav-stack-list-refresh))

(defun nav-stack-list-refresh (&optional list-buffer frame-or-window)
  (interactive)
  (with-current-buffer (or list-buffer (current-buffer))
    (let* ((bilist (nav-stack--bilist frame-or-window))
           (ptr-idx (bilist-ptr-idx bilist))
           ptr-line-pos
           (inhibit-read-only t))
      (setq-local header-line-format
                  (list
                   (format "num-items=%d capacity=%d pointer=%S at %s"
                           (bilist-len bilist)
                           (bilist-max-len bilist)
                           (bilist-ptr-idx bilist)
                           (format-time-string "%R:%S"))
                   '(nav-stack-list--auto-update-timer " [auto-updating]")))
      (delete-region (point-min) (point-max))
      ;; (insert (format "num-items=%d capacity=%d pointer=%S\n\n"
      ;;                 (bilist-len bilist)
      ;;                 (bilist-max-len bilist)
      ;;                 (bilist-ptr-idx bilist)))
      (bilist-do (location bilist :idx-var idx :backward t)
        (let* ((marker (nav-stack-location-marker location))
               (window (nav-stack-location-property location 'window))
               (buf (marker-buffer marker))
               (is-live (buffer-live-p buf))
               (is-ptr (= idx ptr-idx))
               (line (format "%s%4d %s %s: %s\n"
                             (if is-ptr ">" " ")
                             idx
                             (if (window-live-p window)
                                 "     "
                               "nowin")
                             (if is-live
                                 (truncate-string-to-width (buffer-name buf) 40)
                               "<dead buffer>")
                             (if is-live
                                 (with-current-buffer buf
                                   (save-excursion
                                     (goto-char marker)
                                     (buffer-substring (line-beginning-position)
                                                       (line-end-position))))
                               ""))))
          (when is-live
            (setq line (propertize line
                                   'nav-stack-location location
                                   'nav-stack-index idx)))
          (when is-ptr
            (setq ptr-line-pos (point)))
          (insert line)))
      (if (not ptr-line-pos)
          (goto-char (point-min))
        (let ((window (get-buffer-window nil 'visible)))
          (when window
            (with-selected-window window
              (goto-char ptr-line-pos)
              (recenter))))))))

;; XXX I suspect this doesn't scope properly.  It'll probably end up
;; operating on whatever frame/window it's on.
(cl-defun nav-stack-list (&optional (buffer-name "*nav-stack*") frame-or-window)
  (interactive)
  (pop-to-buffer (get-buffer-create buffer-name))
  (nav-stack-list-mode)
  (nav-stack-list-refresh nil frame-or-window))

(defun nav-stack-list-next-location (&optional n)
  (interactive "p")
  (let ((step (if (< n 0) -1 1))
        (last-location (point)))
    (setq n (abs n))
    (while (> n 0)
      (unless (zerop (forward-line step))
        (goto-char last-location)
        (user-error "No more locations"))
      (when (get-text-property (point) 'nav-stack-location)
        (setq last-location (point))
        (cl-decf n)))))

(defun nav-stack-list-previous-location (&optional n)
  (interactive "p")
  (nav-stack-list-next-location (- n)))

(defun nav-stack-list-go-to-location-at-point (&optional frame-or-window)
  (interactive)
  (let ((location (get-text-property (point) 'nav-stack-location))
        (index (get-text-property (point) 'nav-stack-index)))
    (unless (and location index)
      (user-error "No Nav-Stack location at point"))
    (setq index (nav-stack-find-location-index location index frame-or-window))
    (nav-stack-kill-buffer-and-window)
    (if index
        (nav-stack-go-to-location-at-index index)
      (nav-stack-go-to-location location))
    (unless nav-stack-quiet
      (message "Moved to location, now at %s"
               (nav-stack-get-stack-position-message)))))

(defvar nav-stack-list--auto-update-timer nil)

(make-variable-buffer-local 'nav-stack-list--auto-update-timer)

;; This may not be necessary, but it probably is.
(put 'nav-stack-list--auto-update-timer 'permanent-local t)

(defcustom nav-stack-list-auto-update-interval 1
  "Number of seconds between auto-updates."
  :type 'integer)

;; XXX Should this function pay attention to the buffer it's being
;; used in?  Particularly the call to `nav-stack-list-refresh'.
(defun nav-stack-list-set-auto-update (&optional arg quiet)
  (interactive (list (or current-prefix-arg 'toggle)))
  (let ((timer-was-set nav-stack-list--auto-update-timer))
    (when nav-stack-list--auto-update-timer
      (cancel-timer nav-stack-list--auto-update-timer)
      (setq nav-stack-list--auto-update-timer nil))
    (if (if (eq arg 'toggle)
            timer-was-set
          (<= (prefix-numeric-value arg) 0))
        (unless quiet
          (message "Auto-updating turned off"))
      (nav-stack-list-refresh)
      (setq nav-stack-list--auto-update-timer
            (run-at-time nav-stack-list-auto-update-interval
                         nav-stack-list-auto-update-interval
                         #'nav-stack-list-refresh (current-buffer)))
      (unless quiet
        (message "Auto-updating this buffer every %d second(s)"
                 nav-stack-list-auto-update-interval))))
  ;; Update the header line.
  (force-mode-line-update))

(defun nav-stack-list--stop-auto-update ()
  (nav-stack-list-set-auto-update -1 t))

(defun nav-stack-list-compact-all ()
  (interactive)
  (nav-stack-compact-all)
  (nav-stack-list-refresh))

(defvar nav-stack-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "q") 'nav-stack-kill-buffer-and-window)
    (define-key map (kbd "g") 'nav-stack-list-refresh)
    (define-key map (kbd "n") 'nav-stack-list-next-location)
    (define-key map (kbd "p") 'nav-stack-list-previous-location)
    (define-key map (kbd "<down>") 'nav-stack-list-next-location)
    (define-key map (kbd "<up>") 'nav-stack-list-previous-location)
    (define-key map (kbd "RET") 'nav-stack-list-go-to-location-at-point)
    (define-key map (kbd "a") 'nav-stack-list-set-auto-update)
    (define-key map (kbd "c") 'nav-stack-list-compact-all)
    map)
  "Keymap for `nav-stack-list-mode'.")

(define-derived-mode nav-stack-list-mode special-mode "Nav-Stack"
  "Display mode for `nav-stack-list'."
  (setq-local revert-buffer-function #'nav-stack--list-revert)
  (setq-local truncate-lines t)
  (add-hook 'kill-buffer-hook #'nav-stack-list--stop-auto-update nil t)
  (buffer-disable-undo))

(defcustom nav-stack-view-log-show-dead-markers nil
  "If non-nil, `nav-stack-view-log' will show dead markers from the log.
This is nil by default since dead markers are usually not very
interesting."
  :type 'boolean)

(defun nav-stack-view-push-log-refresh (&optional _frame-or-window)
  (interactive)
  (let ((inhibit-read-only t)
        (bilist (nav-stack--bilist))
        (dead-markers 0))
    (delete-region (point-min) (point-max))
    (insert "Stack push log\n\n")
    (insert (format " %-40s  Location\n" "Source"))
    (insert (make-string 80 ?=))
    (insert "\n")
    (nav-stack-debug "v-p-l begin stack=%d/%d ptr=%d"
                     (bilist-len bilist) (bilist-max-len bilist)
                     (bilist-ptr-idx bilist))
    (bilist-do (location bilist :backward t)
      (pcase-let* ((marker (nav-stack-location-marker location))
                   (buf (marker-buffer marker))
                   (log-info (nav-stack-location-property location 'push-log))
                   (marker-info
                    (if (not buf)
                        "<dead marker>"
                      (with-current-buffer buf
                        (save-excursion
                          (goto-char marker)
                          (format "%s:%d: %s"
                                  (buffer-name)
                                  (line-number-at-pos)
                                  (buffer-substring (line-beginning-position)
                                                    (line-end-position))))))))
        (if (or buf nav-stack-view-log-show-dead-markers)
            (insert (format " %s  %s\n"
                            (truncate-string-to-width (format "%s" log-info)
                                                      40 0 ?\s t)
                            marker-info))
          (cl-incf dead-markers))))
    (forward-line (- dead-markers (bilist-ptr-idx bilist) 1))
    (if (= (char-after) ?=)
        (forward-line 1)
      (insert ">")
      (delete-char 1))))

(defun nav-stack--view-push-log-revert (_ignore-auto _noconfirm)
  (nav-stack-view-push-log-refresh))

(defvar nav-stack-view-push-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "q") 'nav-stack-kill-buffer-and-window)
    (define-key map (kbd "g") 'nav-stack-view-push-log-refresh)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    map)
  "Keymap for `nav-stack-view-push-log-mode'.")

(define-derived-mode nav-stack-view-push-log-mode special-mode "Nav-Stack"
  "Display mode for `nav-stack-view-push-log'."
  (setq-local revert-buffer-function #'nav-stack--view-push-log-revert)
  (setq-local truncate-lines t)
  (buffer-disable-undo))

(cl-defun nav-stack-view-push-log (&optional
                                     (buffer-name "*nav-stack-push-log*")
                                     frame-or-window)
  (interactive)
  (pop-to-buffer (get-buffer-create buffer-name))
  (nav-stack-view-push-log-mode)
  (nav-stack-view-push-log-refresh frame-or-window))

(defun nav-stack--push-mark-advice (&rest _args)
  (let ((marker (copy-marker (mark-marker))))
    (nav-stack-debug "push from push-mark: %S" marker)
    (nav-stack-push (nav-stack-make-location nil marker) nil 'push-mark)))

;; This use of `post-command-hook' is inspired by "Emacs-lisp hooks
;; for detecting change of active buffer?"
;; https://stackoverflow.com/q/47456134

(defcustom nav-stack-significant-movement-threshold 2000
  "Number of characters that point must move for a \"significant\" movement.
If nil, merely moving point will never be enough for a movement
to be significant."
  :type 'integer)

(defun nav-stack-significant-post-command-movement (location)
  (or
   ;; A nav-stack move should always be considered "significant".  Of
   ;; course, repeated moves shouldn't be: see
   ;; `nav-stack-not-repeated-move-predicate', which should be in
   ;; `nav-stack-post-command-push-predicates' *prior to this
   ;; predicate function*.
   (nav-stack-this-command-is-move-p)
   (let ((location-win (nav-stack-location-property location 'window)))
     (nav-stack-debug "significant movement cmd=%S old=%S new=%S"
                      this-command
                      location
                      (nav-stack-make-location))
     (or (not (eq location-win (selected-window)))
         (let* ((marker (nav-stack-location-marker location))
                (old-buf (marker-buffer marker))
                (cur-buf (current-buffer)))
           (or (not (eq old-buf cur-buf))
               (and nav-stack-significant-movement-threshold
                    (let ((pos (marker-position marker)))
                      (and pos
                           (> (abs (- pos (point)))
                              nav-stack-significant-movement-threshold))))))))))

(defcustom nav-stack-post-command-excluded-minor-modes
  '(
    isearch-mode
    edebug-mode
    bs-mode
    )
  "Do not implicitly push locations in buffers with any of these modes enabled."
  :type '(repeat variable))

(defcustom nav-stack-post-command-excluded-major-modes
  '(
    bs-mode
    )
  "Do not implicitly push locations in buffers in any of these major modes."
  ;; Is function right, or should it just be symbol?
  :type '(repeat function))

(defun nav-stack-exclude-certain-modes (location)
  (with-current-buffer (nav-stack-location-buffer location)
    (and (not (memq major-mode nav-stack-post-command-excluded-major-modes))
         (not (seq-some (lambda (var) (and (boundp var) (symbol-value var)))
                        nav-stack-post-command-excluded-minor-modes)))))

(defun nav-stack-not-repeated-move-predicate (_location)
  (not (nav-stack-repeated-move-p)))

(defcustom nav-stack-post-command-push-predicates
  '(
    nav-stack-not-repeated-move-predicate
    nav-stack-exclude-certain-modes
    nav-stack-significant-post-command-movement
    )
  "List of functions consulted before recording a location after a command.
After each command (see `post-command-hook'), nav-stack will call
each function in this list with two arguments, WINDOW and MARKER,
representing a proposed location to be pushed onto the stack.  If
all functions in this list return non-nil, or if the list is
empty, the location will be pushed.  If any function returns nil,
no further functions are called and the location is discarded."
  :type '(repeat function))

(defvar nav-stack--pre-command-location nil)

(defun nav-stack--pre-command-hook ()
  (with-demoted-errors
      "nav-stack pre-command hook error: %S"
    ;; We could be working in the minibuffer, such as with Ivy.  Wait
    ;; for us to be done there.
    (unless (minibufferp)
      (setq nav-stack--pre-command-location (nav-stack-make-location))
      (setq nav-stack--last-command-move-direction
            nav-stack--this-command-move-direction)
      (setq nav-stack--this-command-move-direction nil))))

(defun nav-stack--check-post-command-push-predicates ()
  (let ((failed-predicate
         (nav-stack--check-predicates nav-stack-post-command-push-predicates
                                      nav-stack--pre-command-location)))
    (when failed-predicate
      (nav-stack-debug "post-command push rejected by %S" failed-predicate))
    (not failed-predicate)))

(defun nav-stack--post-command-hook ()
  (with-demoted-errors
      "nav-stack post-command hook error: %S"
    ;; We could be working in the minibuffer, such as with Ivy.  Wait
    ;; for us to be done there.
    (unless (minibufferp)
      (when nav-stack--pre-command-location
        (unwind-protect
             (when (and
                    (nav-stack-location-buffer nav-stack--pre-command-location)
                    (nav-stack--check-post-command-push-predicates))
               (nav-stack-debug "post-command-hook push %S for %S in %S"
                                nav-stack--pre-command-location
                                this-command
                                (current-buffer))
               (let ((window (car nav-stack--pre-command-location)))
                 (unless (window-live-p window)
                   (setq window nil))
                 (nav-stack-push nav-stack--pre-command-location
                                 nil
                                 (cons 'post-cmd this-command))))
          ;; Probably not real important, just good housekeeping.
          (setq nav-stack--pre-command-location nil))))))

(declare-function 'nav-stack-ivy "nav-stack-ivy")
(declare-function 'nav-stack-hydra/body "nav-stack-ivy")

(defvar nav-stack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-,") 'nav-stack-go-back-overload)
    (define-key map (kbd "C-M-<") 'nav-stack-go-forward-overload)
    (define-key map (kbd "s-,") 'nav-stack-ivy)
    (define-key map (kbd "C-:") 'nav-stack-hydra/body)
    map))

;;;###autoload
(define-minor-mode nav-stack-mode
    "Navigate back and forth through your frame/window/point history."
  :global t
  :lighter " NavStk"
  :keymap nav-stack-mode-map
  (if nav-stack-mode
      (progn
        (setq nav-stack--scope-property-containers
              (make-hash-table :weakness 'key))
        (nav-stack-reset-all-stacks)
        (advice-add 'push-mark :after #'nav-stack--push-mark-advice)
        (add-hook 'pre-command-hook #'nav-stack--pre-command-hook)
        (add-hook 'post-command-hook
                  #'nav-stack--post-command-hook))
    (advice-remove 'push-mark #'nav-stack--push-mark-advice)
    (remove-hook 'pre-command-hook #'nav-stack--pre-command-hook)
    (remove-hook 'post-command-hook
                 #'nav-stack--post-command-hook))
  (nav-stack-update-compaction-timer))

(provide 'nav-stack)
;;; nav-stack.el ends here
