;;;; nav-stack.el --- Emacs's missing back button   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: convenience, tools

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

;; TBD

;;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'cl-macs))

(defgroup nav-stack nil
  "Navigate back and forth through your point/window/frame history."
  :prefix "nav-stack-"
  :group 'tools)

(defcustom nav-stack-global-size 2000
  "Maximum number of elements in the global nav stack."
  :type 'integer)

(defcustom nav-stack-window-size nav-stack-global-size
  "Maximum number of elements in a window nav stack."
  :type 'integer)

(defcustom nav-stack-buffer-size 100
  "Maximum number of elements in a buffer nav stack."
  :type 'integer)

(defcustom nav-stack-significant-movement-threshold 2000
  "Number of characters movement that constitutes a \"significant\" movement."
  :type 'integer)

(defcustom nav-stack-global-pop-only-to-same-window nil
  "If non-nil, global nav stack items with a deleted window are ignored.
Otherwise, popping a global nav stack item whose window is no
longer live will instead display the buffer in some other window."
  :type 'boolean)

(defcustom nav-stack-auto-push-predicate nil
  "If non-nil, this function must return non-nil if auto-push is OK."
  :type 'function)

(defcustom nav-stack-pop-predicate nil
  "If non-nil, called before popping a location to see if it's OK.
If the predicate returns nil then the location from the stack
will be ignored.  (It has already been removed from the nav stack
at the time this function is called.)

Function will be called with two arguments indicating the
location being popped: the window to be used, and a marker."
  :type 'function)

(defcustom nav-stack-post-pop-hook nil
  "Functions called after popping the current location from a nav stack."
  :type 'hook)

(define-error 'nav-stack--stack-error "Generic nav-stack stack error")
(define-error 'nav-stack--stack-too-far-back "Can't go back that far"
  'nav-stack--stack-error)
(define-error 'nav-stack--stack-too-far-forward "Can't go forward that far"
  'nav-stack--stack-error)

(defun nav-stack--stack-new (length)
  (cl-list* 0 0 0 (make-vector length nil)))

(defun nav-stack--stack-check (stack)
  (cl-destructuring-bind (oldest num-items offset . vec) stack
    (let ((vec-len (length vec)))
      (cl-assert (>= oldest 0))
      (cl-assert (< oldest vec-len))
      (cl-assert (<= 0 num-items vec-len))
      (cl-assert (<= 0 offset num-items)))))

(defun nav-stack--stack-push (stack item)
  (cl-destructuring-bind (oldest num-items offset . vec) stack
    (let ((vec-len (length vec)))
      (when (> offset 0)
        (setf num-items (- num-items offset)
              (nth 2 stack) 0))
      (if (= num-items vec-len)
          (progn
            (aset vec oldest item)
            (setf (car stack) (mod (1+ oldest) vec-len)))
        (aset vec (mod (+ oldest num-items) vec-len) item)
        (setf (cadr stack) (1+ num-items))))))

(defun nav-stack--stack-calc-move (stack n &optional end-sentinel)
  (cl-destructuring-bind (oldest num-items offset . vec) stack
    (setq offset (+ offset (- n)))
    (when (>= offset num-items)
      (signal 'nav-stack--stack-too-far-back (cons num-items n)))
    (when (< offset 0)
      (signal 'nav-stack--stack-too-far-forward (cons num-items n)))
    (cons offset
          (if (zerop offset)
              end-sentinel
            (aref vec
                  (mod (+ oldest (- num-items offset)) (length vec)))))))

(defun nav-stack--stack-move (stack n &optional end-sentinel)
  (cl-destructuring-bind (offset . item)
      (nav-stack--stack-calc-move stack n end-sentinel)
    (setf (nth 2 stack) offset)
    (if (zerop offset)
        end-sentinel
      item)))

(defun nav-stack--stack-peek (stack n &optional no-error sentinel)
  (condition-case err
      (cdr (nav-stack--stack-calc-move stack n))
    ((nav-stack--stack-too-far-back nav-stack--stack-too-far-forward)
     (if no-error
         sentinel
       (signal (car err) (cdr err))))))

(defvar nav-stack-no-auto-push-this-command nil)

(defvar nav-stack--global-stack nil)
(defvar-local nav-stack--buffer-stack nil)

(defun nav-stack-push (&optional force win mark)
  (interactive (list t))
  (let* ((win (or win (selected-window)))
         (mark (or mark (point-marker)))
         (window-stack
          (or (window-parameter win 'nav-stack--stack)
              (set-window-parameter win 'nav-stack--stack
                                    (nav-stack--stack-new nav-stack-window-size))))
         (buffer-stack
          (with-current-buffer (marker-buffer mark)
            (or nav-stack--buffer-stack
                (setq-local nav-stack--buffer-stack
                            (nav-stack--stack-new nav-stack-buffer-size)))))
         pushed)
    (cl-destructuring-bind (&optional top-win . top-mark)
        (nav-stack--stack-peek nav-stack--global-stack -1 :noerror)
      (when (or force
                (not (eq win top-win))
                (not (eq (marker-buffer mark) (marker-buffer top-mark)))
                (>= (abs (- mark top-mark))
                    nav-stack-significant-movement-threshold))
        (nav-stack--stack-push nav-stack--global-stack (cons win mark))
        (setq pushed t)))
    (let ((top-mark (nav-stack--stack-peek window-stack -1 :noerror)))
      (when (or force
                (null top-mark)
                (not (eq (marker-buffer mark) (marker-buffer top-mark)))
                (>= (abs (- mark top-mark))
                    nav-stack-significant-movement-threshold))
        (nav-stack--stack-push window-stack mark)
        (setq pushed t)))
    (let ((top-mark (nav-stack--stack-peek buffer-stack -1 :noerror)))
      (when (or force
                (null top-mark)
                (not (eq (marker-buffer mark) (marker-buffer top-mark)))
                (>= (abs (- mark top-mark))
                    nav-stack-significant-movement-threshold))
        (nav-stack--stack-push buffer-stack mark)
        (setq pushed t)))
    pushed))

(defvar nav-stack--last-win nil)
(defvar nav-stack--last-mark nil)

(defun nav-stack--post-command-hook ()
  (cond
    ;; This condition must be first, so we make sure to clear
    ;; `nav-stack-no-auto-push-this-command' after each command, if
    ;; necessary.
    (nav-stack-no-auto-push-this-command
     (setq nav-stack-no-auto-push-this-command nil))
    ((and nav-stack-auto-push-predicate
          (not (funcall nav-stack-auto-push-predicate)))
     ;; Do nothing, predicate inhibited auto-push
     )
    ((not (minibufferp (current-buffer)))
     ;; Doing a bit of a dance to avoid calling `point-marker' too
     ;; much.  Naïve benchmarking suggests that GC is a *huge* hit
     ;; compared to mutating an existing marker.
     (if (or (nav-stack-push nil
                             nav-stack--last-win
                             nav-stack--last-mark)
             (null nav-stack--last-mark))
         (setq nav-stack--last-mark (point-marker))
       (set-marker nav-stack--last-mark (point) (current-buffer)))
     (setq nav-stack--last-win (selected-window)))))

(defun nav-stack--can-go-to (win mark)
  (and (or nav-stack-global-pop-only-to-same-window
           ;; nil win means we're working off the window stack, so the
           ;; window must be live (right?).
           (null win)
           (window-live-p win))
       (buffer-live-p (marker-buffer mark))
       (or (null nav-stack-pop-predicate)
           (funcall nav-stack-pop-predicate
                    (or win (selected-window)) mark))))

(defun nav-stack--back-internal (win mark ignore-stack)
  (when (window-live-p win)
    (select-window win))
  (switch-to-buffer (marker-buffer mark) nil (not (null win)))
  (goto-char mark)
  (when (and (not (eq ignore-stack 'global))
             (cl-destructuring-bind (&optional top-win . top-mark)
                 (nav-stack--stack-peek nav-stack--global-stack -1 t)
               (and (eq top-win (selected-window))
                    (equal top-mark mark))))
    (nav-stack--stack-move nav-stack--global-stack -1))
  (when (not (eq ignore-stack 'window))
    (let* ((window-stack (window-parameter nil 'nav-stack--stack)))
      (when (equal (and window-stack
                        (nav-stack--stack-peek window-stack -1 t))
                   mark)
        (nav-stack--stack-move window-stack -1))))
  (when (and (not (eq ignore-stack 'buffer))
             nav-stack--buffer-stack
             (equal (nav-stack--stack-peek nav-stack--buffer-stack -1 t)
                    mark))
    (nav-stack--stack-move nav-stack--buffer-stack -1))
  (run-hooks 'nav-stack-post-pop-hook))

(defun nav-stack--try-pop (stack-type stack)
  (let (found pair win mark)
    (condition-case nil
        (while (not found)
          (if (eq stack-type 'global)
              (setq pair (nav-stack--stack-move stack -1)
                    win (car pair)
                    mark(cdr pair))
            (setq mark (nav-stack--stack-move stack -1)))
          (when (nav-stack--can-go-to win mark)
            (nav-stack--back-internal win mark stack-type)
            (setq found t)))
      (nav-stack--stack-too-far-back))
    found))

(defun nav-stack-pop-global-stack ()
  (interactive)
  (setq nav-stack-no-auto-push-this-command t)
  (unless (nav-stack--try-pop 'global nav-stack--global-stack)
    (user-error "Global nav stack is empty")))

(defun nav-stack-pop-window-stack ()
  (interactive)
  (setq nav-stack-no-auto-push-this-command t)
  (let* ((window-stack (window-parameter nil 'nav-stack--stack)))
    (unless (and window-stack
                 (nav-stack--try-pop 'window window-stack))
      (user-error "Window nav stack is empty"))))

(defun nav-stack-pop-buffer-stack ()
  (interactive)
  (setq nav-stack-no-auto-push-this-command t)
  (unless (and nav-stack--buffer-stack
               (nav-stack--try-pop 'buffer nav-stack--buffer-stack))
    (user-error "Buffer nav stack is empty")))

(defun nav-stack-reset ()
  (interactive)
  (setq nav-stack--global-stack (nav-stack--stack-new nav-stack-global-size))
  (dolist (frame (frame-list))
    (dolist (win (window-list frame))
      (set-window-parameter win 'nav-stack--stack nil)))
  (setq-default nav-stack--buffer-stack nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (kill-local-variable 'nav-stack--buffer-stack)))
  (setq nav-stack--last-win nil
        nav-stack--last-mark nil))

(defvar nav-stack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-,") 'nav-stack-pop-window-stack)
    map))

;;;###autoload
(define-minor-mode nav-stack-mode
    "Navigate back through buffer/window/global movement history."
  :global t
  :lighter " NavStk"
  :keymap nav-stack-mode-map
  (nav-stack-reset)
  (if nav-stack-mode
      (add-hook 'post-command-hook #'nav-stack--post-command-hook)
    (remove-hook 'post-command-hook #'nav-stack--post-command-hook)))

(provide 'nav-stack)
;;;; nav-stack.el ends here
