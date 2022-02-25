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

(defcustom nav-stack-max-length 2000
  "Maximum length of a nav stack."
  :type 'integer)

(defcustom nav-stack-significant-movement-threshold 2000
  "Number of characters movement that constitutes a \"significant\" movement."
  :type 'integer)

(defcustom nav-stack-may-widen t
  "If non-nil, nav stack pop operations may call `widen' if necessary."
  :type 'boolean)

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

Function will be called with three arguments: the window being
popped to (may be nil, meaning current window), the buffer being
popped to, and the position within that buffer to pop to."
  :type 'function)

(defcustom nav-stack-post-pop-hook nil
  "Functions called after popping the current location from a nav stack."
  :type 'hook)

(defvar nav-stack-no-auto-push nil)

(defvar nav-stack--global-stack (list 0))
(defvar-local nav-stack--buffer-stack nil)

(defun nav-stack-push (&optional force)
  (interactive (list t))
  (let* ((win (selected-window))
         (buf (current-buffer))
         (pos (point))
         (window-stack (or (window-parameter win 'nav-stack--stack)
                           (set-window-parameter win 'nav-stack--stack (list 0))))
         (buffer-stack (or nav-stack--buffer-stack
                           (setq-local nav-stack--buffer-stack (list 0)))))
    (cl-destructuring-bind (&optional top-win top-buf . top-pos)
        (cadr nav-stack--global-stack)
      (unless (or force
                  (and (eq win top-win)
                       (eq buf top-buf)
                       (or (null top-pos)
                           (< (abs (- pos top-pos))
                              nav-stack-significant-movement-threshold))))
        (push (cl-list* win buf pos) (cdr nav-stack--global-stack))
        (cl-incf (car nav-stack--global-stack))))
    (cl-destructuring-bind (&optional top-buf . top-pos)
        (cadr window-stack)
      (unless (or force
                  (and (eq buf top-buf)
                       (or (null top-pos)
                           (< (abs (- pos top-pos))
                              nav-stack-significant-movement-threshold))))
        (push (cl-list* buf pos) (cdr window-stack))
        (cl-incf (car window-stack))))
    (let ((top-pos (cadr buffer-stack)))
      (unless (or force
                  (null top-pos)
                  (< (abs (- pos top-pos))
                     nav-stack-significant-movement-threshold))
        (push pos (cdr buffer-stack))
        (cl-incf (car buffer-stack))))))

(defun nav-stack--post-command-hook ()
  (cond
    (nav-stack-no-auto-push
     (setq nav-stack-no-auto-push nil))
    ((and nav-stack-auto-push-predicate
          (not (funcall nav-stack-auto-push-predicate)))
     ;; Do nothing, predicate inhibited auto-push
     )
    ((not (minibufferp (current-buffer)))
     (nav-stack-push))))

(defun nav-stack--can-go-to (win buf pos)
  (and (or (not nav-stack-global-pop-only-to-same-window)
           (null win)
           (window-live-p win))
       (buffer-live-p buf)
       (with-current-buffer buf
         (and (or nav-stack-may-widen (>= pos (point-min)))
              (<= pos (if nav-stack-may-widen
                          (buffer-size)
                        (point-max)))))
       (or (null nav-stack-pop-predicate)
           (funcall nav-stack-pop-predicate win buf pos))))

(defun nav-stack--trim-stack (stack)
  (let ((ptr (cdr stack))
        last)
    (cl-loop
      repeat nav-stack-max-length
      do (setq last ptr
               ptr (cdr ptr)))
    (setf (cdr last) nil
          (car stack) nav-stack-max-length)))

(defun nav-stack--pop-internal (win buf pos ignore-stack)
  (when (window-live-p win)
    (select-window win))
  (switch-to-buffer buf nil (not (null win)))
  (when (or (<= pos (point-min))
            (>= pos (point-max)))
    (widen))
  (goto-char pos)
  (when (and (not (eq ignore-stack 'global))
             (cl-destructuring-bind (&optional top-win top-buf . top-pos)
                 (cdr nav-stack--global-stack)
               (and (eq top-win (selected-window))
                    (eq top-buf buf)
                    (eql top-pos pos))))
    (pop (cdr nav-stack--global-stack))
    (cl-decf (car nav-stack--global-stack)))
  (when (> (car nav-stack--global-stack) nav-stack-max-length)
    (nav-stack--trim-stack nav-stack--global-stack))
  (let ((window-stack (window-parameter nil 'nav-stack--stack)))
    (when (and (not (eq ignore-stack 'window))
               (cl-destructuring-bind (&optional top-buf . top-pos)
                   (cdr window-stack)
                 (and (eq top-buf buf)
                      (eql top-pos pos))))
      (pop (cdr window-stack))
      (cl-decf (car window-stack)))
    (when (and window-stack
               (> (car window-stack) nav-stack-max-length))
      (nav-stack--trim-stack window-stack)))
  (when (and (not (eq ignore-stack 'buffer))
             (eql (cadr nav-stack--buffer-stack) pos))
    (pop (cdr nav-stack--buffer-stack))
    (cl-decf (car nav-stack--buffer-stack)))
  (when (and nav-stack--buffer-stack
             (> (car nav-stack--buffer-stack) nav-stack-max-length))
    (nav-stack--trim-stack nav-stack--buffer-stack))
  (run-hooks 'nav-stack-post-pop-hook))

(defun nav-stack-pop-global-stack ()
  (interactive)
  (setq nav-stack-no-auto-push t)
  (catch 'done
    (while (> (car nav-stack--global-stack) 0)
      (cl-destructuring-bind (win buf . pos)
          (pop (cdr nav-stack--global-stack))
        (cl-decf (car nav-stack--global-stack))
        (when (nav-stack--can-go-to win buf pos)
          (nav-stack--pop-internal win buf pos 'global)
          (throw 'done nil))))
    (user-error "Global nav stack is empty")))

(defun nav-stack-pop-window-stack ()
  (interactive)
  (setq nav-stack-no-auto-push t)
  (let* ((window-stack (window-parameter nil 'nav-stack--stack)))
    (catch 'done
      (when window-stack
        (while (> (car window-stack) 0)
          (cl-destructuring-bind (buf . pos)
              (pop (cdr window-stack))
            (cl-decf (car window-stack))
            (when (nav-stack--can-go-to nil buf pos)
              (nav-stack--pop-internal nil buf pos 'window)
              (throw 'done nil)))))
      (user-error "Window nav stack is empty"))))

(defun nav-stack-pop-buffer-stack ()
  (interactive)
  (setq nav-stack-no-auto-push t)
  (catch 'done
    (when nav-stack--buffer-stack
      (let ((buf (current-buffer)))
        (while (> (car nav-stack--buffer-stack) 0)
          (let ((pos (pop (cdr nav-stack--buffer-stack))))
            (cl-decf (car nav-stack--buffer-stack))
            (when (nav-stack--can-go-to nil buf pos)
              (nav-stack--pop-internal nil buf pos 'buffer)
              (throw 'done nil))))))
    (user-error "Buffer nav stack is empty")))

(defun nav-stack-clear-all-stacks ()
  (interactive)
  (setq nav-stack--global-stack (list 0))
  (dolist (frame (frame-list))
    (dolist (win (window-list frame))
      (set-window-parameter win 'nav-stack--stack (list 0))))
  (dolist (buf (buffer-list))
    (setf (buffer-local-value 'nav-stack--buffer-stack buf) (list 0))))

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
  (if nav-stack-mode
      (add-hook 'post-command-hook #'nav-stack--post-command-hook)
    (remove-hook 'post-command-hook #'nav-stack--post-command-hook)))

(provide 'nav-stack)
;;;; nav-stack.el ends here
