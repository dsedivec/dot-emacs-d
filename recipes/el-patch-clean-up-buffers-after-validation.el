;; -*- lexical-binding: t; -*-

;; `el-patch-validate' can/will open buffers and leave them open,
;; which is irritating.  Kill buffers opened by `el-patch-validate'.

(defvar my:el-patch-validate--buffers-opened)

(defvar my:el-patch-validate--depth 0)

(defun my:el-patch-validate--record-new-buffer ()
  (push (current-buffer) my:el-patch-validate--buffers-opened))

(define-advice el-patch-validate
    (:around (orig-fun &rest args) my:dont-leave-buffers-open)
  ;; I don't actually expect to re-enter `el-patch-validate', but why
  ;; risk it?
  (let ((my:el-patch-validate--depth
         (if (>= my:el-patch-validate--depth 0)
             (1+ my:el-patch-validate--depth)
           (warn "`el-patch-validate--depth has illegal value %S"
                 my:el-patch-validate--depth)
           0))
        my:el-patch-validate--buffers-opened)
    (unwind-protect
         (progn
           ;; AFAIK `find-file-hook' is only called when a new buffer
           ;; is created, not when an existing buffer is used.  BTW
           ;; the only reason I know of that `el-patch-validate' will
           ;; ever open a *file* (not just a buffer) is to read the
           ;; source.
           (add-hook 'find-file-hook #'my:el-patch-validate--record-new-buffer)
           (apply orig-fun args))
      (when (<= my:el-patch-validate--depth 1)
        (remove-hook 'find-file-hook
                     #'my:el-patch-validate--record-new-buffer))
      (dolist (buf my:el-patch-validate--buffers-opened)
        (when (and (buffer-live-p buf)
                   (not (buffer-modified-p buf)))
          (kill-buffer buf))))))
