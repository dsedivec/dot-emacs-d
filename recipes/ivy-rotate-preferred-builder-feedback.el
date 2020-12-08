;; -*- lexical-binding: t; -*-

;; `ivy-rotate-preferred-builders' should tell you which matcher
;; you're rotating to!  Make it so.

(require 'el-patch)

(el-patch-feature ivy)

(el-patch-defun ivy-rotate-preferred-builders ()
  "Switch to the next re builder in `ivy-preferred-re-builders'."
  (interactive)
  (when ivy-preferred-re-builders
    (setq ivy--old-re nil)
    (el-patch-let (($cell (assq ivy--regex-function ivy-preferred-re-builders))
                   ($new-cell (or (cadr (memq cell ivy-preferred-re-builders))
                                  (car ivy-preferred-re-builders))))
      (el-patch-wrap 2 1
        (let* ((cell $cell)
               (new-cell $new-cell))
          (setq ivy--regex-function
                (el-patch-swap
                  (let ((cell $cell))
                    (car $new-cell))
                  (car new-cell)))
          (message "Ivy re builder switched to %S" (cdr new-cell)))))))

(el-patch-validate 'ivy-rotate-preferred-builders 'defun t)
