;; -*- lexical-binding: t; -*-

;; It is very annoying to not have `magit-file-mode-map' available for
;; Git-tracked files.  Let's make sure to load Magit the first time we
;; visit a file that is tracked by Git.

(defun my:magit-maybe-load ()
  (when (eq (vc-backend (buffer-file-name)) 'Git)
    (require 'magit-files))
  (when (featurep 'magit)
    (remove-hook 'find-file-hook #'my:magit-maybe-load)))

(add-hook 'find-file-hook #'my:magit-maybe-load)
