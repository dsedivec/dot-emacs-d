;; -*- lexical-binding: t; -*-

;; Reverting a buffer doesn't delete its auto-save file?  Well, OK,
;; fine, I'll delete it.

(defun my:delete-auto-save-after-revert ()
  (let ((auto-save-file-name (make-auto-save-file-name)))
    (when (and (file-exists-p auto-save-file-name)
               ;; Buffer really shouldn't be modified, just being
               ;; extra-safe here.
               (not (buffer-modified-p)))
      (message "Deleting auto save file %s after revert" auto-save-file-name)
      (delete-file auto-save-file-name))))

(add-hook 'after-revert-hook #'my:delete-auto-save-after-revert)
