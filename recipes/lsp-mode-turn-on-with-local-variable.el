;; -*- lexical-binding: t; -*-

;; Run `lsp' whenever `my:use-lsp', a new safe local variable, is
;; turned on.  You can thus drop `my:use-lsp' in a .dir-locals.el file
;; and turn on lsp-mode for a given project.

(autoload 'lsp-buffer-language "lsp-mode")

(defun my:lsp-supported-buffer-p (&optional buffer)
  ;; lsp-mode could probably use a better API for "does lsp-mode have
  ;; support for this buffer".
  ;;
  ;; lsp-mode innards fail when `buffer-file-name' is nil.
  (with-current-buffer (or buffer (current-buffer))
    (when buffer-file-name
      (let ((warning-suppress-log-types '((lsp-mode))))
        (stringp (lsp-buffer-language))))))

(defvar my:use-lsp nil)
(put 'my:use-lsp 'safe-local-variable #'booleanp)

(defvar my:use-lsp-no-lsp-hook nil
  "Normal hook called after we decide we will not turn on `lsp-mode'.")

(defun my:maybe-use-lsp ()
  (if (and my:use-lsp (my:lsp-supported-buffer-p))
      (lsp)
    (run-hooks 'my:use-lsp-no-lsp-hook)))

(add-hook 'hack-local-variables-hook #'my:maybe-use-lsp)
