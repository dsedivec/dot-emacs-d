;; -*- lexical-binding: t; -*-
;;
;; Make `insert-directory' (and thus dired) sort with LC_COLLATE=C on
;; macOS, which would otherwise sort "_foo" above ".." and such.

(defun my:insert-directory-with-locale (orig-fun &rest args)
  (with-environment-variables (("LC_COLLATE" "C"))
    (apply orig-fun args)))

(when (eq system-type 'darwin)
  (advice-add 'insert-directory :around #'my:insert-directory-with-locale))
