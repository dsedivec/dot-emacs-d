;; -*- lexical-binding: t; -*-

;; I want dumb-jump to be the penultimate alternative before asking
;; for a TAGS file.  If I have a TAGS file, use that instead.  If any
;; "smarter" backend gets loaded, use that instead of dumb-jump.

(require 'cl-lib)

(defvar my:use-dumb-jump-this-buffer 'unknown)
(make-variable-buffer-local 'my:use-dumb-jump-this-buffer)

(defun my:dumb-jump-activate-unless-TAGS-exists ()
  (when (eq my:use-dumb-jump-this-buffer 'unknown)
    (setq my:use-dumb-jump-this-buffer
          (if (and buffer-file-name
                   (locate-dominating-file buffer-file-name "TAGS"))
              nil
            (dumb-jump-xref-activate))))
  my:use-dumb-jump-this-buffer)

(add-hook 'xref-backend-functions #'my:dumb-jump-activate-unless-TAGS-exists)
