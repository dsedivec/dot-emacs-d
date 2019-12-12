;; -*- lexical-binding: t; -*-

;; I think `flyspell-prog-mode' should use `key-binding' to figure out
;; what mapping its replacing.  `local-key-binding' doesn't obey
;; command remapping.  While I'm here, how about you look up the
;; binding before you want to use it, rather than just once when you
;; turn the mode on?

;; flyspell.el contains the form below.  In Elisp, this only declares
;; the variable for the scope of the file, so (require 'flyspell)
;; won't help avoid byte compiler warnings from this variable not
;; existing here in this file.  Le sigh.
(defvar flyspell--prev-meta-tab-binding)

(defun my:flyspell-update-prev-meta-tab-binding (&rest _)
  (let ((flyspell-mode nil))
    (setq flyspell--prev-meta-tab-binding (key-binding (kbd "C-M-i")))))

(advice-add 'flyspell-auto-correct-word :before
            #'my:flyspell-update-prev-meta-tab-binding)
