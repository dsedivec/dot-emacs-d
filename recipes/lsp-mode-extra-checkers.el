;; -*- lexical-binding: t; -*-

;; Apparently this is the approved way of adding extra checkers to
;; Flycheck after the LSP server??
;; https://github.com/flycheck/flycheck/issues/1762#issuecomment-750458442

(defvar my:lsp-flycheck-extra-checkers-alist nil)

(defun my:flycheck-checker-get-extra-checkers (orig-fun checker property)
  (or (and (eq checker 'lsp)
           (eq property 'next-checkers)
           (alist-get major-mode my:lsp-flycheck-extra-checkers-alist))
      (funcall orig-fun checker property)))

(advice-add #'flycheck-checker-get
            :around #'my:flycheck-checker-get-extra-checkers)
