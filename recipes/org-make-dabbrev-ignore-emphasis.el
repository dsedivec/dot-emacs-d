;; -*- lexical-binding: t; -*-

;; The problem: I often want to `dabbrev-expand' a symbol from some
;; code in an org file, but I've surrounded the symbol with ~code~
;; emphasis.  By default, dabbrev will search before point for a
;; string of characters that have word *or symbol* syntax, and that's
;; the thing it'll try to complete.  Unfortunately (?), the emphasis
;; characters such as ~ have symbol syntax in org-mode buffers, so
;; dabbrev tries to expand "~foo_bar_" instead of just "foo_bar_".
;;
;; But have no fear: dabbrev provides
;; `dabbrev-abbrev-skip-leading-regexp' for us to get around this
;; problem.  We just put every emphasis character in there.

(defun my:org-make-dabbrev-ignore-emphasis ()
  (when (and (boundp 'dabbrev-abbrev-skip-leading-regexp)
             dabbrev-abbrev-skip-leading-regexp)
    (warn (concat "Overriding global `dabbrev-abbrev-skip-leading-regexp'"
                  " in org-mode)")))
  (setq-local dabbrev-abbrev-skip-leading-regexp "[*/_=~+]"))

(add-hook 'org-mode-hook #'my:org-make-dabbrev-ignore-emphasis)
