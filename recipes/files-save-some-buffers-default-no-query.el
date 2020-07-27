;; -*- lexical-binding: t; -*-

;; Normally you need to give a prefix arg to `save-some-buffers' to
;; prevent being asked to save buffers.  Not being asked is the
;; behavior I want normally.  I only want to be prompted if I *do*
;; give a prefix arg.

(defun my:save-some-buffers-default-no-query (&optional query)
  "`save-some-buffers' but don't query unless given prefix arg."
  (interactive "P")
  (save-some-buffers (not query)))

(define-key global-map [remap save-some-buffers]
  #'my:save-some-buffers-default-no-query)
