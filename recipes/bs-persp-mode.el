;; -*- lexical-binding: t; -*-

;; Make bs-show only show buffers from your perspective.

(require 'bs)
(require 'persp-mode)

(defun my:bs-show-persp-aware (arg)
  "`bs-show' wrapped with `with-persp-buffer-list'."
  (interactive "P")
  (with-persp-buffer-list () (bs-show arg)))

(bind-key [remap bs-show] 'my:bs-show-persp-aware)
