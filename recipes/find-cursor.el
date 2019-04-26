;; -*- lexical-binding: t; -*-

;; New command `my:find-cursor' flashes your cursor so you can find
;; it.

(defface my:find-cursor-face
    '((t (:background "red")))
  "Face used to highlight the cursor in `my:find-cursor'."
  :group 'pulse)

(defun my:find-cursor ()
  (interactive)
  (pulse-momentary-highlight-one-line (point) 'my:find-cursor-face))
