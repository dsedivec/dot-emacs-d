;; -*- lexical-binding: t; -*-

;; Why is it so fucking hard to kill a rectangle and then yank it on
;; to some new lines?  This is like my #1 most common operation I
;; think.
(defun my:yank-rectangle-to-new-lines ()
  (interactive)
  (unless killed-rectangle
    (error (concat "`killed-rectangle' is not set"
                   " (did you mean to `copy-rectangle-as-kill'?)")))
  (save-excursion (newline (length killed-rectangle)))
  (yank-rectangle))

(bind-keys ("C-x r C-y" . my:yank-rectangle-to-new-lines))
