;; -*- lexical-binding: t; -*-

;; This new command can be used to automatically call mark the inside
;; of a string at point in a Python file before calling
;; `edit-indirect-region'.  If the region is already active, or if
;; you're not inside a string, this is just like calling
;; `edit-indirect-region'.
;;
;; Requires package edit-indirect.

(defun my:python-edit-indirect-dwim ()
  (interactive)
  (if (use-region-p)
      (edit-indirect-region (region-beginning) (region-end) t)
    (let ((state (syntax-ppss))
          begin
          quote-len
          end)
      (unless (nth 3 state)
        (user-error "No string here to edit indirectly."))
      (save-excursion
        (goto-char (nth 8 state))
        (unless (looking-at (rx (or (: (group-n 1 (or "\"\"\"" "'''"))
                                       (* space) (? ?\\) (? ?\n))
                                    (group-n 1 (any "\"'")))))
          (error "Can't skip over opening quotes of string at %s." (point)))
        (setq begin (match-end 0)
              quote-len (length (match-string 1)))
        (forward-sexp 1)
        (forward-char (- quote-len))
        (when (= quote-len 3)
          (skip-syntax-backward "-"))
        (setq end (point)))
      (edit-indirect-region begin end t))))
