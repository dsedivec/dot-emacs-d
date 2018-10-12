;; -*- lexical-binding: t; -*-

(defun my:indent-match-last-line ()
  "Indent to the level of the last non-empty line.
This actually copies the indent, so if you have mixed
tabs/spaces, those will get copied, which is usually what you
want (or at least, what *I* want)."
  (interactive)
  (let ((starting-point (point))
        ;; Get indentation from previous non-empty line.
        (indent (save-excursion
                  (beginning-of-line)
                  (skip-chars-backward "\r\n \t")
                  (back-to-indentation)
                  (buffer-substring (line-beginning-position) (point))))
        go-back-to-indentation)
    (save-excursion
      (back-to-indentation)
      ;; If cursor was inside the indentation of this line (including
      ;; at BOL) the user probably wants to be left at the end of
      ;; their new indentation.
      (setq go-back-to-indentation (<= starting-point (point)))
      (delete-horizontal-space t)
      (insert indent))
    (when go-back-to-indentation
      (back-to-indentation))))

(with-eval-after-load 'electric
  (add-to-list 'electric-indent-functions-without-reindent
               'my:indent-match-last-line))
