;; -*- lexical-binding: t; -*-

;; This is an improved version of expand-region's
;; `er/mark-python-string' function.  Improvements are including
;; string prefixes, and I think this version also correctly marks the
;; inside of triple-quoted strings (upstream's erroneously marks the
;; inner quote characters when MARK-INSIDE is non-nil). It may
;; incidentally be faster as it doesn't call `forward-char' in a loop,
;; but that is not at all tested.
;;
;; I should upstream this.

(require 'python)

(defun my:er/mark-python-string (mark-inside)
  "Mark the Python string that surrounds point.

If the optional MARK-INSIDE is not nil, only mark the region
between the string delimiters, otherwise the region includes the
delimiters as well."
  (let ((beginning-of-string (python-syntax-context 'string)))
    (when beginning-of-string
      (goto-char beginning-of-string)))
  ;; This allows invalid combinations of string prefix characters
  ;; because I'm lazy.
  (when (looking-at-p "[rfbuRFBU]*\\s|")
    (skip-chars-backward "rfbuRFBU"))
  (when (looking-at "[rfbuRFBU]*\\('''\\|\"\"\"\\|['\"]\\)")
    (let ((quotes (match-string 1)))
      (set-mark (if mark-inside
                    (match-end 0)
                  (point)))
      (goto-char (match-beginning 1))
      ;; Note `forward-sexp' will raise error if it can't move over
      ;; [what we hope is a] string.
      (forward-sexp)
      (when mark-inside
        (forward-char (- (length quotes))))
      (exchange-point-and-mark))))

(advice-add 'er/mark-python-string :override #'my:er/mark-python-string)
