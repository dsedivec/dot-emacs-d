;; -*- lexical-binding: t; -*-

;; Enable sqlup mode if it looks like the buffer has more upper case
;; SQL keywords than lower case.

(require 'sqlup-mode)

(defun my:maybe-enable-sqlup-mode ()
  (interactive)
  (when sqlup-mode
    (user-error "`sqlup-mode' is already active in this buffer"))
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (concat "\\_<\\(?:"
                          (string-join (sqlup-keywords-regexps) "\\|")
                          "\\)\\_>"))
          (num-lower 0)
          (num-upper 0))
      (while (re-search-forward regexp magic-mode-regexp-match-limit 'noerror)
        (when (and (sqlup-capitalizable-p (point))
                   (not (sqlup-blacklisted-p (downcase (match-string 0)))))
          (let ((first-char (char-after (match-beginning 0))))
            (if (= first-char (downcase first-char))
                (cl-incf num-lower)
              (cl-incf num-upper)))))
      (if (> (+ num-upper num-lower) 0)
          (let ((pct-lower (* (/ (float num-lower)
                                 (+ num-lower num-upper))
                              100)))
            (if (< num-upper num-lower)
                (message (concat "%.1f percent of keywords lower case,"
                                 " `sqlup-mode' not enabled.")
                         pct-lower)
              (message "%.1f%% of keywords upper case, enabling `sqlup-mode'."
                       (- 100 pct-lower))
              (sqlup-mode 1)))
        (message "No keywords found, enabling `sqlup-mode'")
        (sqlup-mode 1)))))
