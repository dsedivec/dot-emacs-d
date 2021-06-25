;; -*- lexical-binding: t; -*-

;; Truncate long branch names (generally "long revisions") in the mode
;; line.  Depends `vc-mode' following the format as given in the
;; docstring for `vc-default-mode-line-string'.

(define-advice vc-mode-line (:after (&rest _) my:shorten-revision)
  (when vc-mode
    (setq vc-mode (replace-regexp-in-string
                   (concat
                    ;; Leading space and backend name
                    "^\\(\\s-*[^-:@!?]+"
                    ;; Separator, optional locker (only after colon separator)
                    "\\(?:[-@!?]\\|:\\(?:[^:]+:\\)?\\)"
                    ;; Prefix of branch name, followed by any excess
                    ".\\{,13\\}\\)\\(.*\\)")
                   (lambda (match)
                     (if (zerop (length (match-string 2 vc-mode)))
                         match
                       (concat (match-string 1 vc-mode) "…")))
                   vc-mode))))
