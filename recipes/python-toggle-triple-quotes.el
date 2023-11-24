;; -*- lexical-binding: t; -*-

;; New command `my:python-toggle-triple-quotes' toggles a string back
;; and forth between using single or triple quotes.

(defun my:python-toggle-triple-quotes ()
  (interactive)
  (save-excursion
    (let ((state (syntax-ppss)))
      ;; If we're out of a string, move outside of it, to its
      ;; beginning.
      (when (nth 3 state)
        (goto-char (nth 8 state)))
      (unless (looking-at "\"\"\"\\|'''\\|['\"]")
        (error "Cannot find string at point."))
      (let* ((orig-quote (match-string-no-properties 0))
             (new-quote (if (= (length orig-quote) 3)
                            (substring orig-quote 0 1)
                          (make-string 3 (elt orig-quote 0))))
             (start-mark (point-marker)))
        ;; `forward-sexp' is broken in Python modes.
        ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=56135
        ;;(forward-sexp 1)
        ;; Best effort, buggy.  Will probably work 98% of the time.
        (forward-char (length orig-quote))
        (re-search-forward (regexp-quote orig-quote))
        ;; End best effort, resume relatively normal code.
        (forward-char (- (length orig-quote)))
        (unless (looking-at orig-quote)
          (error "Did not find expected end quotes %S at %d."
                 orig-quote (point)))
        (replace-match new-quote)
        (goto-char start-mark)
        (delete-char (length orig-quote))
        (insert new-quote)))))
