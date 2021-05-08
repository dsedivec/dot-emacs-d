;; -*- lexical-binding: t; -*-

;; hippie-exp doesn't actually use dabbrev, so my dabbrev fix for
;; expanding "~foo_" in org-mode buffers doesn't work.  (See comment
;; on my `org-mode-hook' that sets
;; `dabbrev-abbrev-skip-leading-regexp'.)
;;
;; hippie-exp has no analogue, so I am left to use advice.

(require 'dabbrev)

(defun my:he-dabbrev-beg-obey-dabbrev-abbrev-skip-leading-regexp
    (completion-start)
  (let ((where-called (point)))
    (if (or (not (boundp 'dabbrev-abbrev-skip-leading-regexp))
            (not dabbrev-abbrev-skip-leading-regexp)
            (equal completion-start where-called))
        completion-start
      (save-excursion
        (goto-char completion-start)
        ;; `dabbrev-abbrev-skip-leading-regexp' skips *characters*,
        ;; one at a time (see `dabbrev--goto-start-of-abbrev').  We
        ;; just wrap it in \(\)+ so that we skip them all with one
        ;; call to `re-search-forward'.  Hopefully that's OK.
        (or (re-search-forward (format "\\(?:%s\\)+"
                                       dabbrev-abbrev-skip-leading-regexp)
                               where-called t)
            completion-start)))))

(advice-add 'he-dabbrev-beg :filter-return
            #'my:he-dabbrev-beg-obey-dabbrev-abbrev-skip-leading-regexp)
