;; -*- lexical-binding: t; -*-
;;
;; This is a very simple company-mode backend for the LaTeX glossaries
;; package.  It just scrapes \newglossaryentry commands from the
;; buffer and offers them as completions for \gls commands under
;; company-mode.  You need to add `my:company-auctex-glossaries' to
;; `company-backends' yourself.
;;
;; Future work should at least include scraping \newacronym and
;; \newabbreviation (from glossaries-extra).  If you want to be bad
;; ass then you figure out how to crawl into included glossary files.

(require 'company)

(defvar my:company-auctex-glossaries-gls-command
  "\\\\[Gg]ls\\(?:pl\\)?\\s-*\\(?:\\[.*?]\\)?{")

(defun my:company-auctex-glossaries (command &optional arg &rest _)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'my:company-auctex-glossaries))
    (prefix
     (save-excursion
       (when-let (((looking-at-p "\\_>"))
                  (start (point))
                  (prefix-len (- (skip-chars-backward "^{")))
                  (prefix (and (not (zerop prefix-len))
                               (buffer-substring (point) start)))
                  ((re-search-backward
                    (concat my:company-auctex-glossaries-gls-command "\\=")
                    nil t)))
         prefix)))
    (candidates
     (save-excursion
       (goto-char (point-min))
       (cl-loop
          while (re-search-forward (rx-let ((spacelike (any space "\n")))
                                     (rx "\\newglossaryentry" (* spacelike)
                                         "{" (* spacelike)
                                         (group (literal arg)
                                                (*? (not "}"))
                                                )
                                         (* spacelike)
                                         (or "}" eos)))
                                   nil t)
          collect (match-string 1))))))
