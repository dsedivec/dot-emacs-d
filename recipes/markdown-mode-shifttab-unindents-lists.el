;; -*- lexical-binding: t; -*-

;; I never expect <backtab> to cycle visibility when I'm in a list.
;; (Really, I probably never expect it to cycle visibility unless I'm
;; on a headline, and maybe not even then---but I *really* don't
;; expect it in a list.)  Instead, I expect it to remove a level of
;; indentation from the current list item.  Make it so.

(require 'markdown-mode)

(defun my:markdown-shifttab-unindents-list (orig-fun &rest args)
  (if (markdown-list-item-at-point-p)
      (let* ((current-indent (current-indentation))
             (next-unindent
              (and (markdown-list-item-at-point-p)
                   (seq-reduce (lambda (best-choice candidate)
                                 (if (and (> candidate best-choice)
                                          (< candidate current-indent))
                                     candidate
                                   best-choice))
                               (markdown-calc-indents)
                               0))))
        (if (< next-unindent current-indent)
            (save-excursion
              (indent-line-to next-unindent))
          (user-error "Can't unindent any more")))
    (apply orig-fun args)))

(advice-add 'markdown-shifttab :around #'my:markdown-shifttab-unindents-list)
