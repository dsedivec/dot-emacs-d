;; -*- lexical-binding: t; -*-

;; Jump over the priority cookie after adding it to a headline.  This
;; is usually because I (1) make a new headline, (2) set its priority,
;; (3) want to type into the headline.  Without this, after (2), point
;; is still before the priority cookie.

(defun my:org-priority-move-over-new-cookie (orig-fun &rest args)
  ;; Not using `org-priority-regexp' because it starts with ".*?"
  ;; which would match *way* more than I intend.  Using [:blank:]
  ;; instead of \s- here because I don't want to match new lines, and
  ;; by default new line is space syntax.
  (let* ((priority-regexp "[[:blank:]]*\\[#[A-Z0-9]][[:blank:]]*")
         (started-looking-at-priority (looking-at-p priority-regexp)))
    (prog1
        (apply orig-fun args)
      (when (and (not started-looking-at-priority)
                 (looking-at priority-regexp))
        (goto-char (match-end 0))))))

(advice-add 'org-priority :around #'my:org-priority-move-over-new-cookie)
