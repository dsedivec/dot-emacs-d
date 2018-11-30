;; -*- lexical-binding: t; -*-

(defun org-dblock-write:my:daily-time-summary (params)
  "Summary of whole day's time, with a total over all days.
To look at all acceptable this requires my patched org-mode that
supports :collapse-steps."
  (cl-loop
     for (name . value) in
       '((:formatter . my:org-daily-time-summary-formatter)
         (:collapse-steps . t)
         (:step . day))
     ;; We let you override these parameters if you want!  But you
     ;; shouldn't.
     unless (plist-member params name)
     do (setq params (plist-put params name value)))
  (org-dblock-write:clocktable params)
  (save-excursion
    ;; I think clocktable doesn't write the CAPTION line when you
    ;; give it :step day?  It's not writing it, in any case, so
    ;; write it ourselves.
    (org-beginning-of-dblock)
    (forward-line 1)
    (let ((time-string (format-time-string (cdr org-time-stamp-formats))))
      (insert (format "#+CAPTION: Generated at [%s]\n"
                      (substring time-string 1 -1))))
    ;; Add total line.
    (goto-char (org-table-end))
    (insert "|-\n")
    (insert "| *Total time* | |\n")
    (insert "#+TBLFM: @>$>=vsum(@<$>..@-I$>)")
    (forward-line -1)
    (org-table-recalculate)))

(defun my:org-daily-time-summary-formatter (ipos tables params)
  "clocktable formatter: sum up all clocked time on one line with start date.
Meant to be used by the my:daily-time-summary dblock (or at least :step day)."
  (let ((time-on-day (apply '+ (mapcar 'cadr tables))))
    (goto-char ipos)
    (insert "|" (plist-get params :tstart) "|"
            (org-duration-from-minutes time-on-day) "|\n")
    (org-table-align)))
