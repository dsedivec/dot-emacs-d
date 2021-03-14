;; -*- lexical-binding: t; -*-

;; Handy command to send current region or else current
;; "defun"/statement.  To me, this is naturally bound to C-c C-c, to
;; match some other major modes.

(defun my:python-shell-send-dwim ()
  (interactive)
  (cond
    ((use-region-p)
     (call-interactively #'python-shell-send-region))
    ((python-info-current-defun)
     (call-interactively #'python-shell-send-defun))
    (t
     (save-excursion
       (let ((start (progn (python-nav-beginning-of-statement)
                           (point)))
             (end (progn (python-nav-end-of-statement)
                         (point))))
         (python-shell-send-region start end))))))
