;; -*- lexical-binding: t; -*-
;;
;; More compact clock display in mode line.
;;
;; This changes the clock to just a clock emoji and the time on task.
;; If your current task has an effort (which I never use) then you
;; will get the default org-mode clock in the mode line.
;;
;; This also puts the full task name in the tooltip (AKA help-echo)
;; for the mode line string.  Note that this doesn't take
;; `org-clock-string-limit' into account, which is a bit of a
;; deficiency, though looking at `org-clock-update-mode-line' source I
;; will say that your tooltip will look stupid if
;; `org-clock-string-limit' kicks in, so I don't feel that bad about
;; ignoring it.

(require 'org)
(require 'org-clock)

(defun my:org-clock-get-compact-clock-string (orig-fun)
  (if org-clock-effort
      (funcall orig-fun)
    (let ((clocked-time (org-clock-get-clocked-time)))
      (concat (propertize "⏱" 'face '(:family "Apple Color Emoji"))
              (org-duration-from-minutes clocked-time)))))

(advice-add 'org-clock-get-clock-string :around
            #'my:org-clock-get-compact-clock-string)

(defun my:org-clock-mode-line-task-name-in-tooltip (&rest _args)
  (let* ((start-idx (if (and org-clock-task-overrun-text
                             (string-prefix-p org-clock-task-overrun-text
                                              org-mode-line-string))
                        (length org-clock-task-overrun-text)
                      0))
         (old-help-echo (get-text-property start-idx 'help-echo
                                           org-mode-line-string)))
    (put-text-property start-idx (- (length org-mode-line-string) start-idx)
                       'help-echo (concat "Task: " org-clock-heading "\n"
                                          old-help-echo)
                       org-mode-line-string)))

(advice-add 'org-clock-update-mode-line :after
            #'my:org-clock-mode-line-task-name-in-tooltip)
