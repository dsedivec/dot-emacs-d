;; -*- lexical-binding: t; -*-
;;
;; Change task state to PENDING when clocking in on a task not already
;; in PENDING.

(defun my:org-switch-state-on-clock-in (current-state)
  (when (and (member current-state '("NEW" "WAITING" "HOLD" "DONE"
                                     "CANCELLED"))
             (member "PENDING" org-not-done-keywords))
    "PENDING"))

(if org-clock-in-switch-to-state
    (warn "`org-clock-in-switch-to-state' is already set, not overriding")
  (setq org-clock-in-switch-to-state #'my:org-switch-state-on-clock-in))
