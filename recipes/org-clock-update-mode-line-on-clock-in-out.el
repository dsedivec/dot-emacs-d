;; -*- lexical-binding: t; -*-
;;
;; Force update of clock display in mode line after starting/stopping
;; clock.  Can't just put `force-mode-line-update' into the hooks for
;; some reason?  It doesn't force the addition/removal of the clock
;; to/from the mode line.  Maybe clock not yet "stopped" from the
;; perspective of the mode line drawing functions when that hook is
;; run?

(defun my:org-clock-in-out-update-mode-line ()
  (run-at-time 0 nil #'force-mode-line-update))

(add-hook 'org-clock-in-hook #'my:org-clock-in-out-update-mode-line)
(add-hook 'org-clock-out-hook #'my:org-clock-in-out-update-mode-line)
