;; -*- lexical-binding: t; -*-

;; Fix for
;; https://github.com/Malabarba/aggressive-indent-mode/issues/138.
;; Per
;; https://github.com/Malabarba/aggressive-indent-mode/issues/138#issuecomment-659827219
;; there, I believe the timer does not need to repeat, since it's just
;; being canceled at the end of the handler anyway.

(require 'aggressive-indent)

(el-patch-feature aggressive-indent)

(el-patch-defun aggressive-indent--indent-if-changed (buffer)
  "Indent any region that changed in BUFFER in the last command loop."
  (el-patch-wrap 2 0
    (when (buffer-live-p buffer)
      (el-patch-splice 3 0
        (if (not (buffer-live-p buffer))
            (cancel-timer aggressive-indent--idle-timer)
          (with-current-buffer buffer
            (when (and aggressive-indent-mode aggressive-indent--changed-list)
              (save-excursion
                (save-selected-window
                  (aggressive-indent--while-no-input
                    (aggressive-indent--proccess-changed-list-and-indent))))
              (el-patch-remove
                (when (timerp aggressive-indent--idle-timer)
                  (cancel-timer aggressive-indent--idle-timer))))))))))

(el-patch-validate 'aggressive-indent--indent-if-changed 'defun t)

(el-patch-defun aggressive-indent--keep-track-of-changes (l r &rest _)
  "Store the limits (L and R) of each change in the buffer."
  (when aggressive-indent-mode
    (push (list l r) aggressive-indent--changed-list)
    (when (timerp aggressive-indent--idle-timer)
      (cancel-timer aggressive-indent--idle-timer))
    (setq aggressive-indent--idle-timer
          (run-with-idle-timer aggressive-indent-sit-for-time
                               (el-patch-swap t nil)
                               #'aggressive-indent--indent-if-changed
                               (current-buffer)))))

(el-patch-validate 'aggressive-indent--keep-track-of-changes 'defun t)
