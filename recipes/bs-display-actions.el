;; -*- lexical-binding: t; -*-

;; When you call `bs-show', bs actually calls `split-window-below'
;; itself, which means that `display-buffer-alist' and Shackle don't
;; get a chance to influence how the bs buffer will be displayed.
;; This patch gives Shackle et al. a shot at influencing just
;; how/where the bs window will be displayed.

(el-patch-feature bs)

(with-eval-after-load 'bs
  (el-patch-defun bs--show-with-configuration (name &optional arg)
    "Display buffer list of configuration with name NAME.
Set configuration NAME and determine window for Buffer Selection Menu.
Unless current buffer is buffer *buffer-selection* we have to save
the buffer we started Buffer Selection Menu and the current window
configuration to restore buffer and window configuration after a
selection.  If there is already a window displaying *buffer-selection*
select this window for Buffer Selection Menu.  Otherwise open a new
window.
The optional argument ARG is the prefix argument when calling a function
for buffer selection."
    (bs-set-configuration name)
    (let ((bs--show-all (or bs--show-all arg)))
      (unless (string= "*buffer-selection*" (buffer-name))
        ;; Only when not in buffer *buffer-selection*
        ;; we have to set the buffer we started the command
        (setq bs--buffer-coming-from (current-buffer)))
      (let ((liste (bs-buffer-list))
            (active-window (get-window-with-predicate
                            (lambda (w)
                              (string= (buffer-name (window-buffer w))
                                       "*buffer-selection*"))
                            nil (selected-frame))))
        (if active-window
            (select-window active-window)
          (bs--restore-window-config)
          (setq bs--window-config-coming-from (current-window-configuration))
          (el-patch-remove
            (when (> (window-height) 7)
              ;; Errors would mess with the window configuration (bug#10882).
              (ignore-errors (select-window (split-window-below))))))
        (el-patch-wrap 2 0
          (let ((switch-to-buffer-obey-display-actions t))
            (bs-show-in-buffer liste)))
        (bs-message-without-log "%s" (bs--current-config-message)))))

  (el-patch-validate 'bs--show-with-configuration 'defun t))
