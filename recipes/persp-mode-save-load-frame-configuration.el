;; -*- lexical-binding: t; -*-

;; Save/restore frame configuration along with persp-mode.  Adding
;; data to perspectives inspired by
;; https://gist.github.com/gilbertw1/8d963083efea41f28bfdc85ed3c93eb4.

(defun my:persp-mode-save-frame-configuration (&rest _)
  (set-persp-parameter 'my:frame-configuration (frameset-save nil)))

(defun my:persp-mode-restore-frame-configuration (&rest _)
  (condition-case e
      (when-let ((frameset (persp-parameter 'my:frame-configuration)))
        (delete-persp-parameter 'my:frame-configuration)
        (frameset-restore frameset :reuse-frames t :cleanup-frames t))
    (t
     (warn "failed to restore frames: %S" e))))

(with-eval-after-load 'persp-mode
  (add-hook 'persp-before-save-state-to-file-functions
            #'my:persp-mode-save-frame-configuration)
  (add-hook 'persp-after-load-state-functions
            #'my:persp-mode-restore-frame-configuration
            t))
