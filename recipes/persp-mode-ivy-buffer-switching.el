;; -*- lexical-binding: t; -*-

;; Perspective-aware buffer switching with Ivy, courtesy
;; https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-ivy-el
;; (linked from persp-mode.el project).  I decided that I did not need
;; all of the code there, so this is just a subset, and a modified
;; subset at that.

(defun my:persp-mode-ivy-filter-buffers (buffer)
  (when-let ((persp (and persp-mode (get-current-persp))))
    (not (persp-contain-buffer-p buffer persp))))

(my:with-eval-after-all-load '(persp-mode ivy)
  (add-hook 'ivy-ignore-buffers #'my:persp-mode-ivy-filter-buffers))
