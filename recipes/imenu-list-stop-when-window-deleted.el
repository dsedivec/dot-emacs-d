;; -*- lexical-binding: t; -*-

;; If you kill the window imenu-list is in, this turns off
;; `imenu-list-minor-mode', which will also kill its update timer.
;; If, on the other hand, you display a buffer with the same name as
;; given in `imenu-list-buffer-name', this turns on imenu-list,
;; including its update timer.

(defun my:imenu-list--watch-for-window-create-or-delete ()
  (let ((imenu-list-visible (get-buffer-window imenu-list-buffer-name t)))
    (cond
      ((and imenu-list-minor-mode (not imenu-list-visible))
       (imenu-list-minor-mode -1))
      ((and (not imenu-list-minor-mode) imenu-list-visible)
       (imenu-list-minor-mode 1)))))

(with-eval-after-load 'imenu-list
  (add-hook 'window-configuration-change-hook
            #'my:imenu-list--watch-for-window-create-or-delete))
