;; -*- lexical-binding: t; -*-

;; This mechanism for making C-g close pop-up windows is based very
;; heavily on that sound in window-purpose's popwin-like extension.
;; Add `my:shackle-display-pop-up-window' via :custom in Shackle rules
;; to use.  Bring your own `my:pop-up-buffer-p', which returns non-nil
;; when the given buffer should be considered a pop-up buffer.

(defun my:shackle-display-pop-up-window (buffer alist plist)
  (cl-remf plist :custom)
  (prog1
      (shackle-display-buffer buffer alist plist)
    (global-set-key [remap keyboard-quit] #'my:shackle-close-pop-up-windows)))

(defun my:shackle-close-pop-up-windows ()
  (interactive)
  (global-set-key [remap keyboard-quit] nil)
  (let ((deleted-any-windows nil))
    (dolist (window (window-list (selected-frame) 'nominibuf))
      (let ((buffer (window-buffer window)))
        (when (my:pop-up-buffer-p buffer)
          (delete-window window)
          (bury-buffer buffer)
          (setq deleted-any-windows t))))
    (unless deleted-any-windows
      (keyboard-quit))))
