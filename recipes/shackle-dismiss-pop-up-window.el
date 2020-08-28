;; -*- lexical-binding: t; -*-

;; This mechanism for making C-g close pop-up windows is based very
;; heavily on that sound in window-purpose's popwin-like extension.
;; Add `my:shackle-display-pop-up-window' via :custom in Shackle rules
;; to use.  Bring your own `my:pop-up-buffer-p', which returns non-nil
;; when the given buffer should be considered a pop-up buffer.

(require 'cl-lib)

(require 'shackle)

(defvar my:shackle-pop-up-buffer-predicate
  #'my:shackle-default-pop-up-buffer-p)

(defun my:shackle-default-pop-up-buffer-p (buffer)
  (with-current-buffer buffer
    (derived-mode-p 'compilation-mode
                    'flycheck-error-list-mode
                    'help-mode
                    'osx-dictionary-mode
                    'helpful-mode)))

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
        (when (funcall my:shackle-pop-up-buffer-predicate buffer)
          (delete-window window)
          (bury-buffer buffer)
          (setq deleted-any-windows t))))
    (unless deleted-any-windows
      (keyboard-quit))))
