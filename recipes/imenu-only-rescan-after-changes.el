;; -*- lexical-binding: t; -*-

;; `which-function-mode' will blissfully rescan imenu every time it
;; fires to update, even if the buffer hasn't changed.  Let's only
;; rescan if the buffer changed.

(defvar-local my:imenu-auto-rescan-last-modification nil
  "`buffer-chars-modified-tick' when `imenu--index-alist' was last rebuilt.")

(defun my:only-update-imenu-index-after-modification (orig-fun &rest args)
  (let ((imenu-auto-rescan (or (null my:imenu-auto-rescan-last-modification)
                               (> (buffer-chars-modified-tick)
                                  my:imenu-auto-rescan-last-modification))))
    (prog1
        (apply orig-fun args)
      (setq my:imenu-auto-rescan-last-modification
            (buffer-chars-modified-tick)))))

(advice-add 'imenu--make-index-alist :around
            #'my:only-update-imenu-index-after-modification)
