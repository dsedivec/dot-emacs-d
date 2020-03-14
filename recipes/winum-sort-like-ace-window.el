;; -*- lexical-binding: t; -*-
;;
;; Make winum number windows the same way ace-window does.

(with-eval-after-load 'winum
  (with-eval-after-load 'ace-window
    (defun my:winum-sort-windows-like-ace-window (windows)
      (sort windows #'aw-window<))

    (advice-add 'winum--window-list :filter-return
                #'my:winum-sort-windows-like-ace-window)))
