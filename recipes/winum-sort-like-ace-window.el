;; -*- lexical-binding: t; -*-
;;
;; Make winum number windows the same way ace-window does.

(require 'winum)
(require 'ace-window)

(defun my:winum-sort-windows-like-ace-window (windows)
  (sort windows #'aw-window<))

(advice-add 'winum--window-list :filter-return
            #'my:winum-sort-windows-like-ace-window)
