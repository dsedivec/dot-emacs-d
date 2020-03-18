;; -*- lexical-binding: t; -*-
;;
;; Implements auto-saving of bookmarks through `auto-save-hook'.
;;
;; Setting `bookmark-save-flag' to 1 to save after every change works
;;  poorly, particularly with auto-named bookmarks in bookmark+ where
;;  things like *moving to the next/previous bookmark* will end up
;;  calling `bmkp-maybe-save-bookmarks' *twice* for each movement
;;  (from `bookmark-rename' and later `bmkp-update-autonamed-bookmark'
;;  itself).  Therefore we set only to save when exiting Emacs, and
;;  then on our own through `auto-save-hook'.

(require 'bookmark)

(setq bookmark-save-flag t)

(defun my:bookmark-maybe-auto-save ()
  (when (and bookmark-save-flag
             (> bookmark-alist-modification-count 0))
    (bookmark-save)))

(add-hook 'auto-save-hook #'my:bookmark-maybe-auto-save)
