;; -*- lexical-binding: t; -*-

;; Overrides `imenu-list-install-display-buffer' to instead make
;; `display-buffer' show `imenu-list' in a side window of
;; `imenu-list-size' width/height.
;;
;; This also sets the new side window to be dedicated and marks that
;; its height/width should be preserved, as appropriate for the value
;; of `imenu-list-position'.
;;
;; You will probably need to call
;; `my:imenu-list-install-display-buffer' if you change
;; `imenu-list-position' or `imenu-list-size'.
;;
;; I think this somewhat changes the meaning of `imenu-list-size',
;; because *I think* before this advice it specified the text
;; width/height, and after this advice it specifies the total
;; width/height.  See `(elisp)Windows Sizes' for the difference.

(defun my:imenu-list-install-display-buffer ()
  "Put imenu-list on a dedicated left side window with a fixed size."
  (setf (alist-get "^\\*Ilist\\*$" display-buffer-alist nil nil #'equal)
        `(display-buffer-in-side-window
          (side . ,imenu-list-position)
          (window-width . ,imenu-list-size)
          ;; Means preserve width, not height.  I think.
          (preserve-size . ,(if (memq imenu-list-position '(left right))
                                '(t . nil)
                              '(nil . t)))
          (dedicated . t))))

(advice-add 'imenu-list-install-display-buffer :override
            #'my:imenu-list-install-display-buffer)
