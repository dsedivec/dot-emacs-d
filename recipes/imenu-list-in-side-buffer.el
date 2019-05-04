;; -*- lexical-binding: t; -*-

;; Overrides `imenu-list-install-display-buffer' to instead make
;; `display-buffer' show `imenu-list' in a side window of
;; `imenu-list-size' width/height.
;;
;; This also sets the new side window to be a dedicated window and
;; preserves the window's height/width (see `window-preserve-size'),
;; as appropriate for the value of `imenu-list-position'.
;;
;; You will probably need to call
;; `my:imenu-list-install-display-buffer' if you change
;; `imenu-list-position' or `imenu-list-size'.

(require 'cl-lib)

(defun my:imenu-list-install-display-buffer ()
  "Put imenu-list buffer on a dedicated side window with a preserved size."
  (let* ((side (cl-ecase imenu-list-position
                 (above 'top)
                 (below 'bottom)
                 ((left right) imenu-list-position)))
         (preserve-dimen (if (memq side '(left right))
                             'window-width
                           'window-height)))
    (setf (alist-get (concat "^" (regexp-quote imenu-list-buffer-name) "$")
                     display-buffer-alist nil nil #'equal)
          `(display-buffer-in-side-window
            (side . ,side)
            ;; It is not totally clear to me if `imenu-list-size' is
            ;; supposed to be the window's body height/width or the
            ;; window's total height/width.  The way we're using it
            ;; here it is definitely the total, not the body.
            ;;
            ;; If it's supposed to be the body height/width then I
            ;; think it might be best to pass a function as the value
            ;; for the `window-height'/`window-width' here, and then
            ;; have the function compute it once the imenu-list window
            ;; has come into existence.  (See documentation for those
            ;; alist entries in the docstring for `display-buffer'.)
            (,preserve-dimen . ,imenu-list-size)
            (preserve-size . ,(if (eq preserve-dimen 'window-width)
                                  '(t . nil)
                                '(nil . t)))
            (dedicated . t)))))

(advice-add 'imenu-list-install-display-buffer :override
            #'my:imenu-list-install-display-buffer)

(when (featurep 'imenu-list)
  ;; Too late, imenu-list already loaded.  Call our new setup function.
  (my:imenu-list-install-display-buffer))
