;; -*- lexical-binding: t; -*-

;; New command `my:link-hint-multi-dispatch' either opens link at
;; point, or else invokes link-hint to open/copy some link on screen.

(autoload 'link-hint--get-link-at-point "link-hint")

(defun my:link-hint-multi-dispatch (copy-link)
  "Open or copy links using link-hint.
If there is a link at point, `link-hint-open-link-at-point' is
used.  Otherwise, `link-hint-open-link' is called.  If COPY-LINK
is true, or if used interactively with a prefix argument, copy
the selected link instead of opening it."
  (interactive "P")
  (if (link-hint--get-link-at-point)
      (link-hint--action-at-point (if copy-link :copy :open))
    (call-interactively (if copy-link
                            #'link-hint-copy-link
                          #'link-hint-open-link))))
