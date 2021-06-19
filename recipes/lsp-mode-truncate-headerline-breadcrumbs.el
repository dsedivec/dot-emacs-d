;; -*- lexical-binding: t; -*-

;; Truncate lsp-mode's header line breadcrumbs *on the left* if they
;; exceed the width of the window.  I consider the stuff on the right
;; to generally be more pertinent to the position of point, so I want
;; to see that before the stuff on the left.  For example, in a Python
;; method, the rightmost element of the breadcrumbs is usually the
;; current method, followed by the enclosing class.  If you're on a
;; long path, without this change the breadcrumbs get truncated on the
;; right, so you never see the current class and method---maybe not
;; even the current file name!
;;
;; This is *definitely* a gross approximation.  AFAIK to do this
;; remotely "right", you'd need to write the breadcrumb text into a
;; buffer, apply the header line face, and then use
;; `window-text-pixel-size' to measure its width.  That seems far too
;; much work to be doing every time you move point (and idle for a
;; bit).
;;
;; Note that the breadcrumbs may include fancy non-ASCII characters
;; that take up more than a single fixed-width ASCII character
;; (e.g. the separators between component) as well as full-on PNG
;; files (e.g. file type icons).  We just... round those up to "two
;; characters wide".  It works well enough, much better than I
;; expected in fact.

(defun my:lsp-headerline--truncate-breadcrumbs (str)
  (let ((char-width (window-font-width))
        (win-width (window-pixel-width)))
    (cl-loop
      for idx from (1- (length str)) above 0
      sum (or (plist-get (cdr (get-text-property idx 'display str)) :width)
              (if (< (aref str idx) 256)
                  char-width
                ;; Assume fancy char like a divider and 2*char-width YOLO
                (* char-width 2)))
      into substr-width
      while (<= substr-width win-width)
      finally return (if (zerop idx)
                         str
                       (concat "…" (substring str (1+ idx)))))))

(with-eval-after-load 'lsp-headerline
  (advice-add #'lsp-headerline--build-string :filter-return
              #'my:lsp-headerline--truncate-breadcrumbs))
