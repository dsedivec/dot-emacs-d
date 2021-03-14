;; -*- lexical-binding: t; -*-

;; You can use this as a :formatter in clocktables where you've
;; forcibly narrowed headlines and don't want bracketed links to be
;; truncated, as they often are since the link target (e.g. a URI
;; which doesn't have any spaces) comes first in such a construct.
;; This just converts the link to its descriptive text.
;;
;; You can use this formatter for a clock block like:
;;
;;     #+BEGIN: clocktable :formatter my:org-clocktable-formatter-strip-links

(require 'subr-x)
(require 'ol)

(defun my:org-clocktable-formatter-strip-links (ipos tables params)
  ;; I *think* it's OK to destructively modify TABLES.  I think the
  ;; formatter is the one and only user of this value.  See
  ;; org-dblock-write:clocktable.
  (mapc (lambda (file)
          (mapc (lambda (entry)
                  (when-let* ((headline (nth 1 entry))
                              (link-desc (and (string-match org-link-bracket-re
                                                            headline)
                                              ;; First group is URL,
                                              ;; second group is
                                              ;; optional link text
                                              (match-string 2 headline))))
                    (setf (nth 1 entry) link-desc)))
                (nth 2 file)))
        tables)
  (funcall org-clock-clocktable-formatter ipos tables params))
