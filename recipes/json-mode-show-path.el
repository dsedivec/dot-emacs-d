;; -*- lexical-binding: t; -*-

;; json-snatcher is broken.  `json-mode-show-path' also seems broken.
;; `json-path-to-position' actually works, so wrap it in a command.
;; Bonus: a function to show the path in jq syntax.

(require 'json)

(defun my:json-mode-show-path (&optional pos)
  "Print the sexp path to the node at POS and put it on the kill ring.
POS defaults to point."
  (interactive "d")
  (let* ((path (plist-get (json-path-to-position (or pos (point))) :path))
         (path-str (format "%S" path)))
    (kill-new path-str)
    (message path-str)))

(defun my:json-mode-show-path-jq (&optional pos)
  "Print the path to the node at POS in jq syntax and put it on the kill ring.
POS defaults to point."
  (interactive "d")
  (let* ((path (plist-get (json-path-to-position (or pos (point))) :path))
         (path-str (seq-reduce #'concat
                               (mapcar (lambda (elt)
                                         (if (numberp elt)
                                             (format "[%d]" elt)
                                           (format ".%s" elt)))
                                       path)
                               "")))
    (kill-new path-str)
    (message path-str)))
