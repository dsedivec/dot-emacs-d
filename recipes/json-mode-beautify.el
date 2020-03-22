;; -*- lexical-binding: t; -*-

;; json-reformat is required by json-mode, but probably obsolete now
;; that we have `json-pretty-print' and friends.  (See also
;; https://github.com/joshwnj/json-mode/issues/43.)  Most importantly
;; for me, I usually want to sort object keys, so just replace
;; `json-mode-beautify' with a thin wrapper around
;; `json-pretty-print-buffer-ordered'.

(require 'json)

(defun my:json-mode-beautify ()
  (let ((json-encoding-default-indentation (make-string js-indent-level ?\s)))
    (if (use-region-p)
        (json-pretty-print-ordered (region-beginning) (region-end))
      (json-pretty-print-buffer-ordered))))

(advice-add 'json-mode-beautify :override #'my:json-mode-beautify)
