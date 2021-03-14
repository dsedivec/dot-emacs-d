;; -*- lexical-binding: t; -*-

;; Handy way to run Black on the region, if active, or otherwise
;; reformat with Black *and* isort, which is usually what I want.

(defun my:python-reformat-region-or-buffer ()
  (interactive)
  (if (use-region-p)
      (black-format-region (region-beginning) (region-end))
    (save-some-buffers)
    (isort-format-buffer)
    (black-format-buffer)))
