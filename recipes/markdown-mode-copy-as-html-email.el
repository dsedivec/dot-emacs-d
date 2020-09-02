;; -*- lexical-binding: t; -*-

;; This uses my scripts markdown2email and pbcopy_html (not included
;; here, look in my dotfiles repository) to render the Markdown in the
;; buffer/region to HTML and copy it to the clipboard in such a way
;; that it is suitable for pasting into Mail.app on macOS.

(defun my:markdown-mode-copy-as-html-email (start end)
  "Copy the region or whole buffer to clipboard as HTML suitable for email."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (call-shell-region start end "markdown2email | pbcopy_html")
  (message "Copied %s to clipboard as HTML email"
           (if (use-region-p) "region" "buffer")))
