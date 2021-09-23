;; -*- lexical-binding: t; -*-

(defun my:ns-paste-as-org ()
  (interactive)
  (let ((clipboard-html (ns-do-applescript "
use framework \"AppKit\"

set pb to current application's NSPasteboard's generalPasteboard()
set html to pb's stringForType:(its NSPasteboardTypeHTML)
if html is missing value then return null
return html as string
")))
    (unless (stringp clipboard-html)
      (user-error "Could not retrieve HTML type from clipboard"))
    (insert (with-temp-buffer
              (insert clipboard-html)
              (call-process-region (point-min) (point-max) "pandoc" t t nil
                                   "-f" "html" "-t" "org")
              (buffer-string)))))
