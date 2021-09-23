;; -*- lexical-binding: t; -*-

(declare-function org-download-screenshot "org-download")
(defvar org-download-screenshot-method)

(defun my:org-download-screenshot-from-clipboard-with-applescript (output-path)
  (let ((image-class (pcase (downcase (file-name-extension output-path))
                       ("png" "«class PNGf»")
                       ("jpg" "JPEG picture")
                       ("gif" "GIF picture")
                       (_ (error "Don't know image type for %S" output-path)))))
    (ns-do-applescript
     (format "
if length of (clipboard info for %s) > 0 then
        set outputPath to POSIX file \"%s\"
        set fh to null
        try
                set fh to open for access outputPath with write permission
                write (the clipboard as %s) to fh
                close access fh
        on error errStr number errorNumber
                if fh is not null then
                        close access fh
                end if
                error errStr number errorNumber
        end try
end if
"
             image-class
             (replace-regexp-in-string (rx (any "\r\t\n\\\"")) "\\&" output-path)
             image-class))))

(defun my:org-download-clipboard-with-applescript (&optional basename)
  (interactive)
  (let ((org-download-screenshot-method
         #'my:org-download-screenshot-from-clipboard-with-applescript))
    (org-download-screenshot basename)))

(when (functionp 'ns-do-applescript)
  (advice-add 'org-download-clipboard :override
              #'my:org-download-clipboard-with-applescript))
