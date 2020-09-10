;; -*- lexical-binding: t; -*-

;; As of 2019-02-20 on Emacs master (ae77728d14e), browse-url-emacs
;; has... problems.  Any buffer with a URL for a file name has
;; problems, methinks.  Therefore I wrote this to just suck a URL into
;; a buffer.
;;
;; As an added bonus, this function does some *truly awful shit* to
;; try and get you a "better" major mode than `fundamental-mode',
;; which is probably at least 50% a bad idea.

(eval-when-compile (require 'mailcap)
                   (require 'mm-decode))

(autoload 'browse-url-interactive-arg "browse-url")

(defun my:find-url (url)
  (interactive (list (car (browse-url-interactive-arg "URL: "))))
  (let* ((url-buf (url-retrieve-synchronously url)))
    (unwind-protect
         (progn
           (switch-to-buffer (generate-new-buffer (format "*URL %s*" url)))
           (url-insert url-buf)
           (goto-char (point-min))
           ;; This is probably awful: fake a `buffer-file-name' based
           ;; on MIME type so that `set-auto-mode' might be able to
           ;; pick a major mode for this buffer other than
           ;; `fundamental-mode'.  I just bet this fake
           ;; `buffer-file-name' is going to cause problems for me.
           ;;
           ;; This use of the mm-* functions is based on `url-insert'.
           (let* ((mm-handle (with-current-buffer url-buf
                               (mm-dissect-buffer t)))
                  (mime-type (prog1
                                 (mm-handle-media-type mm-handle)
                               (mm-destroy-parts mm-handle)))
                  (file-extension (progn
                                    (mailcap-parse-mimetypes)
                                    (car (rassoc mime-type
                                                 mailcap-mime-extensions))))
                  (buffer-file-name (if file-extension
                                        (concat "x." file-extension)
                                      buffer-file-name)))
             (message "MIME type %S buffer file name %S"
                      mime-type buffer-file-name)
             (set-auto-mode))
           (when url-buf
             (kill-buffer url-buf))))))
