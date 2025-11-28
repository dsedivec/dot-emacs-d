;;; 1password-getter.el --- Read passwords from 1Password CLI  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Dale Sedivec

;; Author: Dale Sedivec <dsedivec@OUT-MWFYH3GQHY>
;; Keywords: comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

;; I am stupidly paranoid, and in ways that don't really help things
;; very much.  Enjoy.
(let (1password-getter-service-account-key)
  (defun 1password-getter-get-service-account-key ()
    (or 1password-getter-service-account-key
        (setq 1password-getter-service-account-key
              (read-passwd "Enter 1Password service account key: "))))

  (defun 1password-getter-forget-service-account-key ()
    (interactive)
    (setq 1password-getter-service-account-key nil)))

;; This is here for now.  I should move it later.  (Because it's kind
;; of a problem if someone tries to use it too soon.)
;;;###autoload
(defmacro 1password-def-getter (name url &optional field)
  (declare (indent 1))
  (let ((url (if field
                 `(format "%s/%s" ,url ,field)
               url))
        (cache-var (gensym))
        (status-var (gensym))
        (output-buffer-var (gensym)))
    `(let (,cache-var)
       (defun ,name ()
         (or ,cache-var
             (with-temp-buffer
               (let ((,status-var
                      (with-environment-variables
                          (("OP_SERVICE_ACCOUNT_TOKEN"
                            (1password-getter-get-service-account-key)))
                        (call-process "op" nil t nil "read" ,url))))

                 (unless (eq ,status-var 0)
                   (let ((,output-buffer-var
                          (get-buffer-create "*1password-getter-output*")))
                     (with-current-buffer ,output-buffer-var
                       (without-restriction
                         (goto-char (point-max))
                         (unless (bobp)
                           (insert "\n\n"))))
                     (insert-into-buffer ,output-buffer-var)
                     (error "1Password: op exited with status %S, see %s for details."
                            ,status-var (buffer-name ,output-buffer-var)))))
               (setq ,cache-var (buffer-substring-no-properties
                                 (point-min)
                                 ;; Remove newline at end of buffer.
                                 (max (1- (point-max)) 0)))))))))

(provide '1password-getter)
;;; 1password-getter.el ends here
