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

;; This is here for now.  I should move it later.  (Because it's kind
;; of a problem if someone tries to use it too soon.)
(defmacro my:def-1password-getter (name url &optional field)
  (declare (indent 1))
  (let ((url (if field
                 `(format "%s/%s" ,url ,field)
               url))
        (cache-var (gensym))
        (status-var (gensym)))
    `(let (,cache-var)
       (defun ,name ()
         (or ,cache-var
             (with-temp-buffer
               (let ((,status-var (call-process "op" nil t nil "read" ,url)))
                 (unless (eq ,status-var 0)
                   (error "1Password: op exited with status %S" ,status-var)))
               (setq ,cache-var (buffer-substring-no-properties
                                 (point-min)
                                 ;; Remove newline at end of buffer.
                                 (max (1- (point-max)) 0)))))))))

(provide '1password-getter)
;;; 1password-getter.el ends here
