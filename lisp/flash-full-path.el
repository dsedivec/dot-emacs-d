;;; flash-full-path.el --- Temporarily show buffer's full path in mode line  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: convenience
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))

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

;;; Commentary:

;; TBD

;;; Code:

(defvar flash-full-path-in-mode-line-seconds 5)

(defvar flash-full-path-in-mode-line-directory-mode
  '(dired-mode))

(defvar-local flash-full-path-in-mode-line--timer-info nil)

(defun flash-full-path-in-mode-line--restore (buf)
  (with-current-buffer buf
    (unless flash-full-path-in-mode-line--timer-info
      (error (concat "`flash-full-path-in-mode-line--timer-info' is nil,"
                     " nothing to restore.")))
    (pcase-let ((`(_ ,old-val ,new-val)
                 flash-full-path-in-mode-line--timer-info))
      (setq flash-full-path-in-mode-line--timer-info nil)
      (unless (and (local-variable-p 'mode-line-buffer-identification)
                   (eq mode-line-buffer-identification new-val))
        (error (concat "`flash-full-path-in-mode-line--restore':"
                       " `mode-line-buffer-identification'"
                       " has unexpected value.")))
      (if old-val
          ;; We have the old buffer-local value, restore it.
          (setq mode-line-buffer-identification old-val)
        ;; We established a buffer-local value, kill it.
        (kill-local-variable 'mode-line-buffer-identification))
      (force-mode-line-update))))

;;;###autoload
(defun flash-full-path-in-mode-line (&optional arg)
  "Show the buffer's full path in the mode line temporarily.

If ARG is non-nil then it is interpreted with
`prefix-numeric-value' and taken as the number of seconds for
which to temporarily change the mode line.  If this results in a
negative value, the string that would have been temporarily
displayed is instead placed on the kill ring, and the mode line
is not modified.

If ARG is nil, the full path is show in the mode line for
`flash-full-path-in-mode-line-seconds' seconds.

If the full path is already being displayed when this command is
called, and SECS is non-negative, the old buffer identification
will be immediately restored in the mode line.

If the buffer's major mode is derived from any of the modes in
`flash-full-path-in-mode-line-directory-mode' then
`default-directory` is displayed.  Otherwise, `buffer-file-name`
is displayed."
  (interactive "P")
  (unless (consp mode-line-buffer-identification)
    (error "Expected `mode-line-buffer-identification' to be a cons"))
  (let* ((secs (if arg
                   (prefix-numeric-value arg)
                 flash-full-path-in-mode-line-seconds))
         (orig-fmt mode-line-buffer-identification)
         (use-dir-instead (apply #'derived-mode-p
                                 flash-full-path-in-mode-line-directory-mode))
         (new-id (if use-dir-instead
                     default-directory
                   buffer-file-name)))
    (unless new-id
      (user-error "Buffer has no %s" (if use-dir-instead
                                         "directory"
                                       "file name")))
    (if (>= secs 0)
        (if flash-full-path-in-mode-line--timer-info
            (flash-full-path-in-mode-line--restore (current-buffer))
          (set (make-local-variable 'mode-line-buffer-identification)
               (cons (apply #'propertize new-id
                            (when (stringp (car orig-fmt))
                              (text-properties-at 0 (car orig-fmt))))
                     (cdr orig-fmt)))
          (force-mode-line-update)
          (setq-local flash-full-path-in-mode-line--timer-info
                      (list
                       (run-at-time secs
                                    nil
                                    #'flash-full-path-in-mode-line--restore
                                    (current-buffer))
                       (when (local-variable-p 'mode-line-buffer-identification)
                         orig-fmt)
                       mode-line-buffer-identification)))
      (kill-new new-id)
      (message "Copied %S to kill ring." new-id))))

(provide 'flash-full-path)
;;; flash-full-path.el ends here
