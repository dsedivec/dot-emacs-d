;;; idle-save.el --- Save buffers when you're idle   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: convenience

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

;; This provides a minor mode to save a changed buffer when you're
;; idle.

;;; Code:

(defvar idle-save-buffer-delay 1
  "Number of seconds idle before buffer is saved.")

(defvar idle-save-buffer-timer nil)

;;;###autoload
(define-minor-mode idle-save-buffer-mode
    "Save the buffer when idle."
  :init-value nil
  :lighter "💾"
  (if (not idle-save-buffer-mode)
      (when (and idle-save-buffer-timer
                 (cl-notany
                  (lambda (buf) (buffer-local-value 'idle-save-buffer-mode buf))
                  (buffer-list)))
        (cancel-timer idle-save-buffer-timer)
        (setq idle-save-buffer-timer nil))
    (when idle-save-buffer-timer
      (cancel-timer idle-save-buffer-timer))
    (setq idle-save-buffer-timer
          (run-with-idle-timer idle-save-buffer-delay t
                               #'idle-save-buffer-save-some-buffers))))

(defun idle-save-buffer-save-some-buffers ()
  ;; Hey!  Don't just use `save-some-buffers' here because (1) it does
  ;; more than you want like potentially saving "abbreviations" (??);
  ;; and (2) it wraps its work in `save-window-excursion' whicih
  ;; really fucks up eww buffers when I'm live-previewing Markdown for
  ;; some reason.
  (dolist (buf (buffer-list))
    ;; I don't exactly understand why I need to check `buffer-live-p'
    ;; but I think it's because saving one buffer may kill others, and
    ;; we've already got a reference to that killed buffer from
    ;; `buffer-list' (above).
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and idle-save-buffer-mode
                   (buffer-modified-p))
          (save-buffer))))))

(provide 'idle-save)
;;; idle-save.el ends here
