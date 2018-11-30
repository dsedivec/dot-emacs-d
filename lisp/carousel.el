;;; carousel.el --- Delete old buffers               -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Dale Sedivec

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

;; Deletes buffers that haven't been used after a certain period of
;; time.

;;; Code:

(require 'subr-x)

(defgroup carousel nil "Delete old buffers"
  :group 'convenience)

(defcustom carousel-non-file-buffer-max-age 3600
  "How old a non-file buffer can be before it's killed."
  :type 'integer
  :group 'carousel)

(defcustom carousel-safe-buffer-regexps
  '("\\`\\*\\(?:scratch\\|Messages\\|Warnings\\|ielm\\|spacemacs\\)\\*\\'"
    "\\` \\*\\(?:Minibuf-[[:digit:]]+\\|Echo Area [[:digit:]]+\\)\\*\\'")
  "List of regular expressions of buffers that will never be auto-killed."
  :type '(set regexp)
  :group 'carousel)

(defcustom carousel-safe-buffer-modes
  '(dired-mode)
  "List of major modes whose buffers will never be auto-killed."
  :type '(set symbol)
  :group 'carousel)

(defcustom carousel-dry-run nil
  "Don't actually kill buffers, just report what you would do."
  :type 'boolean
  :group 'carousel)

(defcustom carousel-debug nil
  "When true, `carousel-kill-buffers' is verbose."
  :type 'boolean
  :group 'carousel)

(defvar carousel-timer nil)

(defvar carousel-never-viewed-buffer-info
  (make-hash-table :weakness 'key)
  "Keys are buffers, values are (last-seen . modified-tick).")

(defun carousel-buffer-last-used-time (buf)
  (if-let ((buf-disp-time (buffer-local-value 'buffer-display-time buf)))
      (time-to-seconds buf-disp-time)
    (let ((buf-info (gethash buf carousel-never-viewed-buffer-info))
          (buf-mod-tick (buffer-modified-tick buf)))
      (if (and buf-info
               (eql (cdr buf-info) buf-mod-tick))
          (car buf-info)
        (puthash buf (cons (float-time) buf-mod-tick)
                 carousel-never-viewed-buffer-info)
        nil))))

(defun carousel-kill-buffers ()
  (interactive)
  (when carousel-debug
    (message "carousel-kill-buffers running"))
  (let ((now (float-time))
        (oldest-survivor 0))
    (dolist (buf (buffer-list))
      (unless (or (not (buffer-live-p buf))
                  (buffer-file-name buf)
                  (get-buffer-process buf)
                  ;; midnight does this.  Not sure if it's necessary,
                  ;; but my guess is that maybe a buffer could be
                  ;; visible but not yet have its
                  ;; `buffer-display-time' set.
                  (get-buffer-window buf 'visible)
                  (let ((buf-name (buffer-name buf)))
                    (seq-some (lambda (regexp)
                                (string-match-p regexp buf-name))
                              carousel-safe-buffer-regexps))
                  (let ((buf-mode (buffer-local-value 'major-mode buf)))
                    (seq-some (lambda (mode)
                                (eq mode buf-mode))
                              carousel-safe-buffer-modes)))
        (let* ((buf-used-time (or (carousel-buffer-last-used-time buf) now))
               (secs-since-last-viewed
                (- now buf-used-time)))
          (when carousel-debug
            (message "Considering %S, %S s old" buf secs-since-last-viewed))
          (if (< secs-since-last-viewed carousel-non-file-buffer-max-age)
              (setq oldest-survivor (max oldest-survivor
                                         secs-since-last-viewed))
            (message "Carousel killing buffer %S" (buffer-name buf))
            (unless carousel-dry-run
              (kill-buffer buf))))))
    (when carousel-mode
      (cl-assert (< oldest-survivor carousel-non-file-buffer-max-age) t)
      (when (timerp carousel-timer)
        (cancel-timer carousel-timer))
      (setq carousel-timer (run-at-time (- carousel-non-file-buffer-max-age
                                           oldest-survivor)
                                        nil #'carousel-kill-buffers)))))

(define-advice carousel-kill-buffers
    (:around (orig-fun &rest args) my:carousel-override-persp-mode)
  ;; Will have no effect if `persp-mode' isn't in use.  (Right?)
  (let ((persp-kill-foreign-buffer-behaviour 'kill))
    (apply orig-fun args)))

;;;###autoload
(define-minor-mode carousel-mode
    "Kill buffers when they get too old.

It's like `midnight-mode' but just for deleting old \"special\"
buffers, and with slightly more aggressive timing.

Turning this mode on will immediately kill any old buffers."
  :global t
  ;; Can't decide between 🔥 and ☥.
  :lighter "🔥"
  (when (timerp carousel-timer)
    (cancel-timer carousel-timer))
  (when carousel-mode
    (carousel-kill-buffers)))

(provide 'carousel)
;;; carousel.el ends here
