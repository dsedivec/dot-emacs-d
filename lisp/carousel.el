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

(eval-when-compile
  ;; We override persp-mode's prompt when killing a buffer from
  ;; another perspective, if you're using persp-mode.
  (require 'persp-mode nil t))

(defgroup carousel nil "Delete old buffers"
          :group 'convenience)

(defcustom carousel-file-buffer-max-age (* 72 60 60)
  "How old, in seconds, a file-backed buffer must be before it is kill-able.

If nil, file-backed buffers will never be killed in Carousel."
  :type '(choice integer (const nil))
  :group 'carousel)

(defcustom carousel-non-file-buffer-max-age 3600
  "How old, in seconds, a non-file buffer must be before it is kill-able.

If nil, non-file buffers will never be killed in Carousel."
  :type '(choice integer (const nil))
  :group 'carousel)

(defcustom carousel-safe-buffer-regexps
  (list (rx string-start
            (or " "
                (: "*"
                   (or "scratch" "Messages" "Warnings" "ielm" "spacemacs")
                   "*"
                   string-end))))
  "List of regular expressions of buffers that will never be auto-killed.
Default value excludes buffers that begin with a space or buffers
named *scratch*, *Messages*, *Warnings*, *ielm*, or *spacemacs*."
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

(defvar carousel-mode)

(defun carousel-buffer-last-used-time (buf)
  (if-let* ((buf-disp-time (buffer-local-value 'buffer-display-time buf)))
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
        (next-run (min carousel-file-buffer-max-age
                       carousel-non-file-buffer-max-age)))
    (dolist (buf (buffer-list))
      ;; If the max-age variable for the type of buffer (file
      ;; vs. non-file) is nil then we will not kill any buffers of
      ;; that type.
      (let ((max-age (if (buffer-file-name buf)
                         carousel-file-buffer-max-age
                       carousel-non-file-buffer-max-age)))
        (unless (or (null max-age)
                    (not (buffer-live-p buf))
                    ;; Buffers connected to a process/socket should
                    ;; never be killed.
                    (get-buffer-process buf)
                    ;; Not going to kill a buffer that's going to
                    ;; prompt us to save it anyway.
                    (buffer-modified-p buf)
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
                 (secs-since-last-viewed (- now buf-used-time))
                 (secs-left (- max-age secs-since-last-viewed)))
            (when carousel-debug
              (message "Considering %S, %S s old, %S left to live"
                       buf secs-since-last-viewed secs-left))
            (if (> secs-left 0)
                (setq next-run (min next-run secs-left))
              (message "Carousel killing buffer %S" (buffer-name buf))
              (unless carousel-dry-run
                (condition-case err
                    (kill-buffer buf)
                  (t
                   (when carousel-debug
                     (message "Carousel got %S trying to kill buffer %S: %S"
                              (car err) (buffer-name buf) (cdr err)))))))))))
    (when carousel-mode
      (let ((inhibit-quit t))
        (when (timerp carousel-timer)
          (cancel-timer carousel-timer))
        ;; Repeat after 60 seconds in case we get interrupted before
        ;; we can reschedule.  Then we'll just reschedule in the next
        ;; repeat run.
        (setq carousel-timer (run-at-time next-run 60
                                          #'carousel-kill-buffers))))))

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
