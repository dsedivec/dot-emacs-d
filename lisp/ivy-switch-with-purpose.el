;;; ivy-switch-with-purpose.el --- Ivy switch buffers with Purpose  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>

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

;; Rather than bmag's own ivy-purpose, this uses Ivy's
;; ivy-ignore-buffers special hook to employ Ivy's own buffer
;; switching commands.  Actually goes to some pains to support ido if
;; Ivy is off, and will probably actually work fine if both Ivy and
;; ido are off.

;;; Code:

(require 'ivy)
(require 'window-purpose)

(defvar ivy-swp--limit-to-purpose nil
  "Limits Ivy buffer switching to buffers with the given purpose if non-nil.
Do not give this a global value.  It is meant to be let-bound.")

(defun ivy-swp-purpose-filter (buffer)
  "Ignores buffers without purpose
`ivy-swp--limit-to-purpose', if that variable is
set."
  (when-let ((purpose (and purpose--active-p
                           ivy-swp--limit-to-purpose)))
    (not (equal (purpose-buffer-purpose buffer) purpose))))

(add-hook 'ivy-ignore-buffers #'ivy-swp-purpose-filter)

(defun ivy-swp--call-interactively-with-remapping (cmd)
  (call-interactively (or (command-remapping cmd) cmd)))

(defun ivy-swp--call-regular-or-ido (regular-cmd ido-cmd)
  (ivy-swp--call-interactively-with-remapping
   (if (and (not ivy-mode)
            (boundp 'ido-mode)
            ido-mode)
       ido-cmd
     regular-cmd)))

;; In the spirit of window-purpose itself.  This is arguably
;; abusive!  Reader should use `macrostep-expand', please.
(defmacro ivy-swp--make-switch-overload-command (switch-cmd ido-cmd)
  (declare (indent 2))
  (let* ((ivy-switch-name (symbol-name switch-cmd))
         (regular-or-ido-cmd (intern (format "ivy-swp-friendly-%s"
                                             ivy-switch-name)))
         (without-purpose-cmd (intern (format "ivy-swp-%s-without-purpose"
                                              ivy-switch-name)))
         (with-purpose-cmd (intern (format "ivy-swp-%s-with-purpose"
                                           ivy-switch-name)))
         (overload-cmd (intern (format "ivy-swp-%s-overload"
                                       ivy-switch-name))))
    `(progn
       (defun ,regular-or-ido-cmd ()
         (interactive)
         (ivy-swp--call-regular-or-ido #',switch-cmd #',ido-cmd))

       (defun ,without-purpose-cmd ()
         (interactive)
         (without-purpose
             (ivy-swp--call-interactively-with-remapping #',switch-cmd)))

       (defun ,with-purpose-cmd (&optional purpose)
         (interactive)
         (let ((ivy-swp--limit-to-purpose
                (or purpose (purpose-buffer-purpose (current-buffer)))))
           (ivy-swp--call-interactively-with-remapping #',switch-cmd)))

       (define-purpose-prefix-overload ,overload-cmd
           '(,regular-or-ido-cmd
             ,without-purpose-cmd
             ,with-purpose-cmd)))))

(ivy-swp--make-switch-overload-command
    switch-to-buffer ido-switch-buffer)

(ivy-swp--make-switch-overload-command
    switch-to-buffer-other-window ido-switch-buffer-other-window)

(ivy-swp--make-switch-overload-command
    switch-to-buffer-other-frame ido-switch-buffer-other-frame)

;; Using command remapping here lets me get all of Purpose's own
;; mappings in one fell swoop, because it does define two or three
;; different keys for each of these, if memory serves.
(bind-keys :map purpose-mode-map
           ([remap purpose-switch-buffer-overload]
            . ivy-swp-switch-to-buffer-overload)
           ([remap purpose-switch-buffer-other-window-overload]
            . ivy-swp-switch-to-buffer-other-window-overload)
           ([remap purpose-switch-buffer-other-frame-overload]
            . ivy-swp-switch-to-buffer-other-frame-overload))

(provide 'ivy-switch-with-purpose)
;;; ivy-switch-with-purpose.el ends here
