;;; counsel-auto-grep.el --- Search with rg/ag/pt via one command  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: convenience, tools

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

;; Meant to be a wrapper for `counsel-rg', `counsel-ag', `counsel-pt',
;; and other similar functions, using whatever tools are installed.
;;
;; I somewhat regret using "grep" in these names.  Find me something
;; better.
;;
;; Also, note that `counsel-grep' does not at all do the same thing as
;; these other commands, all of which seem to be based on
;; `counsel-ag'.  I imagine there's not a `counsel-rgrep' because grep
;; is too slow, relative to rg/ag/pt, to run over and over every time
;; input changes.
;;
;; You may also like to add this to your init file, to show you in the
;; prompt which directory you're searching from by default:
;;
;;     (ivy-set-prompt 'counsel-ag #'counsel-prompt-function-dir)
;;
;; This file does not bind any keys, that's up to you.

;;; Code:

(eval-when-compile
  (require 'projectile))

(defvar counsel-auto-grep-candidate-commands
  '(("rg" . counsel-rg)
    ("ag" . counsel-ag)
    ("pt" . counsel-pt)))

(defvar counsel-auto-grep-cached-command nil
  "Cached search command.
Call `counsel-auto-grep' with a negative prefix arg to clear this
cached command.")

(defun counsel-auto-grep-set-cached-command ()
  (setq counsel-auto-grep-cached-command
        (catch 'found
          (dolist (pair counsel-auto-grep-candidate-commands)
            (when (executable-find (car pair))
              (throw 'found (cdr pair))))
          (error "no suitable searching program found"))))

;;;###autoload
(defun counsel-auto-grep (&optional initial-input initial-directory
                            extra-args prompt)
  (interactive)
  ;; Using `funcall' instead of `call-interactively' is depending on
  ;; the internals of `counsel-ag', which looks at
  ;; `current-prefix-arg' directly.  Not great, but it works.
  (funcall (if (or (< (prefix-numeric-value current-prefix-arg) 0)
                   (null counsel-auto-grep-cached-command))
               (counsel-auto-grep-set-cached-command)
             counsel-auto-grep-cached-command)
           initial-input initial-directory extra-args prompt))

;;;###autoload
(defun counsel-auto-grep-ask-dir (&optional initial-input extra-args prompt)
  (interactive)
  ;; This is most definitely depending on the internals of
  ;; `counsel-ag'.
  (let ((current-prefix-arg (or current-prefix-arg '(4))))
    ;; Suppressing prompt for args.  Don't forget that you can pass
    ;; args to the search command with a search like:
    ;;
    ;;     --some --args -- pattern_here
    (counsel-auto-grep initial-input nil (or extra-args "") prompt)))

(autoload 'projectile-project-root "projectile")

;;;###autoload
(defun counsel-auto-grep-projectile (&optional initial-input extra-args prompt)
  (interactive)
  (counsel-auto-grep initial-input (projectile-project-root) extra-args
                     prompt))


;;;###autoload
(defun counsel-auto-grep-maybe-projectile (&optional initial-input extra-args
                                             prompt)
  (interactive)
  (if (and (boundp 'projectile-mode) projectile-mode)
      (counsel-auto-grep-projectile initial-input extra-args prompt)
    (counsel-auto-grep initial-input nil extra-args prompt)))

(provide 'counsel-auto-grep)
;;; counsel-auto-grep.el ends here
