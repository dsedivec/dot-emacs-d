;;; counsel-advice-remove.el --- Remove advice interactively  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: lisp, convenience
;; Version: 1.0
;; Package-Requires: ((emacs "24.4") (ivy "0"))

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

;; There's already an interactive `ad-remove-advice' for "old" advice,
;; but "new" advice (nadvice.el) apparently doesn't have a
;; counterpart.  Here we define one using Ivy.

;; It is unclear to me whether this command can also find and remove
;; advice added by advice.el.

;;; Code:

(require 'nadvice)
(require 'ivy)

(defun counsel-advice-remove--advised-p (symbol)
  "Return non-nil if SYMBOL has (new-style) advice."
  (advice--p (advice--symbol-function symbol)))

(defvar counsel-advice-remove-history nil
  "Ivy history for `counsel-advice-remove'.")

;;;###autoload
(defun counsel-advice-remove (symbol function)
  "Remove advice FUNCTION from SYMBOL.
Just like built-in function `advice-remove', but this is
interactive and uses Ivy.  Very useful when
writing/debugging/renaming advice in Elisp."
  (interactive
   (let* ((symbol (intern
                   (ivy-read "Remove advice from: "
                             obarray
                             :predicate #'counsel-advice-remove--advised-p
                             :require-match t
                             :preselect (ivy-thing-at-point)
                             :history 'counsel-advice-remove-history
                             :caller 'counsel-advice-remove)))
          advice)
     (advice-mapc (lambda (function props)
                    (push (or (alist-get 'name props) function) advice))
                  symbol)
     (list symbol
           (intern (ivy-read "Advice to remove: "
                             advice
                             :require-match t
                             :caller 'counsel-advice-remove)))))
  (message "Removing advice %S from %S" function symbol)
  (advice-remove symbol function))

(provide 'counsel-advice-remove)
;;; counsel-advice-remove.el ends here
