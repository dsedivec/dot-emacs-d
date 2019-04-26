;;; indent-elisp-like-common-lisp.el --- Indent Emacs Lisp with `common-lisp-indent-function'  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: lisp

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

;; I generally prefer how `common-lisp-indent-function' indents, and I
;; want to use it in elisp.  Doing so requires a few accommodations.

;;; Code:

(eval-when-compile (require 'subr-x))

(defun my:use-common-lisp-indent ()
  (setq-local lisp-indent-function #'common-lisp-indent-function))

(add-hook 'emacs-lisp-mode-hook #'my:use-common-lisp-indent)

;; For some reason, `lisp-indent-function' special-cases forms that
;; begin with "def".  `common-lisp-indent-function' doesn't, but we
;; can teach it how to indent the ones we care about easily enough.

(put 'define-derived-mode 'lisp-indent-function 3)

;; This bit applies to both Emacs's own cl-indent.el and also SLIME's
;; slime-cl-indent.el, which is an old fork of Emacs's cl-indent.el.

(defun common-lisp-to-lisp-indent-function-adapter
    (_path state indent-point _sexp-column _normal-indent)
  "Adapter for `common-lisp-indent-call-method' to `lisp-indent-function'.
Useful when you want to make a symbol, like pcase, indented by
`lisp-indent-function' rather than `common-lisp-indent-function'."
  ;; `common-lisp-indent-function' (indirectly) has moved point, and
  ;; `lisp-indent-function' does *not* like that, it expects point
  ;; to be where `calculate-lisp-indent' put it.  Try to move point
  ;; back to where `calculate-lisp-indent' left it before calling
  ;; `common-lisp-indent-function'.  I'm not entirely sure how to
  ;; correctly accomplish this.  My candidates right now are
  ;; `calculate-lisp-indent-last-sexp', an undocumented but
  ;; dynamically-bound variable set by (you guessed it)
  ;; `calculate-lisp-function', or the third element of the parse
  ;; state in state.  I'm going with #2, even though I have no idea
  ;; if it's the right thing to do.  Works for a pcase test case (point at |):
  ;;
  ;;     (pcase foo
  ;;     | (`x (bar (baz)
  ;;      ())))
  (goto-char (nth 2 state))
  (lisp-indent-function indent-point state))

;; Use Elisp indenter for all the pcase functions.
(dolist (sym '(pcase pcase-lambda pcase-defmacro pcase-let pcase-let*
               pcase-dolist))
  (put sym 'common-lisp-indent-function-for-elisp
       'common-lisp-to-lisp-indent-function-adapter))

;; And these bits are only if you're using SLIME's slime-cl-indent.el.

(defvar common-lisp-style)
(declare-function common-lisp-active-style-methods "slime-cl-indent")
(declare-function common-lisp-get-indentation "slime-cl-indent")

;; Note that Emacs's own cl-indent.el does not define this function,
;; so we don't need to condition this advice on e.g.
;; (featurep 'slime-cl-indent).
(define-advice common-lisp-get-indentation
    (:around (orig-fun name &rest args) my:read-elisp-indents)
  (if (derived-mode-p 'emacs-lisp-mode)
      (let ((unprefixed-name (and (string-match-p "^cl-" (symbol-name name))
                                  ;; intern-soft because if the symbol
                                  ;; hasn't been interned, it
                                  ;; presumably doesn't have any
                                  ;; properties for us.  (Really I'm
                                  ;; copying Emacs's cl-indent.el and
                                  ;; guessing at its reasoning.)
                                  (intern-soft (substring (symbol-name name)
                                                          3)))))
        (or
         ;; Test the style first, just like
         ;; `common-lisp-get-indentation' does *before checking the
         ;; property*.  This way your style can still override the
         ;; common-lisp-indent-function-for-elisp property.
         (when common-lisp-style
           (let ((methods (common-lisp-active-style-methods)))
             (or (gethash name methods)
                 ;; Going to avoid looking up nil in the hash table,
                 ;; out of an abundance of caution.
                 (when unprefixed-name
                   (gethash unprefixed-name methods)))))
         ;; Now check the common-lisp-indent-function-for-elisp
         ;; property, in preference to the common-lisp-indent-function
         ;; property that is normally checked in slime-cl-indent.
         (when-let ((method (or (get name
                                     'common-lisp-indent-function-for-elisp)
                                ;; Interestingly, (get) doesn't seem
                                ;; to care if it's called with nil as
                                ;; its first arg.
                                (get unprefixed-name
                                     'common-lisp-indent-function-for-elisp))))
           ;; Must reimplement the (as ...) indent specification,
           ;; unfortunately.
           (if (and (consp method) (eq (car method) 'as))
               (common-lisp-get-indentation (cadr method))
             method))
         ;; OK, none of our special sauce made any difference, let the
         ;; advised function handle it from here (including
         ;; re-checking the hash table--O, the CPU cycles), except do
         ;; prefer unprefixed-name in this case.  Honestly, this is
         ;; almost certainly going to return nil, right?  Or crash...
         (apply orig-fun (or unprefixed-name name) args)))
    ;; Not Emacs Lisp, behave normally.
    (apply orig-fun name args)))

;;; indent-elisp-like-common-lisp.el ends here
