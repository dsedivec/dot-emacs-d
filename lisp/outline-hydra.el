;;; outline-hydra.el --- A hydra for outline[-minor]-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: outlines, convenience
;; Version: 1.0
;; Package-Requires: ((emacs "24.1") (hydra "0.15"))

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

;; This is a hydra I defined for myself since I can never remember the
;; `outline-minor-mode' commands when I'm using them in AUCTeX or
;; Markdown or wherever.  Presumably this also works in
;; `outline-mode', though I haven't tested it myself.

;;; Code:

(require 'outline)

(require 'hydra)

;;;###autoload (autoload 'outline-hydra/body "outline-hydra")
(defhydra outline-hydra (:hint nil :foreign-keys run)
  "
^ ^        ^ ^        │         ^ ^   ^ ^        │        _c_hildren   _a_ll
       ^_u_p^         │ ^    shift _U_p        ^ │ show:  _s_ubtree    _e_ntry    b_r_anches
_p_revious _b_ackward │ promote _<_   _>_ demote ├─────────────────────────────────────^^^^^
_n_ext     _f_orward  │ ^    shift _D_own      ^ │ hide:  _S_ubtree    _E_ntry    _L_eaves
^ ^        ^ ^        │ ^          ^ ^         ^ │        suble_V_els  _B_odies   _O_ther
"
  ("n" outline-next-visible-heading)
  ("p" outline-previous-visible-heading)
  ("u" outline-up-heading)
  ("f" outline-forward-same-level)
  ("b" outline-backward-same-level)
  ("U" outline-move-subtree-up)
  ("D" outline-move-subtree-down)
  ("<" outline-promote)
  (">" outline-demote)
  ("s" outline-show-subtree)
  ("c" outline-show-children)
  ("e" outline-show-entry)
  ("r" outline-show-branches)
  ("a" outline-show-all)
  ("S" outline-hide-subtree)
  ("E" outline-hide-entry)
  ("L" outline-hide-leaves)
  ("V" outline-hide-sublevels)
  ("B" outline-hide-body)
  ("O" outline-hide-other)
  ("q" nil))

(provide 'outline-hydra)
;;; outline-hydra.el ends here
