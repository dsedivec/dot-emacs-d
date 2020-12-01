;;; window-hydra.el --- Hydra for window management  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Version: 1
;; Package-Requires: ((emacs "24") (ace-window "0.7") (transpose-frame "0.1.0") (hydra "0.15.0"))
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

;; Simple hydra for window management.  Meant to be used with
;; ace-window installed.

;;; Code:

(eval-when-compile
  (require 'windmove)
  (require 'winner)

  (require 'ace-window)
  (require 'hydra))

(autoload 'aw-swap-window "ace-window")
(autoload 'windmove-find-other-window "windmove")
(autoload 'winner-undo "winner" nil t)
(autoload 'winner-redo "winner" nil t)

;;;###autoload (autoload 'window-hydra/body "window-hydra" nil t)
(defhydra window-hydra (:hint nil)
  "
^^^^switch or MOVE^^^^   _o_ther window         _1_ one window      winner:
^^^^    window    ^^^^   s_w_ap windows         _2_ split window    _←_ undo
     ^^_j_/_J_^^         _b_alance windows      _0_ delete window   _→_ redo
 _h_/_H_     _l_/_L_     _\\_ transpose frame
     ^^_k_/_K_^^         CCW _[_ _]_ CW rotate frame                _q_uit
  "
  ("j" (windmove-up))
  ("h" (windmove-left))
  ("l" (windmove-right))
  ("k" (windmove-down))
  ("J" (aw-swap-window (windmove-find-other-window 'up)))
  ("H" (aw-swap-window (windmove-find-other-window 'left)))
  ("L" (aw-swap-window (windmove-find-other-window 'right)))
  ("K" (aw-swap-window (windmove-find-other-window 'down)))
  ("1" delete-other-windows)
  ("2" split-window-below)
  ("b" balance-windows)
  ("o" ace-window)
  ("\\" transpose-frame)
  ("[" rotate-frame-anticlockwise)
  ("]" rotate-frame-clockwise)
  ("w" ace-swap-window)
  ("<left>" winner-undo)
  ("0" delete-window)
  ("<right>" winner-redo)
  ("q" nil))

(provide 'window-hydra)
;;; window-hydra.el ends here
