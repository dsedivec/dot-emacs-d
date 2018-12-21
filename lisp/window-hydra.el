;;; window-hydra.el --- Hydra for window management  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Version: 1
;; Package-Requires: ((emacs "24") (ace-window "0.7"))
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

(require 'hydra)
(require 'windmove)
(require 'ace-window)

;;;###autoload (autoload 'window-hydra/body "window-hydra" nil t)
(defhydra window-hydra (:hint nil)
  "
  ^_j_^                  _1_ single window   _2_^ split window      _b_alance windows
_h_   _l_  move buffer   _o_ther window      _\\_ transpose frame   s_w_ap window
  ^_k_^                  _←_ winner-undo     _0_^ delete window     _→_ winner redo
"
  ("j" (aw-swap-window (windmove-find-other-window 'up)))
  ("h" (aw-swap-window (windmove-find-other-window 'left)))
  ("l" (aw-swap-window (windmove-find-other-window 'right)))
  ("k" (aw-swap-window (windmove-find-other-window 'down)))
  ("1" delete-other-windows)
  ("2" split-window-below)
  ("b" balance-windows)
  ("o" ace-window)
  ("\\" transpose-frame)
  ("w" ace-swap-window)
  ("<left>" winner-undo)
  ("0" delete-window)
  ("<right>" winner-redo)
  ("q" nil))

(provide 'window-hydra)
;;; window-hydra.el ends here
