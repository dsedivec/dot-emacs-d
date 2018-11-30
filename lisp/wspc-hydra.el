;;; wspc-hydra.el --- Hydra for whitespace-mode      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: convenience
;; Package-Requires: (hydra)

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

;; `wspc-hydra/body' provides a nice way to toggle whitespace-mode
;; settings.

;;; Code:

(require 'cl-lib)
(require 'hydra)

(defvar wspc-hydra-buffer-local-whitespace-style nil
  "When true, `whitespace-style' will be made buffer local if we change it.
This applies to changing individual options or to applying styles.

This is particularly useful if you toggle whitespace-mode on/off
in the buffer and want your changes to options, or styles, to
persist.  Or if you use Spacemacs, which seems to interfere with
my hooks that apply a whitespace style, which is blown away by
default when Spacemacs apparently toggles whitespace-mode
off/on.")

(define-advice whitespace-toggle-options
    (:after (&rest _) wspc-hydra-make-changes-to-local-whitespace-style)
  (when wspc-hydra-buffer-local-whitespace-style
    (setq-local whitespace-style whitespace-active-style)))

(defun wspc-hydra--option-p (option)
  (memq option (if whitespace-mode whitespace-active-style whitespace-style)))

(defvar wspc-hydra--indicators '(" " . "*"))

(defun wspc-hydra--indicator (option &optional if-on if-off)
  (if (wspc-hydra--option-p option)
      (or if-on (cdr wspc-hydra--indicators))
    (or if-off (car wspc-hydra--indicators))))

(defun wspc-hydra--face-mark-option-info (face-option)
  (let ((mark-option (pcase face-option
                        ('tabs 'tab-mark)
                        ('spaces 'space-mark)
                        ('newline 'newline-mark)
                        (_ (error "unknown face-option: %S" face-option)))))
    (list mark-option
          (wspc-hydra--option-p face-option)
          (wspc-hydra--option-p mark-option))))

(defun wspc-hydra--face-mark-indicator (face-option)
  (cl-destructuring-bind (_ face-p mark-p)
      (wspc-hydra--face-mark-option-info face-option)
    (concat (if face-p "f" " ") (if mark-p "m" " "))))

(defvar wspc-hydra--cycle-together t)

(defun wspc-hydra--cycle-face-mark (face-option)
  (cl-destructuring-bind (mark-option face-p mark-p)
      (wspc-hydra--face-mark-option-info face-option)
    (whitespace-toggle-options
     (if wspc-hydra--cycle-together
         (cond ((eq (null face-p) (null mark-p))
                (list face-option mark-option))
               (face-p mark-option)
               (mark-p face-option))
       (pcase (cons (and face-p t) (and mark-p t))
         (`(nil . nil) face-option)
         (`(t . nil)   (list face-option mark-option))
         (`(nil . t)   face-option)
         (`(t . t)     (list face-option mark-option))
         (_ (error "should not get here")))))))

(defvar wspc-hydra-lines-tail t)

(defvar wspc-hydra-style-alist
  '((default . (face tabs spaces trailing lines space-before-tab newline
                indentation empty space-after-tab
                space-mark tab-mark newline-mark))
    (warn-white-space . (face trailing lines-tail empty space-before-tab
                         indentation))))

;;;###autoload
(defun wspc-hydra-apply-style (style-name)
  "Apply whitespace-mode style named STYLE-NAME.
Will turn on `whitespace-mode' if necessary."
  (interactive
   (list
    (intern (completing-read "Style: "
                             (mapcar #'car wspc-hydra-style-alist)))))
  (let ((style (or (assq style-name wspc-hydra-style-alist)
                   (error "unknown style %S" style-name))))
    (whitespace-mode -1)
    (let ((whitespace-style (cdr style)))
      (whitespace-mode 1))
    (when wspc-hydra-buffer-local-whitespace-style
      (setq-local whitespace-style whitespace-active-style))))

;;;###autoload (autoload 'wspc-hydra/body "wspc-hydra" nil t)
(defhydra wspc-hydra ()
  "
_w_hitespace mode: ?w?
_c_ycle face/mark ?c?^^^^^^^               ^^^^^_f_: [?f?] faces
_t_: [?t?] tabs      _l_: [?l?^^^^] long lines  _i_: [?i?] indentation
_s_: [?s?] spaces    _T_: [?T?] trailing        _b_: [?b?] spc before tab
_n_: [?n?] newlines  _e_: [?e?] empty lines     _a_: [?a?] spc after tab
"
  ("w" whitespace-mode (if whitespace-mode "on" "off"))
  ("c"
   (setq wspc-hydra--cycle-together (not wspc-hydra--cycle-together))
   (if wspc-hydra--cycle-together "together  " "separately"))
  ("f" (whitespace-toggle-options 'face) (wspc-hydra--indicator 'face))
  ("t"
   (wspc-hydra--cycle-face-mark 'tabs)
   (wspc-hydra--face-mark-indicator 'tabs))
  ("s"
   (wspc-hydra--cycle-face-mark 'spaces)
   (wspc-hydra--face-mark-indicator 'spaces))
  ("n"
   (wspc-hydra--cycle-face-mark 'newline)
   (wspc-hydra--face-mark-indicator 'newline))
  ("l"
   (progn
     (when current-prefix-arg
       (setq wspc-hydra-lines-tail (not wspc-hydra-lines-tail)))
     (cl-destructuring-bind (this-opt . other-opt)
         (if wspc-hydra-lines-tail
             '(lines-tail . lines)
           '(lines . lines-tail))
       (if (not current-prefix-arg)
           (whitespace-toggle-options
            (if (wspc-hydra--option-p other-opt)
                (list this-opt other-opt)
              this-opt))
         (let ((this-opt-p (wspc-hydra--option-p this-opt))
               (other-opt-p (wspc-hydra--option-p other-opt)))
           (unless (and this-opt-p (not other-opt-p))
             (whitespace-toggle-options
              (cond
                ((not (or this-opt-p other-opt-p))
                 this-opt)
                ((and (not this-opt-p) other-opt-p)
                 (list this-opt other-opt))
                ((and this-opt-p other-opt-p)
                 other-opt-p))))))))
   (let ((lines-p (wspc-hydra--option-p 'lines))
         (tails-p (wspc-hydra--option-p 'lines-tail)))
     (cond
       ((and lines-p tails-p) "both!")
       (lines-p "lines")
       (tails-p "tail ")
       (t "     "))))
  ("T" (whitespace-toggle-options 'trailing) (wspc-hydra--indicator 'trailing))
  ("e" (whitespace-toggle-options 'empty) (wspc-hydra--indicator 'empty))
  ("i" (whitespace-toggle-options 'indentation)
       (wspc-hydra--indicator 'indentation))
  ("b" (whitespace-toggle-options 'space-before-tab)
       (wspc-hydra--indicator 'space-before-tab))
  ("a" (whitespace-toggle-options 'space-after-tab)
       (wspc-hydra--indicator 'space-after-tab))
  ("r" (whitespace-toggle-options 'whitespace-style) "restore")
  ("y" wspc-hydra-apply-style "style")
  ("g"
   (progn
     (whitespace-mode -1)
     (whitespace-mode 1))
   "restart mode")
  ("q" nil "quit" :exit t))

(provide 'wspc-hydra)
;;; wspc-hydra.el ends here
