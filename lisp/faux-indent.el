;;; faux-indent.el --- Indent naïvely based on the last line  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: 

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

;; 

;;; Code:

(require 'cl-lib)

(defun faux-indent-match-last-line-indent ()
  "Indent to the level of the last non-empty line.
This actually copies the indentation from the preceding line, so
if you have mixed tabs/spaces, that mix will be duplicated on the
current line."
  (interactive)
  (let ((starting-point (point))
        ;; Get indentation from previous non-empty line.
        (indent (save-excursion
                  (beginning-of-line)
                  (skip-chars-backward "\r\n \t\f")
                  (back-to-indentation)
                  (buffer-substring (line-beginning-position) (point))))
        go-back-to-indentation)
    (save-excursion
      (back-to-indentation)
      ;; If cursor was inside the indentation of this line (including
      ;; at BOL) the user probably wants to be left at the end of
      ;; their new indentation.
      (setq go-back-to-indentation (<= starting-point (point)))
      (delete-horizontal-space t)
      (insert indent))
    (when go-back-to-indentation
      (back-to-indentation))))

(with-eval-after-load 'electric
  (add-to-list 'electric-indent-functions-without-reindent
               'faux-indent-match-last-line-indent))

(cl-defun faux-indent-add-indentation (&optional (levels 1))
  "Insert LEVELS of indentation on the current line or lines in the region."
  (interactive "p")
  (cond
    ((< levels 0)
     (faux-indent-remove-indentation (- levels)))
    ((zerop levels))
    ((use-region-p)
     (save-excursion
       (let ((end (region-end))
             (indent (if indent-tabs-mode
                         (make-string levels ?\t)
                       (make-string (* levels standard-indent) ?\s))))
         (goto-char (region-beginning))
         (forward-line 0)
         (while (< (point) end)
           (insert indent)
           (forward-line 1))))
     (setq deactivate-mark nil))
    (t
     (let ((start (point))
           go-back-to-indentation)
       (save-excursion
         (back-to-indentation)
         (setq go-back-to-indentation (<= start (point)))
         (if indent-tabs-mode
             (let ((indent-start (point)))
               ;; Delete any spaces after tabs on a single line.  The
               ;; idea is that we're about to insert tabs, and so we
               ;; assume any spaces after tabs in the indentation were
               ;; for alignment, which will now be wrecked by the new
               ;; tab, so do the user a favor and delete the alignment
               ;; spaces.  Maybe this should be conditional on a user
               ;; setting?
               (delete-region indent-start
                              (+ indent-start (skip-chars-backward " ")))
               (insert-char ?\t levels))
           (insert-char ?\s (let* ((num-spaces (* levels standard-indent))
                                   (overshoot (mod (current-column)
                                                   standard-indent)))
                              (- num-spaces overshoot)))))
       (when go-back-to-indentation
         (back-to-indentation))))))

(cl-defun faux-indent-remove-indentation (&optional (levels 1))
  "Remove LEVELS of indentation from the current line or lines in the region."
  (interactive "p")
  (cl-macrolet ((get-indent-regexp ()
                  `(if indent-tabs-mode
                       (format "^\t\\{1,%d\\}" levels)
                     (format "^\\(?: \\{%d\\}\\)\\{1,%d\\}"
                             standard-indent levels))))
    (cond
      ((< levels 0)
       (faux-indent-add-indentation (- levels)))
      ((zerop levels))
      ((use-region-p)
       (save-excursion
         (goto-char (region-beginning))
         (forward-line 0)
         ;; I haven't decided if this function should remove <standard-indent
         ;; number of spaces.  Currently it does not.
         (let ((end (region-end))
               (indent-regexp (get-indent-regexp)))
           (while (re-search-forward indent-regexp end t)
             (replace-match "")
             (forward-line 1))))
       (setq deactivate-mark nil))
      (t
       (save-excursion
         ;; Remove spaces for alignment if present, count as one level
         ;; of indentation.
         (back-to-indentation)
         (let* ((limit (if indent-tabs-mode
                           nil
                         (- (point) (mod (current-column) standard-indent))))
                (spaces-before (- (skip-chars-backward " " limit))))
           (when (> spaces-before 0)
             (delete-char spaces-before)
             (cl-decf levels)))
         (when (> levels 0)
           (forward-line 0)
           (when (looking-at (get-indent-regexp))
             (replace-match ""))))))))

(defun faux-indent-delete-backward-indent-or-char ()
  "Delete a character or level of indentation before point.
Mostly behaves like `delete-backward-char', except if point is at
the end of indentation, in which case it tries to delete some
indentation."
  (interactive)
  (if (or
       ;; Any prefix argument at all means we behave as
       ;; `delete-backward-char'.
       current-prefix-arg
       ;; The following conditions are all copied from
       ;; `delete-backward-char', for the cases where that function
       ;; does something "special".  If `delete-backward-char' would
       ;; do something special, let it do its special behavior in
       ;; preference to ours.
       (and delete-active-region
            (use-region-p))
       (not (or (null overwrite-mode)
                (memq (char-before) '(?\t ?\n))
                (eobp)
                (eq (char-after) ?\n)))
       (/= (point) (save-excursion (back-to-indentation) (point)))
       (bolp))
      ;; Act like `delete-backward-char'.
      (call-interactively #'delete-backward-char)
    ;; Going to remove some indentation.
    (if indent-tabs-mode
        (let ((spaces-before (- (skip-chars-backward " "))))
          (if (> spaces-before 0)
              ;; If using tabs for indentation and there are
              ;; preceding spaces (presumably for alignment) just
              ;; remove those.
              (delete-char spaces-before)
            ;; Presumably this is removing a single tab.
            (delete-char -1)))
      (let* ((align-spaces (mod (current-column) standard-indent))
             (limit (- (point) (if (zerop align-spaces)
                                   standard-indent
                                 align-spaces))))
        (delete-char (- (skip-chars-backward " " limit)))))))

(defvar faux-indent--saved-indent-line-function nil
  "Local value for `indent-line-function' before turning on `faux-indent-mode'.
Will be nil if there was no local value when `faux-indent-mode'
was turned on.")

(make-variable-buffer-local 'faux-indent--saved-indent-line-function)

(defvar faux-indent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'faux-indent-add-indentation)
    (define-key map (kbd "<backtab>") 'faux-indent-remove-indentation)
    (define-key map (kbd "DEL") 'faux-indent-delete-backward-indent-or-char)
    map)
  "Keymap for `faux-indent-mode'.")

;;;###autoload
(define-minor-mode faux-indent-mode
    "Indent new lines based on last line, tab inserts indent always."
  :keymap faux-indent-mode-map
  (if faux-indent-mode
      (progn
        (when (local-variable-p 'indent-line-function)
          (setq faux-indent--saved-indent-line-function indent-line-function))
        (setq-local indent-line-function 'faux-indent-match-last-line-indent))
    (when (eq indent-line-function 'faux-indent-match-last-line-indent)
      (if faux-indent--saved-indent-line-function
          (setq-local indent-line-function
                      faux-indent--saved-indent-line-function)
        (kill-local-variable 'indent-line-function)))
    (kill-local-variable 'faux-indent--saved-indent-line-function)))

(provide 'faux-indent)
;;; faux-indent.el ends here
