;;; face-search.el --- Search for faces              -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: convenience matching faces

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

;; TBD

;;; Code:

(eval-when-compile (require 'repeat))
(require 'subr-x)

(defvar face-search-default-faces t
  "List of faces face-search commands will search for by default.
If t, motion will search for any non-nil face property.")

(defvar face-search--last-search nil)

(make-variable-buffer-local 'face-search--last-search)

(defun face-search--start-search (faces &optional backward)
  (unless faces
    (user-error "No faces to search for"))
  (setq face-search--last-search (cons faces backward))
  (face-search-repeat-last-search)
  ;; Avoid letting RET repeat when you just invoked us via
  ;; `execute-executed-command' (i.e. M-x ... RET) or the like.  See
  ;; also https://emacs.stackexchange.com/q/38519.
  (let* ((keys (this-command-keys-vector))
         (last-key (seq-subseq keys -1)))
    (when (eq (key-binding keys) this-command)
      (set-transient-map (let ((map (make-sparse-keymap)))
                           (define-key map last-key
                             'face-search-repeat-last-search)
                           map)
                         t)
      (message "%s to keep searching" (key-description last-key)))))

(defun face-search-repeat-last-search ()
  (interactive)
  (unless face-search--last-search
    (user-error "No last search to repeat"))
  (cl-destructuring-bind (faces . backward) face-search--last-search
    (cl-loop
       with search-func = (if backward
                              #'previous-single-property-change
                            #'next-single-property-change)
       for pos = (point) then next-match
       for next-match = (funcall search-func pos 'face)
       unless next-match do (user-error "No matching faces found")
       for face = (get-text-property next-match 'face)
       when (and face
                 (or (eq faces t)
                     (when (listp face)
                       (seq-some (lambda (elt) (memq elt faces)) face))
                     (memq face faces)))
       do (progn
            (when backward
              (setq next-match
                    (or (previous-single-property-change next-match 'face)
                        (point-min))))
            (goto-char next-match))
       and return t)))

(defvar face-search--get-faces-history)

(defun face-search--get-faces-for-command ()
  (if current-prefix-arg
      (let* ((default-faces-str
              (if (eq face-search-default-faces t)
                  ""
                (string-join (mapcar #'symbol-name face-search-default-faces)
                             " ")))
             (face-list-str (read-from-minibuffer
                             "Face(s) to search for (blank finds any faces): "
                             nil nil nil
                             'face-search--get-faces-history
                             default-faces-str)))
        (or (string-empty-p face-list-str)
            (mapcar #'intern
                    (split-string face-list-str "[[:space:],]" t
                                  "[[:space:]\n]+"))))
    face-search-default-faces))

;;;###autoload
(defun face-search-forward (&optional faces)
  (interactive (list (face-search--get-faces-for-command)))
  (face-search--start-search faces))

;;;###autoload
(defun face-search-backward (&optional faces)
  (interactive (list (face-search--get-faces-for-command)))
  (face-search--start-search faces 'backward))

(provide 'face-search)
;;; face-search.el ends here
