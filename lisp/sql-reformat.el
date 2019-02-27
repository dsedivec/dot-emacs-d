;;; sql-reformat.el --- Minimal SQL reformatter      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: languages

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

;; A very crude sort of SQL reformatting that relies upon inserting
;; newlines at strategic places.  While this doesn't require use of
;; sql-indent, I'm not sure how useful this could possibly be without
;; it, unless you're sitting on your own SQL indentation package---in
;; which case you should share it!

;; To be clear: USE OF SQL-INDENT IS HIGHLY RECOMMENDED.

;;; Code:

(defvar sql-reformat-max-line-length 80
  "Maximum line length before we may insert newlines.  See also
  `sql-reformat-maybe-break-before-regexps' and
  `sql-reformat-maybe-break-after-regexps'.")

(defvar sql-reformat-always-break-before-regexps
  (list (rx symbol-start (or "and") symbol-end))
  "List of regexps.
A match to any regexp in this list will always be preceded by a
newline.  Regexps in this list must not create any capturing
groups.")

(defvar sql-reformat-always-break-after-regexps
  (list (rx symbol-start
            (or "select"
                "insert"
                "update"
                (regexp "delete\\s-+from")
                "from"
                "where"
                (regexp "group\\s-+by")
                "having"
                (regexp "order\\s-+by"))
            symbol-end))
  "List of regexps.
A match to any regexp in this list will always be followed by a
newline.  Regexps in this list must not create any capturing
groups.")

(defvar sql-reformat-maybe-break-before-regexps
  (list (rx symbol-start
            (or "as"
                "on")
            symbol-end))
  "List of regexps.
A match to any regexp in this list will be preceded by a newline
if the length of the line after this point would exceed
`sql-reformat-max-line-length'.  Regexps in this list must not
create any capturing groups.")

(defvar sql-reformat-maybe-break-after-regexps
  '(",")
  "List of regexps.
A match to any regexp in this list will be followed by a newline
if the length of the line after this point would exceed
`sql-reformat-max-line-length'.  Regexps in this list must not
create any capturing groups.")

(defun sql-reformat--get-regexp-from-lists (before after)
  "Return combined regexp with BEFORE in group 1, AFTER in group 2."
  (rx-to-string
   `(or (group (or ,@(mapcar (apply-partially #'list 'regexp) before)))
        (group (or ,@(mapcar (apply-partially #'list 'regexp) after))))))

(defun sql-reformat--line-too-long-p (break-regexp bound)
  "Return TRUE if line at next match for BREAK-REGEXP is too long.
Never searches past BOUND.  Returns NIL if BREAK-REGEXP is not
found between point at BOUND.

This function will reindent the current line and it may move
point.  The line is reindented in order to determine line length
more accurately."
  (setq bound (if (markerp bound) bound (copy-marker bound)))
  ;; We already reindented in `sql-reformat-region', but since
  ;; we're inserting newlines, I think it's possible that the line
  ;; we're looking at now may need to be reindented again after we
  ;; inserted a newline, so we're reindenting just to be safe, I
  ;; guess.
  ;;
  ;; Cannot depend on return value of `indent-according-to-mode',
  ;; hence the `progn'.
  (and (progn
         (indent-according-to-mode)
         (re-search-forward (format "\\(?:\n\\|%s\\)" break-regexp) bound t))
       (goto-char (match-beginning 0))
       (>= (current-column)
           sql-reformat-max-line-length)))

;;;###autoload
(defun sql-reformat-region (region-start region-end
                            &optional remove-newlines)
  "Reformat SQL between REGION-START and REGION-END.
If called with a prefix argument, or if REMOVE-NEWLINES is
non-nil, all newlines will be removed from the region before
reformatting."
  (interactive "rP")
  ;; We're going to be changing the buffer contents, so convert these
  ;; to markers.
  (setq region-start (copy-marker region-start)
        region-end (copy-marker region-end))
  (save-excursion
    (when remove-newlines
      (goto-char region-start)
      (while (re-search-forward "\n" region-end t)
        (replace-match "")))
    (let ((case-fold-search t)
          (always-break-regexp (sql-reformat--get-regexp-from-lists
                                sql-reformat-always-break-before-regexps
                                sql-reformat-always-break-after-regexps))
          (maybe-break-regexp (sql-reformat--get-regexp-from-lists
                               sql-reformat-maybe-break-before-regexps
                               sql-reformat-maybe-break-after-regexps)))
      ;; First pass: insert mandatory line breaks
      (goto-char region-start)
      (while (re-search-forward always-break-regexp region-end t)
        (cond
          ((match-beginning 1)
           (let ((match-start (match-beginning 1))
                 (match-end (match-end 1)))
             (unless (save-excursion
                       (goto-char match-start)
                       (skip-syntax-backward "-" region-start)
                       (= (char-before) ?\n))
               (goto-char match-start)
               (insert "\n")
               (goto-char (1+ match-end)))))
          ((match-beginning 2)
           (unless (looking-at-p "\\s-*\n")
             (insert "\n")))
          (t (error "Should not get here, match data: %S" (match-data)))))
      ;; Intermission: reindent the region before inserting optional
      ;; breaks, in order to hopefully make "is this line too long?"
      ;; more reliable.
      (indent-region region-start region-end)
      ;; Second pass: insert optional line breaks for lines that are
      ;; too long
      (goto-char region-start)
      (while (re-search-forward maybe-break-regexp region-end t)
        (cond
          ((match-beginning 1)
           ;; Put both of these in a marker since
           ;; `sql-reformat--line-too-long-p' might shift these
           ;; positions.
           (let ((match-start (copy-marker (match-beginning 1)))
                 (match-end (copy-marker (match-end 1))))
             (when (and
                    ;; Ensure we're not preceded by a newline already.
                    (goto-char match-start)
                    (skip-syntax-backward "-" region-start)
                    (/= (char-before) ?\n)
                    (goto-char match-end)
                    (sql-reformat--line-too-long-p maybe-break-regexp
                                                   region-end))
               ;; Line is too long, break.
               (goto-char match-start)
               (insert "\n"))
             (goto-char match-end)))
          ((match-beginning 2)
           (when (and
                  (not (looking-at-p "\\s-*\n"))
                  (save-excursion
                    (sql-reformat--line-too-long-p maybe-break-regexp
                                                   region-end)))
             (insert "\n")))
          (t (error "Should not get here, match data: %S" (match-data)))))
      ;; Finally, clean up trailing white space and indent the region
      ;; again, the latter of which may be necessary after inserting
      ;; newlines in the second pass.  I think.
      (delete-trailing-whitespace region-start region-end)
      (indent-region region-start region-end))))

(provide 'sql-reformat)
;;; sql-reformat.el ends here
