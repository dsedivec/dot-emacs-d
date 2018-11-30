;;; lisp-comment-dwim.el --- DWIM commenting for lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: lisp
;; Package-Requires: (smartparens)

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

;; The desire for this is obvious once you use comment-dwim-2, but the
;; implementation ideas come from a comment by Fuco1 (author of
;; smartparens AFAIK) on
;; http://endlessparentheses.com/a-comment-or-uncomment-sexp-command.html.
;; Artur's code there breaks on uncommenting when there's a nearby
;; string with ";" in it, e.g. (untested simplified test case for the
;; problem I experienced):
;;
;;     (define-key blah "M-;" #'foo)
;;
;;     ;; (foo)
;;
;; Fuco1 commented about an alternative, simpler solution using
;; `sp-get-thing', and that's what I based this code on.
;;
;; A substitute for `sp-get-sexp' would be nice so that I don't have
;; to install both paredit (which I prefer over smartparens for lisp
;; editing) and smartparens.
;;
;; Also, if you want to lift this, make sure you configure ' (single
;; quote/apostrophe character) for lisp modes in smartparens, like by
;; way of (require 'smartparens-config).
;;
;; Note: this function doesn't work real well without non-nil
;; `comment-empty-lines'.  Blank lines make `sp-get-thing' stop
;; searching for an sexp in a comment.

;;; Code:

;; smartparens doesn't autoload these, but we don't want to suck in
;; smartparens before it's needed, so set it up.
(autoload 'sp-get-thing "smartparens")
(autoload 'sp-split-sexp "smartparens")

(defun lisp-comment-dwim-sexp ()
  "Comment sexp after point, or uncomment sexp in current comment."
  (interactive)
  (let* ((state (syntax-ppss))
         (uncomment-start
          (cond ((nth 4 state)
                 ;; We're in a comment, return the start of that
                 ;; comment.
                 (nth 8 state))
                ((nth 3 state)
                 ;; We're a string, so definitely not in a comment.
                 ;; comment-forward doesn't seem to know about
                 ;; strings, so this test is necessary so that it
                 ;; doesn't get fooled by stuff like "M-;".
                 nil)
                (t
                 ;; Use comment-forward to move past what may or may
                 ;; not be a comment that we're looking at.  Why not
                 ;; just use "\s-*;" or skip-syntax-forward?  I
                 ;; don't know, because all the cool kids are using
                 ;; comment-forward.
                 (let ((start-of-next-line (1+ (line-end-position)))
                       (end-of-comment (save-excursion
                                         (and (comment-forward)
                                              (point)))))
                   (when (and end-of-comment
                              ;; If we were looking at just white
                              ;; space until EOL, and next line is a
                              ;; comment, comment-forward will have
                              ;; grabbed too much.  We just really
                              ;; wanted to know if there was a
                              ;; comment on our current line or not.
                              (<= end-of-comment start-of-next-line))
                     (point)))))))
    (if uncomment-start
        ;; Uncomment an sexp
        (save-excursion
          (let (sp-thing uncomment-end line-end recomment-end
                         maybe-split-at-uncomment-start)
            (goto-char uncomment-start)
            (setq line-end (line-end-position))
            ;; Skip past comment start.
            (comment-search-forward line-end)
            (while (and (< (point) line-end)
                        (setq sp-thing (sp-get-thing))
                        ;; Make sure we don't accidentally skip to
                        ;; the next thing on a following line.
                        (< (plist-get sp-thing :beg) line-end))
              (let ((thing-beg (plist-get sp-thing :beg)))
                (when (< thing-beg uncomment-start)
                  ;; Oops, it seems we need to move uncomment-start
                  ;; backwards.
                  (setq uncomment-start thing-beg
                        maybe-split-at-uncomment-start t)))
              (goto-char (plist-get sp-thing :end)))
            ;; uncomment-end should now be the end of any sexps we
            ;; found on the line on which we started.
            (setq uncomment-end (copy-marker (max (point) line-end))
                  line-end (line-end-position))
            ;; Now go looking for any sexps which are on the same
            ;; line as uncomment-end, but which did not start on the
            ;; same line as uncomment-start.
            (while (and (< (point) line-end)
                        (setq sp-thing (sp-get-thing))
                        (< (plist-get sp-thing :beg) line-end))
              (goto-char (plist-get sp-thing :end))
              (setq line-end (line-end-position)))
            (setq recomment-end (point-marker))
            (when maybe-split-at-uncomment-start
              ;; We had to go backwards from where we started to
              ;; find the start of the commented sexp.  Now decide
              ;; whether we're going to uncomment that whole line,
              ;; or split the comment.
              (goto-char uncomment-start)
              (beginning-of-line)
              (comment-search-forward nil t)
              (if (= (point) uncomment-start)
                  ;; Our commented sexp starts the comment on this
                  ;; line, so we can uncomment on this line.
                  (setq uncomment-start (nth 8 (syntax-ppss)))
                ;; Our commented sexp is only part of the comment on
                ;; this line, so we need to insert a newline,
                ;; leaving the comment preceding comment-start
                ;; as-is.  Note that comment-region doesn't care if
                ;; some of the lines in the region aren't commented,
                ;; like the line we're about to create.
                (goto-char uncomment-start)
                (insert "\n")))
            ;; We're going to want to indent uncomment-start through
            ;; uncomment-end in a second.  Convert uncomment-start
            ;; to a marker so it moves around if needed when
            ;; uncomment-region start uncommenting things.
            (setq uncomment-start (copy-marker uncomment-start))
            (uncomment-region uncomment-start recomment-end)
            ;; We may have uncommented more than we needed when an
            ;; sexp that starts on the same line as uncomment-start
            ;; ends on the same line that another sexp begins.  In
            ;; this case, recomment the other bits.
            (when (> recomment-end uncomment-end)
              (goto-char uncomment-end)
              (insert "\n")
              (comment-region (point) recomment-end))
            ;; Reindent our newly-uncommented region.  Not strictly
            ;; necessary but I quite like the results.
            (indent-region uncomment-start uncomment-end)))

      ;; Comment an sexp
      (when (nth 3 (syntax-ppss))
        ;; We're in a string, split it, and let smartparens move
        ;; point.
        (sp-split-sexp nil))
      (save-excursion
        (cond
          ((eolp)
           ;; When run at end of line, we comment the whole line.
           (back-to-indentation))
          ((looking-at-p "\\sw\\|\\s_\\|\\s'")
           ;; We're possibly in the middle of a symbol/number, move to
           ;; its start.  We usually don't want to comment out half of
           ;; a symbol.
           (skip-syntax-backward "w_'")))
        (let ((start (point))
              (line-end (line-end-position)))
          ;; Skip past white space and sexps on this line.  We stop
          ;; if we hit a scan-error due to unbalanced sexp.
          (condition-case _
              (while (< (point) line-end)
                (or (plusp (skip-syntax-forward " " line-end))
                    (forward-sexp 1)))
            (scan-error))
          (comment-region start (point)))))))

;; This is a bit inspired by `paredit-comment-dwim'.
;;;###autoload
(defun lisp-comment-dwim (&optional arg)
  "Try to DWIM WRT comments."
  (interactive "P")
  (cond
    ((region-active-p)
     (comment-or-uncomment-region (region-beginning) (region-end) arg))
    ((save-excursion
       (beginning-of-line)
       (looking-at-p "\\s-*$"))
     (delete-horizontal-space)
     (insert ";; ")
     (indent-according-to-mode))
    (t
     (lisp-comment-dwim-sexp))))

(provide 'lisp-comment-dwim)
;;; lisp-comment-dwim.el ends here
