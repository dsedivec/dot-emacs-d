;; -*- lexical-binding: t; -*-

;; This uses my preferred indentation for multi-line strings, in
;; particular copying indent from the prior line within the string,
;; rather than removing the prior line's indent on a following line.
;; See the comment, below, for an example.

(require 'el-patch)

(el-patch-feature python)

(with-eval-after-load 'python
  (el-patch-defun python-indent--calculate-indentation ()
    "Internal implementation of `python-indent-calculate-indentation'.
May return an integer for the maximum possible indentation at
current context or a list of integers.  The latter case is only
happening for :at-dedenter-block-start context since the
possibilities can be narrowed to specific indentation points."
    (save-excursion
      (pcase (python-indent-context)
        (`(:no-indent . ,_) (prog-first-column)) ; usually 0
        (el-patch-add
          (`(:inside-string . ,start)
            ;; We're inside a multi-line string, and not on the line the
            ;; string started on (that's a different context).  If we're
            ;; on the line where the string is closed, indent the same as
            ;; the line that started the string.  Otherwise, copy the
            ;; indentation from the previous non-empty line.  This should
            ;; allow us to easily produce strings such as:
            ;;
            ;;     """
            ;;     SELECT
            ;;         foo,
            ;;         bar,
            ;;
            ;;         baz
            ;;     """
            (if (eq (line-number-at-pos (point))
                    ;; Find the line number at the end of the string.
                    (save-excursion
                      (goto-char start)
                      (ignore-errors (forward-sexp))
                      (line-number-at-pos (point))))
                ;; On the line that ends the string.
                (goto-char start)
              ;; Somewhere else inside the string.
              (skip-chars-backward "[:space:]\n"))
            (current-indentation)))
        (`(,(or :after-line
                :after-comment
                (el-patch-remove :inside-string)
                :after-backslash) . ,start)
         ;; Copy previous indentation.
         (goto-char start)
         (current-indentation))
        (`(,(or :inside-paren-at-closing-paren
                :inside-paren-at-closing-nested-paren) . ,start)
         (goto-char (+ 1 start))
         (if (looking-at "[ \t]*\\(?:#\\|$\\)")
             ;; Copy previous indentation.
             (current-indentation)
           ;; Align with opening paren.
           (current-column)))
        (`(:inside-docstring . ,start)
         (let* ((line-indentation (current-indentation))
                (base-indent (progn
                               (goto-char start)
                               (current-indentation))))
           (max line-indentation base-indent)))
        (`(,(or :after-block-start
                :after-backslash-first-line
                :after-backslash-assignment-continuation
                :inside-paren-newline-start) . ,start)
         ;; Add one indentation level.
         (goto-char start)
         (+ (current-indentation) python-indent-offset))
        (`(,(or :inside-paren
                :after-backslash-block-continuation
                :after-backslash-dotted-continuation) . ,start)
         ;; Use the column given by the context.
         (goto-char start)
         (current-column))
        (`(:after-block-end . ,start)
         ;; Subtract one indentation level.
         (goto-char start)
         (- (current-indentation) python-indent-offset))
        (`(:at-dedenter-block-start . ,_)
         ;; List all possible indentation levels from opening blocks.
         (let ((opening-block-start-points
                (python-info-dedenter-opening-block-positions)))
           (if (not opening-block-start-points)
               (prog-first-column) ; if not found default to first column
             (mapcar (lambda (pos)
                       (save-excursion
                         (goto-char pos)
                         (current-indentation)))
                     opening-block-start-points))))
        (`(,(or :inside-paren-newline-start-from-block) . ,start)
         (goto-char start)
         (+ (current-indentation)
            (* python-indent-offset python-indent-def-block-scale))))))

  (el-patch-validate 'python-indent--calculate-indentation 'defun t))
