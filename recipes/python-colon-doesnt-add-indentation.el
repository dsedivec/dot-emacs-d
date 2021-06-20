;; -*- lexical-binding: t; -*-

;; Don't *add* indentation when typing ":"
;;
;; The code here seems to say that it wants to "[j]ust re-indent
;; dedenters", so let's make sure we're doing just that.
;;
;; This was bugging me a lot because I often have already un-indented
;; a line such as `elif ...` and then I end it with `:`, only to have
;; my indentation undone.

(require 'el-patch)

(el-patch-feature python)

(with-eval-after-load 'python
  (el-patch-defun python-indent-post-self-insert-function ()
    "Adjust indentation after insertion of some characters.
This function is intended to be added to `post-self-insert-hook.'
If a line renders a paren alone, after adding a char before it,
the line will be re-indented automatically if needed."
    (when (and electric-indent-mode
               (eq (char-before) last-command-event)
               (not (python-syntax-context 'string))
               (save-excursion
                 (beginning-of-line)
                 (not (python-syntax-context 'string (syntax-ppss)))))
      (cond
        ;; Electric indent inside parens
        ((and
          (not (bolp))
          (let ((paren-start (python-syntax-context 'paren)))
            ;; Check that point is inside parens.
            (when paren-start
              (not
               ;; Filter the case where input is happening in the same
               ;; line where the open paren is.
               (= (line-number-at-pos)
                  (line-number-at-pos paren-start)))))
          ;; When content has been added before the closing paren or a
          ;; comma has been inserted, it's ok to do the trick.
          (or
           (memq (char-after) '(?\) ?\] ?\}))
           (eq (char-before) ?,)))
         (save-excursion
           (goto-char (line-beginning-position))
           (let ((indentation (python-indent-calculate-indentation)))
             (when (and (numberp indentation) (< (current-indentation) indentation))
               (indent-line-to indentation)))))
        ;; Electric colon
        ((and (eq ?: last-command-event)
              (memq ?: electric-indent-chars)
              (not current-prefix-arg)
              ;; Trigger electric colon only at end of line
              (eolp)
              ;; Avoid re-indenting on extra colon
              (not (equal ?: (char-before (1- (point)))))
              (not (python-syntax-comment-or-string-p)))
         ;; Just re-indent dedenters
         (let ((dedenter-pos (python-info-dedenter-statement-p)))
           (when (el-patch-wrap 1 1
                   (and dedenter-pos
                        (<= (python-indent-calculate-indentation)
                            (current-indentation))))
             (let ((start (copy-marker dedenter-pos))
                   (end (point-marker)))
               (save-excursion
                 (goto-char start)
                 (python-indent-line)
                 (unless (= (line-number-at-pos start)
                            (line-number-at-pos end))
                   ;; Reindent region if this is a multiline statement
                   (python-indent-region start end))))))))))

  (el-patch-validate 'python-indent-post-self-insert-function 'defun t))
