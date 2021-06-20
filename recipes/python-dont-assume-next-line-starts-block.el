;; -*- lexical-binding: t; -*-

;; Don't assume next line starts a block
;;
;; May fix https://debbugs.gnu.org/cgi/bugreport.cgi?bug=42513.
;;
;; Test case:
;;
;;     def f():
;;         if (
;;             True
;;         ):
;;             pass
;;             else:  # <- indent here
;;
;; Without this patch, that last line gets reindented to column 0
;; rather than column 4.

(require 'el-patch)

(el-patch-feature python)

(with-eval-after-load 'python
  (el-patch-defun python-info-dedenter-opening-block-positions ()
    "Return points of blocks the current line may close sorted by closer.
Returns nil if point is not on a dedenter statement or no opening
block can be detected.  The latter case meaning current file is
likely an invalid python file."
    (save-excursion
      (let ((dedenter-pos (python-info-dedenter-statement-p)))
        (when dedenter-pos
          (goto-char dedenter-pos)
          (let* ((cur-line (line-beginning-position))
                 (pairs '(("elif" "elif" "if")
                          ("else" "if" "elif" "except" "for" "while")
                          ("except" "except" "try")
                          ("finally" "else" "except" "try")))
                 (dedenter (match-string-no-properties 0))
                 (possible-opening-blocks (cdr (assoc-string dedenter pairs)))
                 (collected-indentations)
                 (opening-blocks))
            (catch 'exit
              (while (python-nav--syntactically
                      (lambda ()
                        (re-search-backward (python-rx block-start) nil t))
                      #'<)
                (let ((indentation (current-indentation)))
                  (when (and (not (memq indentation collected-indentations))
                             (or (not collected-indentations)
                                 (< indentation (apply #'min collected-indentations)))
                             ;; There must be no line with indentation
                             ;; smaller than `indentation' (except for
                             ;; blank lines) between the found opening
                             ;; block and the current line, otherwise it
                             ;; is not an opening block.
                             (save-excursion
                               (el-patch-add (python-nav-end-of-statement))
                               (forward-line)
                               (let ((no-back-indent t))
                                 (save-match-data
                                   (while (and (< (point) cur-line)
                                               (setq no-back-indent
                                                     (or (> (current-indentation) indentation)
                                                         (python-info-current-line-empty-p))))
                                     (forward-line)))
                                 no-back-indent)))
                    (setq collected-indentations
                          (cons indentation collected-indentations))
                    (when (member (match-string-no-properties 0)
                                  possible-opening-blocks)
                      (setq opening-blocks (cons (point) opening-blocks))))
                  (when (zerop indentation)
                    (throw 'exit nil)))))
            ;; sort by closer
            (nreverse opening-blocks))))))

  (el-patch-validate 'python-info-dedenter-opening-block-positions 'defun t))
