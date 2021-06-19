;; -*- lexical-binding: t; -*-

;; This is a patched version of `markdown-calc-indents' that does
;; better at getting the indent from a prior line, so that your next
;; line can use that indent level.  I wanted this after working with
;; multi-paragraph lists, nested lists, fenced code blocks as part of
;; list items, or combinations thereof.  (I should add some examples
;; of exactly why I wanted these behaviors, but I don't have them at
;; hand right now.)

(require 'el-patch)

(el-patch-feature markdown-mode)

(el-patch-defun markdown-calc-indents ()
  "Return a list of indentation columns to cycle through.
The first element in the returned list should be considered the
default indentation level.  This function does not worry about
duplicate positions, which are handled up by calling functions."
  (let (pos prev-line-pos positions)

    ;; Indentation of previous line
    (setq prev-line-pos (el-patch-swap
                          ;; `markdown-prev-line-indent' only looks at
                          ;; exactly the one, single preceding line.
                          ;; The patch here searches backwards for a
                          ;; *non-blank* line.
                          (markdown-prev-line-indent)
                          (save-excursion
                            (while (and (zerop (forward-line -1))
                                        (looking-at (rx (0+ space) eol))))
                            (if (bobp)
                                0
                              (current-indentation)))))
    (setq positions (cons prev-line-pos positions))

    ;; Indentation of previous non-list-marker text
    (when (setq pos (save-excursion
                      (el-patch-let (($orig (forward-line -1)))
                        (el-patch-swap
                          $orig
                          ;; Skip blank lines when looking backwards
                          ;; for a list.
                          (while (and (zerop $orig)
                                      (eolp)))))
                      (when (looking-at markdown-regex-list)
                        (- (match-end 3) (match-beginning 0)))))
      (setq positions (cons pos positions)))

    ;; Indentation required for a pre block in current context
    (setq pos (length (markdown-pre-indentation (point))))
    (setq positions (cons pos positions))

    ;; Indentation of the previous line + tab-width
    (if prev-line-pos
        (setq positions (cons (+ prev-line-pos tab-width) positions))
      (setq positions (cons tab-width positions)))

    ;; Indentation of the previous line - tab-width
    (if (and prev-line-pos (> prev-line-pos tab-width))
        (setq positions (cons (- prev-line-pos tab-width) positions)))

    ;; Indentation of all preceding list markers (when in a list)
    (when (setq pos (markdown-calculate-list-levels))
      (setq positions (append pos positions)))

    ;; First column
    (setq positions (cons 0 positions))

    ;; Return reversed list
    (reverse positions)))

(el-patch-validate 'markdown-calc-indents 'defun t)
