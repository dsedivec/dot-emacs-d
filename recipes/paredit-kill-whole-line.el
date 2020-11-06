;; -*- lexical-binding: t; -*-

;; Requires: el-patch

;; This is `kill-whole-line' but works in `paredit-mode' to keep
;; parens balanced.

(require 'paredit)

(eval-when-compile
  (require 'el-patch))

(defun my:paredit-kill-whole-line (&optional arg)
  "Like `kill-whole-line', but avoids breaking matched paredit pairs."
  (interactive "p")
  (when (null arg) (setq arg 1))
  (unless (zerop arg)
    (save-excursion
      (if (>= arg 0)
          ;; Move point to BOL.
          (forward-line 0)
        ;; arg is negative.  Go backwards arg - 1 lines.  Note that
        ;; this matches the behavior of `kill-whole-line', which kills
        ;; the current line and then arg - 1 lines backward when given
        ;; a negative argument.  We will then kill (abs arg) lines
        ;; forward from our new point.  Starting from the highest line
        ;; and killing forward nicely handles the case where, for
        ;; example, a preceding line is actually a multi-line list
        ;; that we're going to kill, where the number of lines
        ;; occupied by that list is >= (abs arg).
        (forward-line (1+ arg))
        (setq arg (abs arg)))
      (let ((target-line (save-excursion
                           (forward-line arg)
                           ;; Using a marker, rather than a position
                           ;; (a number), so that we can see how many
                           ;; lines `paredit-kill' is deleting each
                           ;; time.
                           (point-marker)))
            ;; This tells `paredit-kill' (as well as `kill-line',
            ;; incidentally) to kill everything on a line including
            ;; the newline, rather than everything up to the newline.
            (kill-whole-line t))
        (catch 'exit
          (while t
            (let ((target-line-before (line-number-at-pos target-line)))
              (paredit-kill)
              (let ((target-line-after (line-number-at-pos target-line)))
                (when (or
                       ;; Have we now deleted at least as many lines
                       ;; as we set out to delete?
                       (>= (line-number-at-pos) target-line-after)
                       ;; Did our last delete option fail to delete at
                       ;; least one line?  If so we've probably
                       ;; reached the end of an enclosing sexp, so we
                       ;; should stop.
                       (= target-line-before target-line-after))
                  (throw 'exit nil))))))))))

(with-eval-after-load 'paredit
  (define-key paredit-mode-map
      [remap kill-whole-line] 'my:paredit-kill-whole-line))

;; I believe this is a necessary and proper bug fix to
;; `paredit-kill-sexps-on-whole-line', which is only used from
;; `paredit-kill-sexps-on-line', which is in turn used from
;; `paredit-kill' (which I use in `my:paredit-kill-whole-line').
;; Without this fix,
;;
;;     (let ((kill-whole-line t)) (forward-line 0) (paredit-kill))
;;
;; will try to kill the whole current line *as well as any indentation
;; at the start of the following line*.  That means you'll have
;; indentation you didn't expect to kill in your kill ring---but you
;; probably won't even notice unless you then yank because
;; `paredit-kill-sexps-on-whole-line' will reindent the following
;; line.
;;
;; I think this also fixes another bug where a line like
;;
;;     (foo)  ; bar
;;
;; will not have its newline character killed, which is an error.

(el-patch-feature paredit)

(with-eval-after-load 'paredit
  (el-patch-defun paredit-kill-sexps-on-whole-line (beginning)
    (kill-region beginning
                 (or (save-excursion  ; Delete trailing indentation...
                       ;; Patch: don't go onto the next line
                       (paredit-skip-whitespace t (el-patch-add
                                                    (line-end-position)))
                       ;; Patch: only return point if we're looking at
                       ;; something other than a comment *or a
                       ;; newline*.  This is useful when called with
                       ;; BEGINNING as (line-beginning-position) on a
                       ;; line such as (point at |):
                       ;;
                       ;;         (foo)|   )
                       (and (not (el-patch-swap (eq (char-after) ?\; )
                                                (memq (char-after)
                                                      '(?\; ?\n))))
                            (point)))
                     ;; ...or just use the point past the newline, if
                     ;; we encounter a comment.
                     ;;
                     ;; Patch: actually go *past* the newline, as in
                     ;; the original function's comment.
                     (el-patch-wrap 1 (1+ (point-at-eol)))))
    (cond ((save-excursion (paredit-skip-whitespace nil (point-at-bol))
                           (bolp))
           ;; Nothing but indentation before the point, so indent it.
           (lisp-indent-line))
          ((eobp) nil)    ; Protect the CHAR-SYNTAX below against NIL.
          ;; Insert a space to avoid invalid joining if necessary.
          ((let ((syn-before (char-syntax (char-before)))
                 (syn-after  (char-syntax (char-after))))
             (or (and (eq syn-before ?\) )        ; Separate opposing
                      (eq syn-after  ?\( ))       ;   parentheses,
                 (and (eq syn-before ?\" )        ; string delimiter
                      (eq syn-after  ?\" ))       ;   pairs,
                 (and (memq syn-before '(?_ ?w))  ; or word or symbol
                      (memq syn-after  '(?_ ?w))))) ;   constituents.
           (insert " "))))

  (el-patch-validate 'paredit-kill-sexps-on-whole-line 'defun t))
