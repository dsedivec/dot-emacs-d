;; -*- lexical-binding: t; -*-

;; Requires: el-patch

;; This is `kill-whole-line' but works in `paredit-mode' to keep
;; parens balanced.

(defun my:paredit-kill-whole-line (&optional arg)
  (interactive "p")
  (when (null arg) (setq arg 1))
  (unless (zerop arg)
    (save-excursion
      (if (>= arg 0)
          (forward-line 0)
        (forward-line (1+ arg))
        (setq arg (abs arg)))
      (let ((target-line (save-excursion
                           (forward-line arg)
                           (point-marker)))
            (kill-whole-line t))
        (catch 'exit
          (while t
            (let ((target-line-before (line-number-at-pos target-line)))
              (paredit-kill)
              (let ((target-line-after (line-number-at-pos target-line)))
                (when (or
                       (>= (line-number-at-pos) target-line-after)
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
           (insert " ")))))
