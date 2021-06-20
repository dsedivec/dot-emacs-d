;; -*- lexical-binding: t; -*-

;; Don't compare columns to character position
;;
;; This caused TAB to move your cursor to the beginning of the line
;; when (re)indenting in a file that used tabs for indentation.
;;
;; (save-excursion (back-to-indentation) (point)) seems expensive.  I
;; originally compared (current-indentation) to (current-column)
;; instead, but I found that (current-column) often *lied* when used
;; in conjunction with smart-tabs-mode, which modifies tab-width
;; inside the python.el indent functions.  This seemed to be some kind
;; of race, as I would get the wrong number from (current-column),
;; (sleep-for 1), and then I could get the right number from
;; (current-column), all within (python-indent-line).
;;
;; I fought with that for a couple hours, and eventually landed on
;; just using character positions instead of column positions.
;;
;; I also removed the comparison against (line-beginning-position)
;; because, as far as I can tell, that condition will always be true.

(require 'el-patch)

(el-patch-feature python)

(with-eval-after-load 'python
  (el-patch-defun python-indent-line (&optional previous)
    "Internal implementation of `python-indent-line-function'.
Use the PREVIOUS level when argument is non-nil, otherwise indent
to the maximum available level.  When indentation is the minimum
possible and PREVIOUS is non-nil, cycle back to the maximum
level."
    (let ((follow-indentation-p
           ;; Check if point is within indentation.
           (el-patch-swap
             (and (<= (line-beginning-position) (point))
                  (>= (+ (line-beginning-position)
                         (current-indentation))
                      (point)))
             ;; Check if point is within indentation.  Note that using
             ;; `current-column' here instead of this "relatively
             ;; expensive" excursion led to problems in conjunction with
             ;; smart-tabs-mode, where `current-column' would sometimes
             ;; use smart-tabs-mode's large value of `tab-width', other
             ;; times the normal value of `tab-width'.
             (>= (save-excursion (back-to-indentation) (point)) (point)))))
      (save-excursion
        (indent-line-to
         (python-indent-calculate-indentation previous))
        (python-info-dedenter-opening-block-message))
      (when follow-indentation-p
        (back-to-indentation))))

  (el-patch-validate 'python-indent-line 'defun t))
