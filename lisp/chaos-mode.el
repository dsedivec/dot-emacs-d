;;; chaos-mode.el --- Randomly swap function definitions  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
;; Version: 1

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

;; Randomly swaps function definitions between symbols every so often.
;; Why would you use this?

;;; Code:

(require 'seq)

(defgroup chaos nil "Randomly break Emacs, slowly"
          :group 'convenience
          :prefix "chaos-")

(defcustom chaos-frequency 60
  "How long to wait before breaking another pair of functions."
  :type 'integer)

(defcustom chaos-forbidden-symbol-regexp "^chaos-"
  "Regexp for symbols that `chaos-mode' shouldn't touch."
  :type 'regexp)

(defcustom chaos-quiet nil
  "Don't tell the user when you've swapped functions."
  :type 'boolean)

(defcustom chaos-num-samples 20
  "How hard to try and find functions to swap in `obarray'."
  :type 'integer)

(defvar chaos-swapped-functions nil)

(defun chaos--compatible-arities (f1 f2)
  (pcase-let ((`(,f1-min . ,f1-max) (func-arity f1))
              (`(,f2-min . ,f2-max) (func-arity f2)))
    (and (= f1-min f2-min)
         ;; (func-arity 'or) → (0 . unevalled)
         (not (eq f1-max 'unevalled))
         (not (eq f2-max 'unevalled))
         ;; (func-arity 'min) → (1 . many)
         (eql f1-max f2-max))))

;; Reservoir sampling courtesy
;; https://en.wikipedia.org/wiki/Reservoir_sampling#An_optimal_algorithm
(defun chaos--random-sample-functions (sample-size)
  (cl-assert (> sample-size 0))
  (let* ((samples (make-vector sample-size nil))
         (num-funcs 0)
         (W (exp (/ (log (cl-random 1.0)) sample-size)))
         (i (+ sample-size (floor (/ (log (cl-random 1.0))
                                     (log (- 1 W)))))))
    (mapatoms (lambda (sym)
                (when (and (functionp sym)
                           (not (string-match-p chaos-forbidden-symbol-regexp
                                                (symbol-name sym)))
                           (ignore-errors
                             (not (eq (cdr (func-arity sym)) 'unevalled))))
                  (cond
                    ((< num-funcs sample-size)
                     (aset samples num-funcs sym))
                    ((= num-funcs i)
                     (aset samples (random sample-size) sym)
                     (setq W (exp (/ (log (cl-random 1.0)) sample-size))
                           i (+ i 1 (floor (/ (log (cl-random 1.0))
                                              (log (- 1 W))))))))
                  (cl-incf num-funcs))))
    samples))

(defun chaos--swap-random-function-pair ()
  (catch 'swapped
    (let ((funcs (seq-into (chaos--random-sample-functions chaos-num-samples)
                           'list)))
      (while (cdr funcs)
        (let ((s1 (pop funcs)))
          (dolist (s2 funcs)
            ;; `ignore-errors' because sometimes fset fails?
            (when (ignore-errors
                    (let ((f2 (symbol-function s2)))
                      (fset s2 (symbol-function s1))
                      (fset s1 f2)
                      t))
              (push (cons s1 s2) chaos-swapped-functions)
              (unless chaos-quiet
                (message "Ye gods, %s and %s have swapped bodies!" s1 s2))
              (throw 'swapped nil))))))
    (unless chaos-quiet
      (message "You have escaped chaos... for now."))))

(defvar chaos--timer nil)

;;;###autoload
(define-minor-mode chaos-mode
    "Randomly swap function definitions."
  :global t
  :lighter " 乱"
  (when (timerp chaos--timer)
    (cancel-timer chaos--timer)
    (setq chaos--timer nil))
  (when chaos-mode
    (setq chaos--timer (run-with-timer t chaos-frequency
                                       #'chaos--swap-random-function-pair))))

(provide 'chaos-mode)
;;; chaos-mode.el ends here
