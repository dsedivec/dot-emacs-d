;; -*- lexical-binding: t; -*-

;; Don't let flex matching match candidates that are too long.  Adjust
;; the number in the `rx' form within
;; `my:completion-flex-skip-long-candidates' as necessary for
;; performance.  40 (the current value in that function) was my
;; starting point and seemed very generous---that's a long symbol.
;;
;; This is a reaction to realizing that long Elisp symbols would kill
;; company-mode performance when flex completion is in use, such as
;; documented at https://github.com/raxod502/straight.el/issues/803.
;; This problem is probably not unique to Elisp, though, so this fix
;; applies itself whenever flex matching is in use.

(defvar my:completion-in-flex-completion nil)

(defun my:completion-track-in-flex-completion (orig-fun &rest args)
  (let ((my:completion-in-flex-completion t))
    (apply orig-fun args)))

(advice-add #'completion-flex-try-completion
            :around #'my:completion-track-in-flex-completion)

(advice-add #'completion-flex-all-completions
            :around #'my:completion-track-in-flex-completion)

(defun my:completion-flex-skip-long-candidates (orig-fun &rest args)
  (if my:completion-in-flex-completion
      (let ((completion-regexp-list completion-regexp-list))
        (cl-assert completion-regexp-list)
        (push (rx bos (** 0 40 not-newline) eos) completion-regexp-list)
        (apply orig-fun args))
    (apply orig-fun args)))

(advice-add #'all-completions :around #'my:completion-flex-skip-long-candidates)
