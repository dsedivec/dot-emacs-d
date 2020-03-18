;; -*- lexical-binding: t; -*-
;;
;; This file defines `my:LaTeX-convert-to-gls' to wrap some words at
;; point in \gls{}.  `my:LaTeX-backward-convert-to-gls' does the same
;; for words before point.
;;
;; If you're using these, you may be using too much \gls.  Maybe you
;; should stop.

(defun my:LaTeX-convert-to-gls (&optional start end num-words)
  "Put region between START and END, or NUM-WORDS forward, in \gls or \glspl."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end) nil)
                 (list nil nil (or current-prefix-arg 1))))
  (assert (or (and start end (null num-words))
              (and (null start) (null end) num-words)))
  (unless (null num-words)
    (save-excursion
      (forward-word num-words)
      (setq end (point))
      (backward-word num-words)
      (setq start (point))))
  (when (string-match (rx "'s" eos) (buffer-substring start end))
    (setq end (- end 2)))
  (atomic-change-group
    (let ((end (copy-marker end))
          (is-plural (string-match (rx "s" (0+ space) eos)
                                   (buffer-substring start end))))
      (goto-char start)
      (insert (let ((case-fold-search nil))
                (cond ((looking-at (rx upper))
                       (prog1
                           (if (looking-at (rx (>= 2 upper)))
                               "\\gls"
                             "\\Gls")
                         (downcase-region start end)))
                      (t "\\gls"))))
      (when is-plural (insert "pl"))
      (insert "{")
      (while (re-search-forward (rx (1+ (any "\n" space))) end t)
        (replace-match "-"))
      (goto-char end)
      (when is-plural (delete-char -1))
      (insert "}")
      (fill-paragraph)
      (undo-boundary))))

(defun my:LaTeX-backward-convert-to-gls (&optional num-words)
  "Put NUM-WORDS before cursor in \gls or \glspl."
  (interactive "p")
  (atomic-change-group
    (backward-word num-words)
    (my:LaTeX-convert-to-gls nil nil num-words)))

