;; -*- lexical-binding: t; -*-
;;
;; Add fancy numbers in the mode line corresponding to winum's
;; numbering.  Inspired by (or maybe "copied from", I can't remember)
;; Spacemacs.

(defvar my:winum-number-string-base #x2780)

(defvar my:winum-number-string-min 1)

(defvar my:winum-number-string-max 10)

(defun my:winum-get-number-string (&optional window)
  (let* ((n (winum-get-number window))
         (s (if (numberp n)
                (concat
                 (if (and (>= n my:winum-number-string-min)
                          (<= n my:winum-number-string-max))
                     (char-to-string (+ my:winum-number-string-base
                                        (- n my:winum-number-string-min)))
                   (int-to-string n))
                 " ")
              "")))
    (propertize s 'face 'winum-face)))

(with-eval-after-load 'winum
  (push '(winum-mode (:eval (my:winum-get-number-string)))
        (cdr mode-line-format))

  (set-face-attribute 'winum-face nil :height 1.2))
