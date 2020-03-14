;; -*- lexical-binding: t; -*-
;;
;; Split Python triple-quoted strings correct, until
;; https://github.com/Fuco1/smartparens/issues/1015 is fixed.

(defun my:sp-split-python-strings (orig-fun arg)
  (let ((syntax (and (derived-mode-p 'python-mode)
                     (syntax-ppss))))
    (if (and syntax (elt syntax 3))
        (let ((quotes (save-excursion
                        (goto-char (elt syntax 8))
                        (and (looking-at "\"\"\"\\|'''\\|['\"]")
                             (match-string-no-properties 0)))))
          (when arg
            (error "I never wrote support for prefix arg in my advice, sorry")
            ;; But seriously, I have never used that feature.  And in Python?!
            )
          (unless quotes
            (error "Could not find quotes at start of string"))
          (insert quotes)
          (save-excursion
            (insert quotes)))
      (funcall orig-fun arg))))

(advice-add 'sp-split-sexp :around 'my:sp-split-python-strings)
