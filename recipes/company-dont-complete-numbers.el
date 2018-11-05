;; -*- lexical-binding: t; -*-

;; In sql-mode (company-dabbrev-code) and org-mode (company-dabbrev)
;; I was getting stuff like 123456789 completed, which was rarely
;; what I wanted when typing "12".  Never complete numbers.  I can
;; manually invoke dabbrev if that's what I really want.
;; https://github.com/company-mode/company-mode/issues/358
;;
;; This applies to both company-dabbrev and company-dabbrev-code, so
;; I configure it here for brevity of code, rather than individually
;; in use-package forms for those two packages.
(defun my:company-dabbrev-dont-complete-numbers (candidates)
  (cl-remove-if (lambda (cand)
                  (and (string-match-p "^[[:digit:]]" cand)
                       (memq (or (get-text-property 0 'company-backend
                                                    cand)
                                 (and (listp company-backend)
                                      (car company-backend))
                                 company-backend)
                             '(company-dabbrev
                               company-dabbrev-code))))
                candidates))

(dolist (feature '(company-dabbrev company-dabbrev-code))
  (with-eval-after-load feature
    (add-to-list 'company-transformers
                 'my:company-dabbrev-dont-complete-numbers)))
