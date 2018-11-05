;; -*- lexical-binding: t; -*-

;; I want to use company-dabbrev-code with company-capf.  This means
;; that, for example, I can get completion on functions I'm
;; currently writing, before I've evaluated them, in Emacs Lisp
;; buffers.  Very nice.
;;
;; However, you will get duplicates from company-dabbrev-code
;; if/when company-capf returns completions with annotations like
;; "<f>" on functions in Emacs Lisp buffers.  Company does not
;; consider two candidates with identical strings but different
;; annotations to be duplicates.  However, I can't imagine
;; company-dabbrev-code ever adds annotations, so if you've got a
;; completion from it that is a duplicate of one from a smarter
;; backend, we always drop the duplicate company-dabbrev-code
;; completion.  To accomplish this requires us to add a new function
;; to company-transformers.
;;
;; See also:
;; https://github.com/company-mode/company-mode/issues/432
;; https://github.com/company-mode/company-mode/pull/509
;; Company commit 7779820493, and its revert in 395f846b05f

(defun my:company-remove-dabbrev-duplicates (candidates)
  (if (not (and (consp company-backend)
                (or (memq 'company-dabbrev-code company-backend))))
      candidates
    (let (last)
      (cl-delete-if
       (lambda (cand)
         (not (unless (and (equal cand last)
                           (eq (get-text-property 0 'company-backend cand)
                               'company-dabbrev-code))
                (setq last cand))))
       ;; Make sure dabbrev-code candidates are sorted last.  This
       ;; may already be done by company but I can't see that it's
       ;; guaranteed.
       (sort candidates
             (lambda (c1 c2)
               (let ((comparison (compare-strings c1 0 (length c1)
                                                  c2 0 (length c2))))
                 (if (eq comparison t)
                     (eq (get-text-property 0 'company-backend c2)
                         'company-dabbrev-code)
                   (minusp comparison)))))))))

;; Add our transformer.
(with-eval-after-load 'company
  (add-to-list 'company-transformers #'my:company-remove-dabbrev-duplicates))
