;; -*- lexical-binding: t; -*-

;; I want to use `company-dabbrev-code' with `company-capf'.  This
;; means that, for example, I can get completion on functions I'm
;; currently writing, before I've evaluated them, in Emacs Lisp
;; buffers.  Very nice.
;;
;; However, you will get duplicates from `company-dabbrev-code'
;; if/when `company-capf' returns completions with annotations like
;; "<f>" on functions in Emacs Lisp buffers.  Company does not
;; consider two candidates with identical strings but different
;; annotations to be duplicates.  However, I can't imagine
;; `company-dabbrev-code' ever adds annotations, so if you've got a
;; completion from it that is a duplicate of one from a smarter
;; backend, we always drop the duplicate `company-dabbrev-code'
;; completion.  To accomplish this requires us to add a new function
;; to `company-transformers'.
;;
;; See also:
;; https://github.com/company-mode/company-mode/issues/432
;; https://github.com/company-mode/company-mode/pull/509
;; Company commit 7779820493, and its revert in 395f846b05f

(defun my:company-remove-duplicates-ignoring-annotations (candidates)
  "Reorder company candidates, removing any duplicates.
cand-1 is a duplicate of cand-2 if (string= cand-1 cand-2).  Note
that this ignores text properties, such as the company-backend
text property as well as any annotation-related properties.  This
is desirable to, for example, remove duplicate candidates when
using `company-dabbrev-code' grouped with other, (presumably) more
intelligent backends.

In fact, this function will also replace a candidate from
`company-dabbrev-code' with any other `string=' candidate.

Order of candidates is preserved (which is usually
important/desirable, particularly when using something like
company-prescient).  If a `company-dabbrev-code' candidate has a
duplicate later in the of candidates, the `company-dabbrev-code'
candidate will be replaced by the candidate that appears later in
the list."
  (let* ((default-backend (if (listp company-backend)
                              (car company-backend)
                            company-backend))
         (best-cands (make-hash-table :test #'equal))
         has-duplicates)
    ;; First pass: Put the best candidate in hash table best-cands.
    ;; Candidates from `company-dabbrev-code' backend are worse than
    ;; all other candidates.  Aside from that rule, first candidate ==
    ;; best candidate.
    (dolist (cand candidates)
      (pcase-let* ((cand-backend (or (get-text-property 0 'company-backend cand)
                                     default-backend))
                   (cand-prio (cond
                                ((eq cand-backend 'company-dabbrev-code) -10)
                                (t 0)))
                   (`(,best-cand . ,best-prio) (gethash cand best-cands)))
        (when best-cand
          (setq has-duplicates t)
          (when (> cand-prio best-prio)
            (puthash cand (cons cand cand-prio) best-cands)))))
    (if has-duplicates
        ;; Second pass: Remove duplicates.  Replace the first instance
        ;; of a given candidate with its best candidate, e.g. replace
        ;; a `company-dabbrev-code' candidate with a duplicate
        ;; candidate from any other backend.
        (cl-loop
          for cand in candidates
          for (best-cand . best-prio) = (gethash cand best-cands)
          if best-cand
          collect best-cand
          and do (remhash cand best-cands))
      ;; There were no duplicates (maybe a common case), we can just
      ;; return the original list.
      candidates)))

;; Add our transformer.
(with-eval-after-load 'company
  (add-to-list 'company-transformers
               #'my:company-remove-duplicates-ignoring-annotations))
