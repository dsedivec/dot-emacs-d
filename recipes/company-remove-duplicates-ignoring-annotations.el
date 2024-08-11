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

(defvar my:company-remove-duplicates-low-priority-backends
  '(company-dabbrev-code company-dabbrev))

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
  ;; default-backend is here, I guess, because you could have a
  ;; grouped backend with a low priority backend as first in the list,
  ;; and IIRC the first backend in a grouped
  ;; backend... maybe... doesn't get a company-backend property?  It's
  ;; been a while.  I'm not sure this is correct, let alone useful.
  (let ((default-backend (if (listp company-backend)
                             (car company-backend)
                           company-backend))
        (candidates-ht (make-hash-table :test #'equal))
        (idx 0)
        has-duplicates)
    (dolist (cand candidates)
      (let* ((cand-backend (or (get-text-property 0 'company-backend cand) default-backend))
             (low-priority-backend (memq cand-backend my:company-remove-duplicates-low-priority-backends))
             (existing (gethash cand candidates-ht)))
        (when existing
          (setq has-duplicates t))
        (when (or (not existing)
                  (and (not low-priority-backend) (cdr existing)))
          (puthash cand (cons idx low-priority-backend) candidates-ht)))
      (cl-incf idx))
    ;; Questionable premature optimization: Don't make the second pass
    ;; and return candidates as-is if we didn't find any duplicates.
    (if (not has-duplicates)
        candidates
      (setq idx 0)
      (seq-filter (lambda (cand)
                    (prog1
                        (when-let ((ht-entry (gethash cand candidates-ht)))
                          (= (car ht-entry) idx))
                      (cl-incf idx)))
                  candidates))))

;; Add our transformer.
(with-eval-after-load 'company
  (add-to-list 'company-transformers
               #'my:company-remove-duplicates-ignoring-annotations))
