;; -*- lexical-binding: t; -*-

;; Leave a single blank line before a heading if there already is one.

;; Sheesh, fix your indents!
(put 'org-with-limited-levels 'lisp-indent-function 0)

(defun my:org--blank-before-heading-p (orig-fun &optional parent)
  "Adds 'preserve method to `org-blank-before-new-entry' to
preserve preceding blank line.  PARENT is ignored as I don't see
how it makes sense to use it in this context."
  (pcase (assq 'heading org-blank-before-new-entry)
    (`(heading . preserve)
     (and (not (org-at-heading-p)) (org-previous-line-empty-p)))
    (_ (funcall orig-fun parent))))

(advice-add 'org--blank-before-heading-p :around
            #'my:org--blank-before-heading-p)
