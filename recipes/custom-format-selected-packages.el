;; -*- lexical-binding: t; -*-

;; Re-write `package-selected-packages' in `custom-file' with one
;; package per line, sorted.  This way, when I've added a bunch of new
;; packages to my init.el, and I want to commit each addition in its
;; own logical commit, I don't have to edit the staged version of
;; `package-selected-packages': now I can just select the lines that
;; have the packages I want to add in that commit.  Much easier, much
;; less error prone, and all at the low low cost of a little
;; moderately-gross advice!

(defun my:custom-save-selected-packages-sorted-one-per-line (&rest _args)
  (save-excursion
    (let ((err-fmt "In `my:selected-packages-one-per-line' advice: %S")
          (this-buf (current-buffer))
          form
          pos)
      ;; `with-demoted-errors' deployed *strategically* throughout
      ;; this advice to avoid blowing up `custom-save-variables'
      ;; because we didn't find exactly what we were looking for.  I
      ;; don't just wrap this whole thing in `with-demoted-errors'
      ;; because once we start modifying the buffer, you probably need
      ;; to know about errors since we might have just made the
      ;; `custom-set-variables' form invalid.
      (with-demoted-errors err-fmt
        ;; `custom-save-variables' should have left us right at the
        ;; end of the `custom-set-variables' form that it inserted.
        (forward-sexp -1)
        (unless (looking-at-p "(custom-set-variables\\_>")
          (error "Expected to be at `custom-set-variables' form"))
        ;; Move into the list of variables.
        (down-list)
        (forward-sexp)
        ;; Look for `package-selected-packages' in the list of
        ;; variables.
        (condition-case err
            ;; Yeah, this is ugly.  Read carefully and note that we
            ;; save the whole `package-selected-packages' form in
            ;; `form'.
            (while (not (eq (caadr (setq form (read this-buf)))
                            'package-selected-packages)))
          (t (error "Couldn't find `package-selected-packages': %S" err))))
      (pcase form
        ;; Make sure this form looks like how we expect:
        ;;
        ;;     '(package-selected-packages '(<list of packages>))
        (`(quote (package-selected-packages
                  (quote ,(and (pred listp) packages))))
         (with-demoted-errors err-fmt
           ;; Move back to beginning of '(package-selected packages
           ;; ...) because `read' moved us to the end of it
           (forward-sexp -1)
           ;; Go inside the list of packages
           (down-list 3)
           ;; Go to the opening parenthesis of the list of packages
           (backward-up-list 1))
         ;; Not going to use `with-demoted-errors' after this since
         ;; we're going to start modifying the buffer.
         ;;
         ;; Insert the package list with one package per line
         (insert "(")
         (newline-and-indent)
         (dolist (package (cl-sort packages #'string< :key #'symbol-name))
           (prin1 package this-buf)
           (newline-and-indent))
         (insert ")")
         ;; Delete the old list of packages (we inserted in front of it)
         (setq pos (point))
         (forward-sexp)
         (delete-region pos (point)))
        (_
         (error "`package-selected-packages' doesn't look as expected: %S"
                form))))))

(advice-add 'custom-save-variables :after
            #'my:custom-save-selected-packages-sorted-one-per-line)
