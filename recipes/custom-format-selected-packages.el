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
          pos
          packages)
      ;; `with-demoted-errors' deployed *strategically* throughout
      ;; this advice to avoid blowing up `custom-save-variables'
      ;; because we didn't find exactly what we were looking for.  I
      ;; don't just wrap this whole thing in `with-demoted-errors'
      ;; because once we start modifying the buffer, you probably need
      ;; to know about errors since we might have just made the
      ;; `custom-set-variables' form invalid.
      (when
          (with-demoted-errors err-fmt
            ;; `custom-save-variables' should have left us right at
            ;; the end of the `custom-set-variables' form that it
            ;; inserted.
            (forward-sexp -1)
            (unless (looking-at-p "(custom-set-variables\\_>")
              (error "Expected to be at `custom-set-variables' form"))
            ;; Move into the list of variables.
            (down-list)
            (forward-sexp)
            ;; Look for `package-selected-packages' in the list of
            ;; variables.
            (condition-case err
                ;; Yeah, this is ugly.  Read carefully and note that
                ;; we save the whole `package-selected-packages' form
                ;; in `form'.
                (while (not (eq (caadr (setq form (read this-buf)))
                                'package-selected-packages)))
              (t (error "Couldn't find `package-selected-packages': %S" err)))
            (pcase form
              (`(quote (package-selected-packages
                        (quote ,(and (pred listp) pcase-packages))))
               (setq packages pcase-packages))
              (_
               (error "`package-selected-packages' has unexpected value: %S"
                      form)))
            ;; Move back to beginning of '(package-selected packages
            ;; ...) because `read' moved us to the end of it
            (forward-sexp -1)
            ;; Move inside the `package-selected-packages' entry.
            (down-list 1)
            ;; Emacs 26 writes it as "(quote (list of packages))".
            ;; Emacs 27 writes it as "'(list of packages)".  Perform
            ;; accordingly.  ">" is "end of comment" and is needed
            ;; here to skip over newlines.
            (skip-syntax-forward "w_->")
            (cond
              ((looking-at-p "(quote")
               ;; Enter this extra list that doesn't exist in Emacs 27.
               (down-list 1))
              ((not (eq (char-after) ?'))
               (error (concat "Can't find list of packages in"
                              " `package-selected-packages' stuck at pos %d")
                      (point))))
            ;; Enter the list of packages
            (down-list 1)
            ;; Now go back up to its opening parenthesis
            (backward-up-list 1)
            ;; Success, ready to rewrite the list of packages
            t)
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
        (delete-region pos (point))))))

(advice-add 'custom-save-variables :after
            #'my:custom-save-selected-packages-sorted-one-per-line)
