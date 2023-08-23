;; -*- lexical-binding: t; -*-

;; `lsp-ivy-workspace-symbol' was causing an error deep in Ivy because
;; Ivy tries to use both the single-arg and three-arg model for
;; calling dynamic completion collection functions.  Or something like
;; that.  The single arg version got called, there must have been an
;; error, then Ivy tried the three arg call.  That raised an error
;; because the function (provided by lsp-mode) didn't take three
;; arguments, thus hiding the original error.  Now we re-raise the
;; original error.

(require 'el-patch)

(el-patch-defun ivy--dynamic-collection-cands (input)
  (let ((coll (condition-case (el-patch-swap nil err)
                  (funcall (ivy-state-collection ivy-last) input)
                (error
                 (el-patch-wrap 2 1
                   (condition-case nil
                       (funcall (ivy-state-collection ivy-last) input nil t)
                     (wrong-number-of-arguments
                      (signal (car err) (cdr err)))))))))
    (if (listp coll)
        (mapcar (lambda (x) (if (consp x) (car x) x)) coll)
      coll)))

(el-patch-validate 'ivy--dynamic-collection-cands 'defun t)
