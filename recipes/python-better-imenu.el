;; -*- lexical-binding: t; -*-

;; New and improved Python imenu function
;;
;; Here's a gnarly test case that the built-in version doesn't handle
;; right, I think:
;;
;;     class X:
;;         def error(self):
;;             pass
;;
;;         def other():
;;             pass
;;
;;         def add(self):
;;             if True:
;;                 def binder():
;;                     pass
;;
;;                 def tester():
;;                     pass
;;
;;     class X:
;;         def error(self):
;;             pass
;;
;;         def other():
;;             pass
;;
;;         def add(self):
;;             if True:
;;                 def binder():
;;                     pass
;;
;;                 def tester():
;;                     pass
;;
;;     def stuff():
;;         pass
;;
;;     def more_stuff():
;;         pass
;;
;; (Maybe take the last two top-level functions out if you can't
;; reproduce with the above.  Was I really testing this with two
;; classes both named X?  What a weird case.)

(defun my:python-imenu-create-index-improved ()
  (goto-char (point-max))
  (cl-loop
    with siblings = nil
    with stack = nil
    with last-indent = 0
    for pos = (python-nav-backward-defun)
    while pos
    do
     (let* ((defun-type-name (python-imenu--get-defun-type-name))
            (type (car defun-type-name))
            (name (cadr defun-type-name))
            (label (when name
                     (funcall python-imenu-format-item-label-function
                              type name)))
            (indent (current-indentation)))
       (cond
         ((= indent last-indent)
          (push (cons label pos) siblings))
         ((< indent last-indent)
          (cl-assert siblings t)
          (let ((branch (python-imenu--put-parent type name pos siblings)))
            (if (or (null stack) (> indent (caar stack)))
                ;; The siblings at the top of the stack are at a
                ;; less-indented level than the current line, or
                ;; else there is no prior list of siblings, which
                ;; means we just create a new set of siblings with
                ;; the current line as its only member.
                (setq siblings (list branch))
              (setq siblings (cons branch (cdr (pop stack)))))))
         (t
          (cl-assert (> indent last-indent) t)
          ;; We're entering a higher level of indent, start
          ;; collecting a list of siblings for some future parent.
          (push (cons last-indent siblings) stack)
          (setq siblings (list (cons label pos)))))
       (setq last-indent indent))
    finally
     (while stack
       ;; You should probably never have to get in here, especially
       ;; not if you have a syntactically-correct Python file.
       (let ((branch (cons "<Unknown>" siblings)))
         (setq siblings (cons branch (cdr (pop stack))))))
     (cl-return siblings)))

(advice-add #'python-imenu-create-index
            :override #'my:python-imenu-create-index-improved)
