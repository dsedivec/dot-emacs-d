;; -*- lexical-binding: t; -*-
;;
;; Add an Ivy "t" action to turn on tracing for a function from
;; `counsel-describe-function'.

(defun my:counsel-trace-function-toggle (func-name)
  (let ((func (intern func-name)))
    (if (trace-is-traced 'func)
        (untrace-function func)
      (trace-function func))))

(with-eval-after-load 'counsel
  (ivy-add-actions
   'counsel-describe-function
   '(("t" my:counsel-trace-function-toggle "toggle tracing"))))
