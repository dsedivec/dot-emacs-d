;; -*- lexical-binding: t; -*-
;;
;; Add an Ivy "t" action to turn on tracing for a function from
;; `counsel-describe-function'.

(require 'ivy)

(eval-when-compile
  (require 'trace))

(autoload 'trace-is-traced "trace")

(defun my:counsel-trace-function-toggle (func-name)
  (let ((func (intern func-name)))
    (if (trace-is-traced 'func)
        (untrace-function func)
      (trace-function func))))

(ivy-add-actions 'counsel-describe-function
                 '(("t" my:counsel-trace-function-toggle "toggle tracing")))
