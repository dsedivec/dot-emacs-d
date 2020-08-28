;; -*- lexical-binding: t; -*-

(require 'flycheck)

(defun my:flycheck-python-pylint-ensure-tabs-option ()
  (let ((command (flycheck-checker-get 'python-pylint 'command))
        (tabs-opt '(eval (when indent-tabs-mode
                           "--indent-string=\\t"))))
    (unless (member tabs-opt command)
      (unless (eq (car (last command)) 'source-inplace)
        (error (concat "Can't find source-inplace at end of pylint command when"
                       " trying to insert tabs option, did Flycheck change?")))
      (setf (flycheck-checker-get 'python-pylint 'command)
            (append (butlast command 1) (list tabs-opt) (last command)))
      (message "Added tab indent detection to Flycheck's pylint checker."))))

(defun my:flycheck-python-pylint-ensure-tabs-option-advice (symbol &rest _args)
  (when (eq symbol 'python-pylint)
    (my:flycheck-python-pylint-ensure-tabs-option)))

(advice-add 'flycheck-define-command-checker :after
            #'my:flycheck-python-pylint-ensure-tabs-option-advice)

(my:flycheck-python-pylint-ensure-tabs-option)
