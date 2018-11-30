;; -*- lexical-binding: t; -*-

;; This patches Flycheck's pylint support to add a --disable option to
;; the checker.  Maybe I should see about pushing these changes
;; upstream.
(flycheck-def-option-var flycheck-pylint-disabled-messages nil python-pylint
  "String to be passed to pylint's --disable option."
  :type 'string)
;; TODO: Add custom args for completeness!

(put 'python-pylint 'flycheck-command
     '("pylint" "-r" "n"
       "--output-format" "text"
       "--msg-template"
       (eval (if flycheck-pylint-use-symbolic-id
                 "{path}:{line}:{column}:{C}:{symbol}:{msg}"
               "{path}:{line}:{column}:{C}:{msg_id}:{msg}"))
       (option "--disable" flycheck-pylint-disabled-messages)
       (config-file "--rcfile" flycheck-pylintrc)
       ;; Need `source-inplace' for relative imports (e.g. `from .foo
       ;; import bar'), see https://github.com/flycheck/flycheck/issues/280
       source-inplace))
