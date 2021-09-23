;; -*- lexical-binding: t; -*-

;; This will automatically enable `black-format-on-save-mode' (which
;; I've defined in my init.el using Purcell's reformatter.el) if you
;; have a .editorconfig file with:
;;
;;     [*.py]
;;     org.codefu/python_formatter = black

(require 'editorconfig)

(declare-function 'black-format-on-save-mode "../init.el")

(defvar my:black-format-on-save t)
(put 'my:black-format-on-save 'safe-local-variable #'booleanp)

(defun my:maybe-enable-black-format-on-save ()
  ;; Guard on `buffer-file-name' needed to avoid Elpy stuffing Python
  ;; into temporary buffers when sending code to the REPL.
  (when buffer-file-name
    (let* ((default (gensym))
           (props (funcall editorconfig-get-properties-function))
           (formatter (gethash 'org.codefu/python_formatter props default)))
      (when (or (equal formatter "black")
                (and (eq formatter default)
                     (executable-find "black")
                     my:black-format-on-save))
        (black-format-on-save-mode 1)))))

(defun my:maybe-enable-black-format-on-save-later ()
  "Maybe enable `black-format-on-save-mode' after local variables are loaded."
  (add-hook 'hack-local-variables-hook #'my:maybe-enable-black-format-on-save
            nil t))

(add-hook 'python-mode-hook #'my:maybe-enable-black-format-on-save-later)
