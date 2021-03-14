;; -*- lexical-binding: t; -*-

;; This will automatically enable `black-format-on-save-mode' (which
;; I've defined in my init.el using Purcell's reformatter.el) if you
;; have a .editorconfig file with:
;;
;;     [*.py]
;;     org.codefu/python_formatter = black

(require 'editorconfig)

(declare-function 'black-format-on-save-mode "../init.el")

(defun my:maybe-enable-black-format-on-save ()
  ;; Guard on `buffer-file-name' needed to avoid Elpy stuffing Python
  ;; into temporary buffers when sending code to the REPL.
  (when buffer-file-name
    (let ((props (funcall editorconfig-get-properties-function)))
      (when (equal (gethash 'org.codefu/python_formatter props) "black")
        (black-format-on-save-mode 1)))))

(add-hook 'python-mode-hook #'my:maybe-enable-black-format-on-save)
