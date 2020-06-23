;; -*- lexical-binding: t; -*-
;;
;; Set `my:web-mode-local-engine' as a file/directory local variable
;; in order to affect `web-mode-engine'.
;;
;; For future reference, here's an alternative way to define
;; per-directory-tree engines, as opposed to my file/dir local
;; variable hook, below.
;;
;; (add-to-list 'web-mode-engines-alist
;;              (cons "cheetah"
;;                    (format "\\`%s/.*\\.html\\(\\.translate\\)?\\'"
;;                            (expand-file-name "~/git/pippin"))))

(declare-function web-mode-engine-canonical-name "web-mode")
(declare-function web-mode-set-engine "web-mode")

(defvar my:web-mode-local-engine nil
  "This can be set as a file or directory local variable and
`my:web-mode-set-engine-from-local-variable' will use it to
set the engine for the file upon loading.")

(defun my:web-mode-safe-local-engine (engine)
  (when (symbolp engine)
    (setq engine (symbol-name engine)))
  (web-mode-engine-canonical-name engine))

(put 'my:web-mode-local-engine 'safe-local-variable
     #'my:web-mode-safe-local-engine)

(defun my:web-mode-set-engine-from-local-variable ()
  "Set web-mode engine from `my:web-mode-local-engine', if set."
  (when (boundp 'my:web-mode-local-engine)
    (let ((engine my:web-mode-local-engine))
      (when (symbolp engine)
        (setq engine (symbol-name engine)))
      (when (stringp engine)
        (web-mode-set-engine engine)))))

(defun my:web-mode-set-hook-to-set-engine-from-local-variable ()
  "Set up engine from file-local variable after local variables are loaded."
  (add-hook 'hack-local-variables-hook
            #'my:web-mode-set-engine-from-local-variable 0 t))

(add-hook 'web-mode-hook
          #'my:web-mode-set-hook-to-set-engine-from-local-variable)
