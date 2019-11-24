;; -*- lexical-binding: t; -*-

;; Set `my:web-mode-local-html-engine' as a file/directory local
;; variable in order to affect `web-mode-engine'.

;; For future reference, here's an alternative way to define
;; per-directory-tree engines, as opposed to my file/dir local
;; variable hook, below.
;; (add-to-list 'web-mode-engines-alist
;;              (cons "cheetah"
;;                    (format "\\`%s/.*\\.html\\(\\.translate\\)?\\'"
;;                            (expand-file-name "~/git/pippin"))))

(defvar my:web-mode-local-html-engine nil
  "This can be set as a file or directory local variable and
`my:web-mode-set-html-engine-from-local-variable' will use it to
set the engine for the file upon loading.")

(put 'my:web-mode-local-html-engine 'safe-local-variable #'stringp)

(defun my:web-mode-set-html-engine-from-local-variable ()
  "Set web-mode engine from `my:web-mode-local-html-engine', if set."
  (when (and (boundp 'my:web-mode-local-html-engine)
             (stringp my:web-mode-local-html-engine))
    (web-mode-set-engine
     (web-mode-engine-canonical-name my:web-mode-local-html-engine))))

(defun my:web-mode-set-hook-to-set-engine-from-local-variable ()
  "Set up engine from file-local variable after local variables are loaded."
  (add-hook 'hack-local-variables-hook
            #'my:web-mode-set-html-engine-from-local-variable nil t))

(add-hook 'web-mode-hook
          #'my:web-mode-set-hook-to-set-engine-from-local-variable)
