;;; Prologue

;; Prefer loading a newer .el to an older .elc.  Probably keeps me
;; from getting in trouble if I forget to byte compile.
(setq load-prefer-newer t)

;; Set this early before I potentially install packages, which will
;; modify customizable variable `package-selected-packages'.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)


;;; package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(defvar my:packages
  '(
    counsel
    )
  "List of packages I want installed.")

(defvar my:package-last-refresh 0)

(defvar my:package-max-age-before-refresh 3600)

(define-advice package-refresh-contents
    (:after (&rest args) my:note-last-refresh-time)
  (setq my:package-last-refresh (float-time)))

(defun my:package-sync ()
  "Install all packages listed by `my:packages'."
  (interactive)
  (dolist (pkg my:packages)
    (unless (package-installed-p pkg)
      (when (>= (- (float-time) my:package-last-refresh)
	        my:package-max-age-before-refresh)
	(package-refresh-contents))
      (package-install pkg))))

(my:package-sync)


;;; counsel
(counsel-mode 1)
