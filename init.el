;;;; Prologue

;; Prefer loading a newer .el to an older .elc.  Probably keeps me
;; from getting in trouble if I forget to byte compile.
(setq load-prefer-newer t)


;;; Customization

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
    bind-key
    counsel
    ivy
    magit
    paredit
    persp-mode
    swiper
    which-key
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




;;;; Configure various packages

;;; counsel

(counsel-mode 1)


;;; ivy

(ivy-mode 1)


;;; persp-mode

;; Must set this before turning on persp-mode for it to have an effect
;; at startup.
(setq persp-auto-resume-time 0.1)

(persp-mode 1)


;;; swiper

(bind-key "s-s" 'swiper)


;;; which-key

(which-key-mode 1)


;;; windmove

(windmove-default-keybindings)

(setq windmove-wrap-around t)

(defun my:delete-window-that-direction ()
  "Delete the window in that direction.
The last keyboard event (i.e. the last part of this key binding
for this command) must be an arrow key."
  (interactive)
  (let ((window-to-delete (windmove-find-other-window last-command-event)))
    (when (and window-to-delete
               (not (eq window-to-delete (selected-window))))
      (delete-window window-to-delete))))

(bind-keys ("C-x <up>" . my:delete-window-that-direction)
           ("C-x <down>" . my:delete-window-that-direction)
           ("C-x <left>" . my:delete-window-that-direction)
           ("C-x <right>" . my:delete-window-that-direction))


;;; winner-mode

(winner-mode 1)
