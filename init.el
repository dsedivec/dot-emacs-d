;;;; Prologue

;; Prefer loading a newer .el to an older .elc.  Probably keeps me
;; from getting in trouble if I forget to byte compile.
(setq load-prefer-newer t)


;;; Recipes

(defvar my:recipes-dir
  (expand-file-name "recipes" (file-name-directory load-file-name)))

(defun my:load-recipe (recipe)
  (load (expand-file-name (symbol-name recipe) my:recipes-dir)))


;;; Spacemacs compatibility

(defvar my:is-spacemacs (boundp 'dotspacemacs-directory))

(defmacro my:if-spacemacs (then &rest else)
  `(if my:is-spacemacs
       ,then
     ,@else))

(put 'my:if-spacemacs 'common-lisp-indent-function-for-elisp 1)

(defmacro my:when-spacemacs (&rest body)
  `(when my:is-spacemacs ,@body))

(put 'my:when-spacemacs 'common-lisp-indent-function-for-elisp 0)

(defmacro my:unless-spacemacs (&rest body)
  `(unless my:is-spacemacs ,@body))

(put 'my:unless-spacemacs 'common-lisp-indent-function-for-elisp 0)


;;; Customization

;; Set this early before I potentially install packages, which will
;; modify customizable variable `package-selected-packages'.
(setq custom-file (expand-file-name "customizations.el"
                                    (my:if-spacemacs
                                        dotspacemacs-directory
                                      user-emacs-directory)))
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
    undo-tree
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


;;; "Leader" keys setup

(my:if-spacemacs
    (progn
      (defvaralias 'my:global-leader-map 'spacemacs-cmds)
      (defvar my:global-leader-jump-map
        (lookup-key my:global-leader-map (kbd "j"))))

  (define-prefix-command 'my:global-leader-map)
  (bind-key "M-m" 'my:global-leader-map)
  (define-prefix-command 'my:global-leader-jump-map)
  (bind-key "M-m j" 'my:global-leader-jump-map))


;;;; Configure various packages

;;; counsel

(counsel-mode 1)


;;; elisp-mode

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
;; XXX port this, remove conditional
(my:when-spacemacs
  (add-hook 'emacs-lisp-mode-hook #'my:warn-whitespace-mode))

(defun my:emacs-lisp-mode-hook()
  ;; Make name shorter in mode line.
  (setq mode-name "ELisp")
  (setq indent-tabs-mode nil)
  (setq imenu-generic-expression
        (append imenu-generic-expression
                '(("Sections" "^;;;;?\\s-+\\(.*\\)" 1))))
  (my:when-spacemacs
    (smartparens-mode -1))
  ;; XXX
  ;; (add-hook 'completion-at-point-functions
  ;;           #'my:elisp-feature-completion-at-point nil t)
  ;; Trying out case-insensitive dabbrev-code completion in Emacs
  ;; Lisp.  Would have saved me time figuring out why I couldn't
  ;; complete "my:LaTex-" (note lower case "X"--oops).
  (setq-local company-dabbrev-code-ignore-case t))

(add-hook 'emacs-lisp-mode-hook #'my:emacs-lisp-mode-hook)

(my:load-recipe 'indent-elisp-like-common-lisp)


;;; imenu

(my:unless-spacemacs
  (bind-keys :map my:global-leader-jump-map ("i" . imenu)))


;;; ivy

(ivy-mode 1)


;;; lisp-mode

(add-hook 'lisp-mode-hook #'paredit-mode)

(defun my:lisp-mode-hook ()
  (setq indent-tabs-mode nil))

(add-hook 'lisp-mode-hook #'my:lisp-mode-hook)


;;; persp-mode

;; Must set this before turning on persp-mode for it to have an effect
;; at startup.
(setq persp-auto-resume-time 0.1)

(require 'persp-mode)

;; Don't attempt to reactivate persp-mode if it's already active
;; (Spacemacs will have it enabled I believe).  Doing so does weird
;; things.
(unless persp-mode
  (persp-mode 1))


;;; swiper

(bind-key "s-s" 'swiper)


;;; undo-tree

(global-undo-tree-mode)


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
