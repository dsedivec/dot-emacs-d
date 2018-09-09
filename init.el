;;;; Prologue

;; Prefer loading a newer .el to an older .elc.  Probably keeps me
;; from getting in trouble if I forget to byte compile.
(setq load-prefer-newer t)

(add-to-list 'load-path
             (expand-file-name "lisp" (file-name-directory load-file-name)))


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
    avy
    bind-key
    counsel
    expand-region
    hydra
    ivy
    magit
    multiple-cursors
    paredit
    persp-mode
    phi-search
    popwin
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

(my:unless-spacemacs
  (define-prefix-command 'my:global-leader-map)
  (bind-key "M-m" 'my:global-leader-map))

(dolist (prefix (string-to-list '("f" "f e" "j")))
  (let ((kbd-str (format "M-m %s" prefix)))
    (unless (global-key-binding (kbd kbd-str))
      (bind-key kbd-str (make-sparse-keymap)))))


;;;; Configure various packages

;;; avy

(bind-keys ("C-'" . avy-goto-char)
           ("M-g g" . avy-goto-line)
           ("M-g M-g" . avy-goto-line))


;;; counsel

(counsel-mode 1)


;;; ediff

(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;;; elisp-mode

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'my:warn-white-space-mode)

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

(bind-keys :map emacs-lisp-mode-map ("C-c C-r" . eval-region))

(my:load-recipe 'indent-elisp-like-common-lisp)


;;; expand-region

(bind-key "M-@" 'er/expand-region)


;;; find-func

(bind-keys ("M-m j f" . find-function)
           ("M-m f e l" . find-library))


;;; imenu

(bind-key "M-m j i" 'imenu)


;;; ivy

(ivy-mode 1)

(bind-key "<f6>" 'ivy-resume)


;;; lisp-mode

(add-hook 'lisp-mode-hook #'paredit-mode)

(defun my:lisp-mode-hook ()
  (setq indent-tabs-mode nil))

(add-hook 'lisp-mode-hook #'my:lisp-mode-hook)


;;; magit

(autoload 'magit "magit" nil t)


;;; multiple-cursors

(bind-keys ("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("<s-mouse-1>" . mc/add-cursor-on-click)
           ("C-?" . mc/mark-all-dwim))

(with-eval-after-load 'multiple-cursors
  (bind-keys :map mc/keymap
             ;; Return should not end multiple-cursors-mode.
             ("<return>" . nil)
             ;; isearch doesn't work with multiple cursors, phi-search is
             ;; the suggested substitute.
             ("C-s" . phi-search)
             ("C-r" . phi-search-backward)))


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


;;; popwin

;; popwin-mode is not autoloaded.
(require 'popwin)
(popwin-mode 1)


;;; prog-mode

(add-hook 'prog-mode-hook #'show-paren-mode)


;;; startup

(setq inhibit-startup-screen t)


;;; swiper

(bind-key "s-s" 'swiper)


;;; tool-bar

(tool-bar-mode -1)


;;; undo-tree

(global-undo-tree-mode)


;;; which-key

(which-key-mode 1)


;;; whitespace

;; This is the default value except when in space visualization mode
;; (the car, not the cdr, used when indent-tabs-mode is non-nil); in
;; that case, only visualize indentation that begins with spaces.
;; Otherwise it'll visualize spaces after initial tabs, which is
;; something I quite like ("smart tabs").
(setq whitespace-indentation-regexp
      '("^\\(\\( \\{%d\\}\\)+\\)[^\n\t]" . "^ *\\(\t+\\)[^\n]"))

(require 'wspc-hydra)

(defun my:warn-white-space-mode ()
  (wspc-hydra-apply-style 'warn-white-space))

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
