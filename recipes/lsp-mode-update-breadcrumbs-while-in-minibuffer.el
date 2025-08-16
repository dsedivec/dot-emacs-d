;; -*- lexical-binding: t; -*-

;; Make the lsp-mode breadcrumbs, showing the context at point
;; (e.g. class and method name), update when you're in the minibuffer.
;; Why would you want this?  Why, because you're *searching*, and you
;; want to know where the result you just landed on is---which method
;; or whatever.

(defvar my:lsp-breadcrumbs-while-in-minibuffer-timer nil)

(defun my:lsp-update-breadcrumbs-while-in-minibuffer ()
  (with-minibuffer-selected-window
    (when (and lsp-mode
               lsp-headerline-breadcrumb-mode)
      (lsp-headerline-check-breadcrumb))))

(defun my:lsp-start-updating-breadcrumbs-while-in-minibuffer ()
  (when (and my:lsp-breadcrumbs-while-in-minibuffer-timer
             (<= (minibuffer-depth) 1))
    (cancel-timer my:lsp-breadcrumbs-while-in-minibuffer-timer)
    (setq my:lsp-breadcrumbs-while-in-minibuffer-timer nil))
  (when (with-minibuffer-selected-window
          (and lsp-mode
               lsp-headerline-breadcrumb-mode))
    (setq my:lsp-breadcrumbs-while-in-minibuffer-timer
          (run-with-idle-timer idle-update-delay t
                               #'my:lsp-update-breadcrumbs-while-in-minibuffer))))

(defun my:lsp-stop-updating-breadcrumbs-while-in-minibuffer ()
  (when (and my:lsp-breadcrumbs-while-in-minibuffer-timer
             (<= (minibuffer-depth) 1))
    (cancel-timer my:lsp-breadcrumbs-while-in-minibuffer-timer)
    (setq my:lsp-breadcrumbs-while-in-minibuffer-timer nil)))

(with-eval-after-load 'lsp-mode
  (add-hook 'minibuffer-setup-hook
            #'my:lsp-start-updating-breadcrumbs-while-in-minibuffer)

  (add-hook 'minibuffer-exit-hook
            #'my:lsp-stop-updating-breadcrumbs-while-in-minibuffer))
