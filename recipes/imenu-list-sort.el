;; -*- lexical-binding: t; -*-

;; Sort imenu-list, with a command to toggle it on and off.

(require 'imenu)

(require 'imenu-list)

(defun my:imenu-sort-index-alist (index-alist predicate)
  (setq index-alist (sort index-alist predicate))
  (mapc (lambda (entry)
          (when (consp (cdr entry))
            (setcdr entry (my:imenu-sort-index-alist (cdr entry) predicate))))
        index-alist)
  index-alist)

(defvar my:imenu-list-sort-function nil)

(defun my:imenu-list-sort (&rest _args)
  (when my:imenu-list-sort-function
    (setq imenu-list--imenu-entries
          (my:imenu-sort-index-alist (copy-tree imenu-list--imenu-entries)
                                     my:imenu-list-sort-function))))

(advice-add 'imenu-list-collect-entries :after #'my:imenu-list-sort)

(defvar my:imenu-list-default-sort-function #'imenu--sort-by-name)

(defun my:imenu-list-toggle-sort (&optional sort-fn)
  (interactive)
  (if my:imenu-list-sort-function
      (progn
        (setq-local my:imenu-list-sort-function nil)
        (message "imenu-list sorting disabled."))
    (setq-local my:imenu-list-sort-function
                (or sort-fn
                    (and (called-interactively-p 'any)
                         current-prefix-arg
                         (intern (completing-read "Sort function: " obarray
                                                  #'fboundp)))
                    my:imenu-list-default-sort-function
                    #'imenu--sort-by-name))
    (message "imenu-list sorted locally using %S."
             my:imenu-list-sort-function))
  (imenu-list-refresh))
