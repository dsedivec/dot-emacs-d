;; -*- lexical-binding: t; -*-

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
  (cl-assert (symbolp recipe) nil "expected symbol not %S" recipe)
  (load (expand-file-name (symbol-name recipe) my:recipes-dir)))

(defun my:load-recipes (&rest recipes)
  (dolist (recipe recipes)
    (my:load-recipe recipe)))


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

(let ((this-emacs-dir (my:if-spacemacs
                          dotspacemacs-directory
                        user-emacs-directory)))

  ;; Set this early before I potentially install packages, which will
  ;; modify customizable variable `package-selected-packages'.
  (setq custom-file (expand-file-name "customizations.el" this-emacs-dir))
  (load custom-file)

  (add-to-list 'custom-theme-load-path
               (expand-file-name "themes" this-emacs-dir))
  (load-theme 'dsedivec t))


;;; package.el

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives
             '("my:org" . "/Users/dale/repositories/org-mode/"))
(add-to-list 'package-pinned-packages '(org-plus-contrib . "my:org"))

(package-initialize)

(defvar my:packages
  '(
    avy
    bind-key
    clean-aindent-mode
    counsel
    expand-region
    hydra
    ivy
    magit
    multiple-cursors
    org-plus-contrib
    paredit
    persp-mode
    phi-search
    popwin
    smartparens
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

(defun my:package-sync (&optional force-refresh)
  "Install all packages listed by `my:packages'."
  (interactive "p")
  (dolist (pkg my:packages)
    (unless (package-installed-p pkg)
      (when (or force-refresh
                (>= (- (float-time) my:package-last-refresh)
                    my:package-max-age-before-refresh))
        (package-refresh-contents))
      (package-install pkg))))

(my:package-sync)

(my:unless-spacemacs
  (setq package-selected-packages my:packages)
  (package-autoremove))


;;; "Leader" keys setup

(my:unless-spacemacs
  (define-prefix-command 'my:global-leader-map)
  (bind-key "M-m" 'my:global-leader-map))

(dolist (prefix (string-to-list '("f" "f e" "j")))
  (let ((kbd-str (format "M-m %s" prefix)))
    (unless (global-key-binding (kbd kbd-str))
      (bind-key kbd-str (make-sparse-keymap)))))


;;;; Emacs built-ins

(setq ns-use-native-fullscreen nil)


;;;; Configure various packages

;;; autorevert

(global-auto-revert-mode 1)

;;; avy

(bind-keys ("C-'" . avy-goto-char)
           ("M-g g" . avy-goto-line)
           ("M-g M-g" . avy-goto-line))


;;; clean-aindent-mode

(clean-aindent-mode 1)

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


;;; files

(setq make-backup-files nil)


;;; find-func

(bind-keys ("M-m j f" . find-function)
           ("M-m f e l" . find-library))


;;; frame

(bind-key "<s-return>" 'toggle-frame-fullscreen)


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


;;; org

;; Don't step on windmove keys.
(setq org-replace-disputed-keys t)

(when (featurep 'org)
  (warn (concat "org-mode loaded before `org-replace-disputed-keys' set,"
                " windmove will be broken in org-mode buffers")))

(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))

(setq
   ;; This is out of alphabetical order because I use it as part of
   ;; another value, below.
   org-default-notes-file "~/todo.org"
   ;; Tricky: show blocked tasks, don't show sublevels of TODO
   ;; items.  Might want to override this in a custom agenda view.
   org-agenda-dim-blocked-tasks nil
   org-agenda-todo-list-sublevels nil
   org-agenda-show-future-repeats 'next
   org-agenda-start-on-weekday 0
   org-agenda-todo-ignore-deadlines 'far
   org-agenda-todo-ignore-scheduled 1
   ;; I don't want or like org creating bookmarks, especially since
   ;; Bookmark+ then highlights the bookmarks.
   org-bookmark-names-plist nil
   ;; I never use this bookmark, and now with Bookmark+ I seem to be
   ;; getting the last thing I capture visualized, which I actively
   ;; don't want.
   org-capture-bookmark nil
   org-capture-templates '(("t" "Todo" entry
                            (file+headline "~/todo.org" "Inbox")
                            "* NEW %?" :prepend t)
                           ("j" "Journal" entry
                            (file "~/journal.org")
                            "* %U %?" :prepend t)
                           ("s" "Someday" entry
                            (file+headline "~/someday.org" "Someday")
                            "* %?\n  - Created on %U"
                            :prepend t)
                           ("b" "Blog" entry
                            (file "~/Documents/blog/ideas.org")
                            "* %?"
                            :kill-buffer t))
   org-clock-display-default-range 'untilnow
   org-clock-out-remove-zero-time-clocks t
   org-clock-report-include-clocking-task t
   org-default-priority ?D
   org-duration-format '(("h" . t) (special . 2))
   org-enforce-todo-dependencies t
   org-hide-leading-stars t
   org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
   org-link-search-must-match-exact-headline nil
   org-log-into-drawer t
   org-mouse-1-follows-link nil
   ;; Enable this if you want to complete like a path, or disable
   ;; it if you want to use IDO.
   org-outline-path-complete-in-steps nil
   org-pretty-entities t
   org-pretty-entities-include-sub-superscripts nil
   org-refile-use-outline-path t
   ;; Refile/capture to top of tree, not bottom.
   org-reverse-note-order t
   ;; Nice indentation.  Use with visual-line-mode.
   org-startup-indented t
   org-special-ctrl-a/e '(t . reversed)
   org-speed-commands-user '(("a" . org-archive-subtree-default)
                             ("P" . org-set-property)
                             ("S" . widen)
                             ("x" . org-cut-subtree))
   org-use-speed-commands (lambda ()
                            (and (org-at-heading-p)
                                 (looking-back "^\\**"
                                               (line-beginning-position))))
   ;; I think I always want indentation preserved in my source
   ;; blocks.
   org-src-preserve-indentation t
   org-src-window-setup 'other-window
   org-tags-column -76)

;; This actually visits org-default-notes-file, so we don't load this
;; until we really have to.
(with-eval-after-load 'org-agenda
  (setq org-agenda-files (org-add-archive-files (list org-default-notes-file))))

(my:when-spacemacs
    (setq
     ;; Spacemacs turns this on but I don't want/need it.  Plus it makes
     ;; headlipes expand when I mark a task as DONE, which is
     ;; irritating.
     org-log-done nil))

(bind-keys ("C-c r" . org-capture))

(with-eval-after-load 'org
  (bind-keys :map org-mode-map
             ("C-c a" . org-agenda)
             ;; This interferes with avy, and I don't use
             ;; org-cycle-agenda-files anyway.
             ("C-'" . nil)
             ;; Org manual suggests setting this globally, but let's do
             ;; locally in org-mode buffers for now.
             ("C-c l" . org-store-link)
             ;; Yasnippet overrides C-c &, which is the default shortcut
             ;; for this command.  M-* not currently bound in my org-mode
             ;; buffer.
             ("M-*" . org-mark-ring-goto)
             ;; org commit 68b076bf5238 stopped binding C-a/C-e in favor
             ;; of replacing move-beginning-of-line/move-end-of-line
             ;; (function remapping), but Spacemacs doesn't have C-a/C-e
             ;; bound to those in the first place (uses mwim.el instead),
             ;; so that basically breaks C-a/C-e in org.
             ("C-a" . org-beginning-of-line)
             ("C-e" . org-end-of-line))

  (my:when-spacemacs
    ;; M-RET stopped working after org-mode stopped binding M-<return>
    ;; (in favor of M-RET directly) in upstream commit 80cbf909eab.
    ;; Give me back my fucking M-RET, please.  To figure this out I
    ;; had to dive into the bowels of bind-map, but see also/first
    ;; spacemacs/set-leader-keys-for-major-mode, which is where the
    ;; fun starts.  That function is the one called from
    ;; layers/+emacs/org/packages.el.  See also
    ;; https://github.com/syl20bnr/spacemacs/issues/9603.
    (bind-key "M-RET" nil spacemacs-org-mode-map-root-map))

  ;; Allow org-open-at-point (C-c C-o) to open OS X message: links.
  (org-link-set-parameters "message"
                           :follow (lambda (path)
                                     (browse-url (concat "message:" path))))

  ;; Must have org-id loaded if you want org-store-link to set up an
  ;; ID, rather than making a link to the headline text (which is
  ;; inclined to change often).
  (add-to-list 'org-modules 'org-id)
  ;; org-tempo now needed for things like "< e TAB" expanding to an
  ;; example block.
  (add-to-list 'org-modules 'org-tempo)
  ;; This seems to be the most robust way to trigger modules to be
  ;; loaded if they haven't already been loaded, rather than calling
  ;; into what I perceive to be org-mode internals.
  (customize-set-variable 'org-modules org-modules)

  ;; Add a few languages I occasionally want to evaluate in org-mode
  ;; buffers.
  (dolist (language '(shell sql python))
    (setf (alist-get language org-babel-load-languages) t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))

(add-hook 'org-mode-hook #'visual-line-mode)

(defun my:org-mode-hook ()
  ;; Don't know why Spacemacs isn't turning this on for me?
  ;;XXX (setq-local show-trailing-whitespace t)
  ;; Spacemacs turns on hl-todo-mode in text-mode-hook, but that
  ;; totally fucks up my TODO and DONE keywords in org-mode buffers.
  (my:when-spacemacs
    (if (and (boundp 'hl-todo-mode) hl-todo-mode)
        (hl-todo-mode -1)
      (warn (concat "Spacemacs did not turn on hl-todo-mode, maybe"
                    " update my configuration")))))

(add-hook 'org-mode-hook #'my:org-mode-hook)

(my:when-spacemacs
  ;; Spacemacs turns on company-mode in org-mode buffers, which I
  ;; don't like.
  (spacemacs|disable-company org-mode)

  ;; *Org Agenda* buffers are important Spacemacs, let me easily
  ;; switch back to them with C-x b.
  (add-to-list 'spacemacs-useful-buffers-regexp "\\*Org Agenda\\*"))

;; Clock persistence between restarts.
(setq org-clock-persist t
      org-clock-persist-query-resume nil)

(org-clock-persistence-insinuate)

;; Change task state to PENDING when clocking in on a task not already
;; in PENDING.
(defun my:org-switch-state-on-clock-in (current-state)
  (when (and (member current-state '("NEW" "WAITING" "HOLD" "DONE"
                                     "CANCELLED"))
             (member "PENDING" org-not-done-keywords))
    "PENDING"))

(setq org-clock-in-switch-to-state #'my:org-switch-state-on-clock-in)

;; As of 45048eb78 I guess org-end-of-line started working, and now
;; C-e is bringing me to the end of my headline with collapsed
;; content, i.e. before the ellipsis.  This infurates me when I
;; usually want to "C-e M-RET" to add a new sibling headline to the
;; collapsed one.  (Maybe I should come up with a new key
;; combination for that.)  Give me back the old behavior: move to
;; after the ellipsis.
;;
;; Further hack required if you want the 'special behavior of
;; org-special-ctrl-a/e to work right: if we are at end of line
;; you'll never be toggled back over to the end of the headline text
;; if you keep hitting C-e.  This is because org-end-of-line only
;; toggles if you're at the end of the line and not the end of the
;; *visible* line (which, for a collapsed headline, means you're
;; actually some lines further on).  So as a *hack*, move back to
;; the real EOL, before the ellipsis, before executing
;; org-end-of-line.
(define-advice org-end-of-line
    (:around (orig-fun &rest args) my:end-of-line-move-past-ellipsis)
  (when (and (eq this-command last-command)
             (= (point) (save-excursion
                          (end-of-visible-line)
                          (point))))
    (beginning-of-visual-line)
    (end-of-line))
  (prog1
      (apply orig-fun args)
    (when (and (eolp) (org-invisible-p))
      (end-of-visual-line))))

(my:load-recipes 'org-property-drawer-fixes
                 'org-fix-faces-after-goto
                 'org-babel-read-table-in-dblock
                 'org-daily-time-summary
                 'org-columns-delete-property)

;; You can use this as a :formatter in clocktables where you've
;; forcibly narrowed headlines and don't want bracketed links to be
;; truncated, as they often are since the link target (e.g. a URI
;; which doesn't have any spaces) comes first in such a construct.
;; This just converts the link to its descriptive text.
(defun my:org-clocktable-formatter-strip-links (ipos tables params)
  ;; I *think* it's OK to destructively modify TABLES.  I think the
  ;; formatter is the one and only user of this value.  See
  ;; org-dblock-write:clocktable.
  (mapc (lambda (file)
          (mapc (lambda (entry)
                  (setf (nth 1 entry)
                        (replace-regexp-in-string org-bracket-link-regexp
                                                  "\\3" (nth 1 entry))))
                (nth 2 file)))
        tables)
  (funcall org-clock-clocktable-formatter ipos tables params))


;;; paredit

(require 'lisp-comment-dwim)

(with-eval-after-load 'paredit
  (bind-keys :map paredit-mode-map ("M-;" . lisp-comment-dwim)))


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


;;; simple

(column-number-mode 1)


;;; smartparens

;; Need this so ' is configured in lisp modes correctly, so that my
;; lisp-comment-dwim works.
(with-eval-after-load 'smartparens
  (require 'smartparens-config))


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
