;; -*- lexical-binding: t; -*-

;;;; Prologue

;; Prefer loading a newer .el to an older .elc.  Probably keeps me
;; from getting in trouble if I forget to byte compile.
(setq load-prefer-newer t)

(when (fboundp 'native-compile)
  ;; I believe the primary author of the native-comp branch explicitly
  ;; does not recommend speed 3 for normal use, maybe only for use in
  ;; special cases where extra performance is needed.
  ;;(setq comp-speed 3)

  ;; Emacs is going to be running ld early and often, it needs to run
  ;; the one from MacPorts that might know about libgccjit.  Note that
  ;; this will later be overwritten by my use of exec-path-from-shell.
  (setenv "PATH" (concat "/opt/local/bin:" (or (getenv "PATH") "")))

  ;; Don't pop up *Warnings* for native-comp warnings because the
  ;; async native-comp warnings are far too numerous.
  (with-eval-after-load 'warnings
    (add-to-list 'warning-suppress-types '(comp))))

(defvar my:local-packages-dir (expand-file-name "lisp" user-emacs-directory))

(defvar my:private-lisp-dir (expand-file-name "private" user-emacs-directory))

(add-to-list 'load-path my:local-packages-dir)

(require 'cl-lib)
(require 'subr-x)


;;; Local packages

(defvar my:local-packages-autoload-file
  (expand-file-name "autoloads.el" my:local-packages-dir))

(defvar generated-autoload-file)

(defun my:update-local-package-autoloads ()
  (interactive)
  (let ((kill-buffer-after
         (not (get-file-buffer my:local-packages-autoload-file))))
    (let* ((generated-autoload-file my:local-packages-autoload-file))
      (update-directory-autoloads my:local-packages-dir))
    (when-let ((buf (and kill-buffer-after
                         (get-file-buffer my:local-packages-autoload-file))))
      (kill-buffer buf))
    (load my:local-packages-autoload-file)))

(if (file-exists-p my:local-packages-autoload-file)
    (load my:local-packages-autoload-file)
  (my:update-local-package-autoloads))


;;; "Recipes"

(defvar my:recipes-dir
  (expand-file-name "recipes" (file-name-directory load-file-name)))

(defun my:load-recipe (recipe)
  (cl-assert (symbolp recipe) nil "expected symbol not %S" recipe)
  (load (expand-file-name (symbol-name recipe) my:recipes-dir)))

(defun my:load-recipes (&rest recipes)
  (dolist (recipe recipes)
    (my:load-recipe recipe)))


;;; Add time stamps to *Messages*

;; Loading this early because it can be very useful to see time stamps
;; on messages.
(my:load-recipe 'timestamp-messages)


;;; customize and themes

;; Set this early before I potentially install packages, which will
;; modify customizable variable `package-selected-packages'.
(setq custom-file (expand-file-name "customizations.el" user-emacs-directory))
(load custom-file)

(my:load-recipes 'custom-format-selected-packages
                 'custom-delete-compiled-theme)

(add-to-list 'my:never-compiled-themes 'modus-vivendi)

(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(load-theme 'dsedivec t)

(defun my:set-theme-for-macos-system-theme (&optional toggle)
  (interactive "P")
  (let* ((scpt (concat "tell application \"System Events\" to"
                       " get the dark mode of appearance preferences"
                       " as integer"))
         (emacs-theme (if (cl-equalp (face-background 'default) "white")
                          'light
                        'dark))
         (target-theme (cond
                         (toggle (cl-ecase emacs-theme
                                   (light 'dark)
                                   (dark 'light)))
                         ((zerop (ns-do-applescript scpt))
                          'light)
                         (t 'dark))))
    (message "Configuring Emacs for %s theme" target-theme)
    (cl-assert (memq target-theme '(light dark)))
    (unless (eq emacs-theme target-theme)
      (modify-all-frames-parameters `((ns-appearance . ,target-theme)))
      (invert-face 'default))))

;; My persp-mode frame restoration can end up restoring the initial
;; frame to look however it was when you exited it, even though
;; subsequent frames will be created with my default theme.  This
;; fixes that problem.  Moreover, though, it generally addresses the
;; desire, "I want Emacs to have the proper theme whether I start it
;; during the day or during the night."
(add-hook 'after-init-hook #'my:set-theme-for-macos-system-theme)


;;; package.el with auto-package-update

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives
             '("my:org" . "~/repositories/org-mode/"))
(add-to-list 'package-pinned-packages '(org-plus-contrib . "my:org"))

;; Remove this when Emacs 26 is definitely dead.
(unless package--initialized
  (package-initialize))

;; May not be installed yet.
(require 'auto-compile nil t)

;; We will require 'auto-compile again after we install packages, so
;; this may not run until then.
(with-eval-after-load 'auto-compile
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(defvar my:package-last-refresh 0)

(defvar my:package-max-age-before-refresh 3600)

(define-advice package-refresh-contents
    (:after (&rest _args) my:note-last-refresh-time)
  (setq my:package-last-refresh (float-time)))

(defun my:package-refresh-maybe (&optional force-refresh)
  (when (or force-refresh
            (>= (- (float-time) my:package-last-refresh)
                my:package-max-age-before-refresh))
    (package-refresh-contents)))

;; I always forget to run `package-refresh-contents' before trying to
;; install.
(define-advice package-install (:before (&rest _args) my:package-refresh-maybe)
  (my:package-refresh-maybe))

(defun my:quelpa-git-local-or-github (name &optional repo-name &rest plist)
  "Return Quelpa recipe for package NAME.
REPO-NAME is optional local and GitHub repo name if it is not the
same as NAME."
  (let* ((repo-name (or repo-name (symbol-name name)))
         (local-repo (expand-file-name (concat "~/repositories/" repo-name))))
    (cons name
          (if (file-directory-p local-repo)
              (cl-list* :fetcher 'git :url (concat "file://" local-repo)
                        plist)
            (cl-list* :fetcher 'github :repo (concat "dsedivec/" repo-name)
                      plist)))))

(defvar my:quelpa-packages
  `((bookmark+ :fetcher wiki
               :files ,(cons "bookmark+.el"
                             (mapcar (lambda (suffix)
                                       (format "bookmark+-%s.el" suffix))
                                     '(mac bmu 1 key lit))))
    (hl-line+ :fetcher wiki)
    (blackout :fetcher github :repo "raxod502/blackout")
    ,(my:quelpa-git-local-or-github 'deft)
    ,(my:quelpa-git-local-or-github 'eltu nil
                                    :files '(:defaults "eltu_update_tags.py"))
    ,(my:quelpa-git-local-or-github 'ns-copy-html)
    ,(my:quelpa-git-local-or-github 'python "python-el")
    ,(my:quelpa-git-local-or-github 'smart-tabs)
    ;; I have a fork of sql-indent, hopefully just temporarily.
    ,(my:quelpa-git-local-or-github 'sql-indent "emacs-sql-indent")
    ,(my:quelpa-git-local-or-github 'sticky-region)))

;; Don't update MELPA at startup.
(setq quelpa-update-melpa-p nil)

(defun my:packages-sync (&optional upgrade)
  "Install, (maybe) upgrade, and remove packages.

Selected but missing packages are installed.  Missing packages
from `my:quelpa-packages' are installed.  Packages that are
neither selected nor in `my:quelpa-packages' are removed via
`package-autoremove'.

If UPGRADE is true, or if this command is invoked with a prefix
argument, packages (both package.el and Quelpa) are
upgraded."
  (interactive "P")
  (unless (package-installed-p 'quelpa)
    (package-install 'quelpa))
  (let* ((quelpa-pkg-names
          (mapcar (lambda (pkg)
                    (let ((pkg-name (car pkg)))
                      (condition-case-unless-debug err
                          (quelpa pkg :upgrade upgrade)
                        (t
                         (warn "Quelpa error %s %s: %S"
                               (if upgrade
                                   "installing/upgrading"
                                 "installing")
                               pkg-name
                               err)))
                      pkg-name))
                  my:quelpa-packages))
         ;; Must remove Quelpa packages from `package-activated-list'
         ;; so that `auto-package-update-now' doesn't bitch about not
         ;; recognizing them.
         (package-activated-list (seq-remove (lambda (pkg)
                                               (memq pkg quelpa-pkg-names))
                                             package-activated-list)))
    (when upgrade
      ;; This calls `package-refresh-contents'.
      (auto-package-update-now))
    (unless (seq-every-p #'package-installed-p package-selected-packages)
      (my:package-refresh-maybe)
      (package-install-selected-packages)))
  ;; Interestingly this will not remove Quelpa packages because Quelpa
  ;; puts our requested packages into `package-selected-packages'.  I
  ;; don't know if that's sane, but it works for me right now.
  (package-autoremove))

;; Don't show the package update buffer if nothing was updated.
(define-advice apu--write-results-buffer
    (:around (orig-fun contents &rest args) my:suppress-empty-results-buffer)
  (when (string-match-p "\n" contents)
    (apply orig-fun contents args)))

;; Delete old versions of packages.
(setq auto-package-update-delete-old-versions t)

(my:packages-sync)

;; In case it didn't get loaded before, presumably because it wasn't
;; installed:
(require 'auto-compile)

;; Useful command to upgrade a single Quelpa package.  I should
;; probably make a command to upgrade a single package.el package some
;; day.

(defvar my:quelpa-upgrade--history nil)

(defun my:quelpa-upgrade (package)
  (interactive (list
                (intern (completing-read "Package: "
                                         (mapcar #'car my:quelpa-packages)
                                         nil t ""
                                         'my:quelpa-upgrade--history))))
  (quelpa (assq package my:quelpa-packages) :upgrade t))


;;; Utility functions

(defun my:add-to-list-before (list-var new-element before-element)
  "Conditionally add NEW-ELEMENT to LIST-VAR before BEFORE-ELEMENT.

If NEW-ELEMENT is already in the list then no changes are made,
even if NEW-ELEMENT occurs after BEFORE-ELEMENT.  If
BEFORE-ELEMENT occurs multiple times, NEW-ELEMENT is added before
the first occurrence.  If neither NEW-ELEMENT nor BEFORE-ELEMENT
are in the list, NEW-ELEMENT is added at the end of the list.  If
LIST-VAR is null then it is set to a list containing only
NEW-ELEMENT."
  (let ((list-val (symbol-value list-var))
        target-cell
        last-cell)
    (unless
        (catch 'done
          (while list-val
            (cond
              ((equal (car list-val) new-element)
               (throw 'done t))
              ((and (null target-cell)
                    (equal (car list-val) before-element))
               (setq target-cell list-val)))
            (setq last-cell list-val
                  list-val (cdr list-val))))
      (cond
        (target-cell
         (setf (cdr target-cell) (cons (car target-cell) (cdr target-cell))
               (car target-cell) new-element))
        (last-cell
         (cl-assert (null (cdr last-cell)))
         (setf (cdr last-cell) (list new-element null)))
        (t
         (set list-var (list new-element)))))))

(defmacro my:setq-local (&rest bindings)
  "Like setq for BINDINGS, and make all variables buffer-local."
  (when (= (mod (length bindings) 2) 1)
    (error "`my:setq-local' needs pairs but got odd number of args"))
  `(progn
     ,@(cl-loop for next-binding on bindings by #'cddr
          collect `(set (make-local-variable ',(car next-binding))
                        ,(cadr next-binding)))))

(defun my:add-hooks (hook-var &rest hook-funcs)
  (declare (indent 1))
  (dolist (hook-func (reverse hook-funcs))
    (add-hook hook-var hook-func)))

(defmacro my:with-eval-after-all-load (files &rest body)
  (declare (indent 1))
  (let ((form `(progn ,@body))
        (quoted (when (eq (car files) 'quote)
                  (setq files (cadr files))
                  t)))
    (dolist (file (reverse files) form)
      (setq form `(eval-after-load ,(if (and quoted (symbolp file))
                                        `',file
                                      file)
                    (lambda () ,form))))))

(defun my:minor-mode-arg-will-turn-on (arg mode-currently-on)
  "Returns non-nil if ARG turns on a mode when passed to a minor mode function.
MODE-CURRENTLY-ON is a boolean indicating whether the minor mode
is currently turned on.  Rules for what means \"turn on\" were
taken from the docstring of a minor mode function defined by
`define-minor-mode'."
  (or (and (numberp arg) (> arg 0))
      (null arg)
      (and (eq arg 'toggle) (not mode-currently-on))))

;; When you're truly reluctant to type `(current-buffer)' as that last
;; arg to `buffer-local-value', here's a version that defaults to the
;; current buffer.  It's a generalized variable, too.

(defun my:buffer-local-value (variable &optional buffer)
  "Like `buffer-local-value' but BUFFER defaults to the current buffer."
  (buffer-local-value variable (or buffer (current-buffer))))

(gv-define-setter my:buffer-local-value (val variable &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (set (make-local-variable ,variable) ,val)))

(defun my:get-standard-value (var)
  "Return the standard value of VAR."
  (eval (car (get var 'standard-value))))

(defun my:pop-up-buffer-p (&optional buffer additional-modes)
  (with-current-buffer (or buffer (current-buffer))
    (or (apply #'derived-mode-p additional-modes)
        (and
         ;; `ivy-occur-grep-mode' is derived from `compilation-mode', but
         ;; I don't want to treat it as pop-up.
         (not (derived-mode-p 'ivy-occur-grep-mode))
         (derived-mode-p 'compilation-mode
                         'flycheck-error-list-mode
                         'help-mode
                         'osx-dictionary-mode))
        (equal (buffer-name) "*Buttercup*"))))

(defun my:highlight-line-after-movement ()
  ;; `next-error' is the face that xref uses after jumping.  Good
  ;; enough for xref, good enough for imenu.
  (pulse-momentary-highlight-one-line (point) 'next-error))

;; Repeatable command stuff courtesy Drew Adams,
;; https://www.emacswiki.org/emacs/Repeatable.
(defun my:repeat-command (command)
  "Repeat COMMAND."
  (require 'repeat)
  (let ((repeat-previous-repeated-command  command)
        (repeat-message-function           #'ignore)
        (last-repeatable-command           'repeat))
    (repeat nil)))

(defun my:make-repeatable-command (repeat-cmd-name regular-cmd-name)
  (fset repeat-cmd-name
        (lambda (&rest _)
          (my:repeat-command regular-cmd-name)))
  (put repeat-cmd-name 'interactive-form (interactive-form regular-cmd-name))
  (put repeat-cmd-name 'function-documentation
       (format "Repeatable version of `%S'." regular-cmd-name)))

;; Utilities to edit the mode line using treepy zippers.

(require 'treepy)
(require 'dash)

(cl-defun my:treepy-mode-line-zip (mode-line-spec
                                   &key
                                     visit-eval
                                     visit-properties
                                     (visit-conditional t)
                                     (visit-width t))
  (let ((branchp (lambda (node)
                   (when (listp node)
                     (pcase (car node)
                       (:eval visit-eval)
                       ((pred symbolp) visit-conditional)
                       ((pred integerp) visit-width)
                       (_ t)))))
        (children (lambda (node)
                    (if (and (not visit-properties)
                             (eq (car node) :propertize))
                        (seq-subseq node 0 2)
                      node)))
        (make-node (lambda (old-node children)
                     (if (and (not visit-properties)
                              (eq (car children) :propertize))
                         (append children (seq-subseq old-node 2))
                       children))))
    (treepy-zipper branchp children make-node mode-line-spec)))

(cl-defmacro my:treepy-edit-mode-line-var
    ((mode-line-place zip-var &rest make-zip-args)
                                test-form edit-form
     &optional (result-form nil result-form-p))
  "Edit MODE-LINE-PLACE with a treepy zipper in ZIP-VAR.
The mode line spec at MODE-LINE-PLACE is walked over using
preorder depth-first traversal using treepy.  TEST-FORM is
evaluated with a treepy loc bound to ZIP-VAR at each step.  If
TEST-FORM evaluates to a true value, EDIT-FORM is evaluated, with
the same loc from TEST-FORM still bound to ZIP-VAR.  EDIT-FORM
must result in a new treepy loc, which will then be used to set
the value of MODE-LINE-PLACE (via `setf').  If a match is made
then the result is either RESULT-FORM, if given, or else the
result of `treepy-node' on the current loc after TEST-FORM
returns true, but before EDIT-FORM is evaluated (in other words,
it returns the node that your EDIT-FORM changed)."
  (declare (indent 1))
  (let ((break-sym (gensym)))
    `(let ((,zip-var (my:treepy-mode-line-zip ,mode-line-place
                                              ,@make-zip-args)))
       (catch ',break-sym
         (while (not (treepy-end-p ,zip-var))
           (when ,test-form
             (throw ',break-sym
               (prog1 ,(if result-form-p
                           result-form
                         (list 'treepy-node zip-var))
                 (setf ,mode-line-place
                       (treepy-root ,edit-form)))))
           (setq ,zip-var (treepy-next ,zip-var)))))))


;;;; Emacs built-ins

;; My nav-stack package uses circular lists, try not to stack overflow
;; when I accidentally end up printing one during debugging.  (Why is
;; this nil nby default, anyway?)
(setq print-circle t)

;; Doubling these as I kept running out of undo history (at least, I
;; did with undo-tree).
(setq undo-limit (* 2 80000)
      undo-strong-limit (* 2 120000))

;; Resize windows proportionally for the whole frame, not just the
;; window being split.
(setq window-combination-resize t)

;; Default is to convert yanked text in a search to lower case.  That
;; really screws me up, turn that off.
(setq search-upper-case t)

;; In fact, having a search/replace for "foo bar" → "baz eek" convert
;; to replacing "Foo bar" → "baz eek" seems dangerous, let's just make
;; case folding explicit (M-c).
(setq-default case-fold-search nil)

(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(defun my:show-trailing-white-space ()
  ;; setq-local instead of setq superfluous!
  (setq-local show-trailing-whitespace t))

(add-hook 'text-mode-hook #'my:show-trailing-white-space)

(add-hook 'prog-mode-hook #'my:show-trailing-white-space)

(when (eq window-system 'ns)
  (setq ns-use-native-fullscreen nil
        ;; This defaults to '(t) which tries to use appropriate icons
        ;; from the system in the title bar.  Unfortunately lots of
        ;; these icons on my system are the icon of a blank page, and
        ;; this makes it hard to pick out Emacs in e.g. Witch or other
        ;; application switchers.  Let's just disable file type icons.
        ns-icon-type-alist nil)

  ;; Saving/restoring frames via persp-mode seems to affect this
  ;; somehow.  Setting this seems to put us back where we belong,
  ;; without a friggin' white title bar (WTF).
  (setf (alist-get 'ns-transparent-titlebar default-frame-alist) nil)

  ;; If you google "emacs mac x-colors" you will see lots of people
  ;; commenting that they're missing colors.  I think there's some
  ;; kind of timing bug during build, or something else isn't
  ;; configured right, that leaves you with just the colors from the
  ;; "Developer" colors in your `x-colors' variable.  This is the
  ;; quick-and-dirty fix, I hope.
  (setq x-colors (ns-list-colors))

  ;; Demand higher contrast, particularly when running under
  ;; (invert-face 'default).  This was determined by looking at the
  ;; `color-distance' between the region face's background and
  ;; font-lock-comment-face's foreground, which is currently >150,000
  ;; for me.  For reference, perhaps:
  ;;
  ;;     (color-distance "black" "white") → 589800
  (setq face-near-same-color-threshold 160000)
  ;; Docstring for `face-near-same-color-threshold' says to do this.
  (clear-face-cache))


;;; Mode line mods

;; Don't take up mode line space if encoding is unspecified or Unicode-ish.
(unless
    (my:treepy-edit-mode-line-var
        ((default-value 'mode-line-mule-info) zip)
      (equal (treepy-node zip) "%z")
      (treepy-replace zip
                      `(:eval (let ((coding-info (format-mode-line
                                                  ,(treepy-node zip))))
                                (unless (string-match-p "^[-U]$" coding-info)
                                  coding-info)))))
  (warn "couldn't make \"%%z\" conditional in `mode-line-mule-info'"))

;; Same with EOL, don't need to see it unless it's weird.
(unless
    (my:treepy-edit-mode-line-var
        ((default-value 'mode-line-mule-info) zip)
      (equal (treepy-node zip) '(:eval (mode-line-eol-desc)))
      (treepy-replace zip
                      '(:eval (let ((eol-desc (mode-line-eol-desc)))
                                (unless (equal eol-desc ":")
                                  eol-desc)))))
  (warn "couldn't make EOL info conditional in `mode-line-mule-info'"))

;; Don't take up mode line space if current file is local.  (BTW, %@
;; seems to be undocumented?  Read src/xdisp.c.)
(unless
    (my:treepy-edit-mode-line-var
        ((default-value 'mode-line-remote) zip)
      (equal (treepy-node zip) "%1@")
      (treepy-replace zip
                      ;; FYI this is approximately the same logic as
                      ;; src/xdisp.c uses.
                      `(:eval (when (and (stringp default-directory)
                                         (file-remote-p default-directory))
                                ,(treepy-node zip)))))
  (warn "couldn't make remote indicator conditional in `mode-line-remote'"))

;; Move the buffer percentage way over to the right on the mode line.
(let ((percent-spec
       (my:treepy-edit-mode-line-var
           (mode-line-position zip)
         (pcase (treepy-node zip)
           (`(:propertize mode-line-percent-position . ,_) t))
         (treepy-remove zip))))
  (if (not percent-spec)
      (warn (concat "couldn't find `mode-line-percent-position'"
                    " in `mode-line-position'"))
    (unless (my:treepy-edit-mode-line-var
                ((default-value 'mode-line-format) zip)
              (eq (treepy-node zip) 'mode-line-end-spaces)
              (treepy-insert-left zip percent-spec))
      (warn (concat "couldn't find `mode-line-end-spaces',"
                    " appending percentage to mode line"))
      ;; We already removed it from `mode-line-position', above.
      ;; Need to put it somewhere so it's not totally lost.
      (nconc mode-line-format (list percent-spec)))))

;; Remove extra spaces between buffer ID and position.
(unless
    (my:treepy-edit-mode-line-var
        ((default-value 'mode-line-format) zip)
      (let ((node (treepy-node zip)))
        (and (stringp node)
             (string-match-p "^ \\{2,\\}$" node)
             (eq (-some-> zip
                   treepy-right
                   treepy-node)
                 'mode-line-position)))
      (treepy-replace zip " "))
  (warn "couldn't remove spaces before buffer position in mode line"))

;; I am no longer seeing excessive spacing around the (line,col)
;; position information in Emacs 28.0.50.  Check (probably) new
;; variable `mode-line-position-column-line-format' if it becomes a
;; problem again.
(when (< emacs-major-version 28)
  ;; Remove fixed width for position output, causes too much extra
  ;; space after (line,col) in mode line.
  (unless
      (my:treepy-edit-mode-line-var
          (mode-line-position zip)
        ;; Note there is also a %C variant which I don't use.
        (and (equal (treepy-node zip) " (%l,%c)")
             (treepy-up zip)
             (-some-> zip treepy-left treepy-node numberp))
        (treepy-replace (treepy-up zip) (treepy-node zip)))
    (warn "couldn't remove width specification from `mode-line-position'")))


;;;; Configure various packages

;;; which-key

(which-key-mode 1)

(my:load-recipes 'which-key-some-prefixes-are-fast)

;; `defun' by default, which seems a little weird.  I prefer this
;; indentation.
(put 'which-key-add-key-based-replacements 'lisp-indent-function 0)

;; Fill in some explanations for Emacs built-in prefixes, or prefixes
;; that are kind of shared like C-x r.
(which-key-add-key-based-replacements
  "C-x 4" "other window"
  "C-x 5" "other frame"
  "C-x n" "narrow"
  "C-x r" "register/rectangle"
  "C-x w" "highlight/winum")


;;; "Leader" keys setup

;; Inspired by Spacemacs.

(define-prefix-command 'my:global-leader-map)

(bind-key "M-m" 'my:global-leader-map)

(which-key-add-key-based-replacements
  "M-m a" "apps"
  "M-m h" "help"
  "M-m i" "insert"
  "M-m j" "jump/join"
  "M-m m" "major mode"
  "M-m s" "search"
  "M-m u" "utils"
  "M-m v" "vc"
  "M-m x" "smartparens")


;;; wspc-hydra

;; This package is out of order because I use
;; `my:warn-white-space-mode' all over the place.

(setq wspc-hydra-buffer-local-whitespace-style t)

(defun my:warn-white-space-mode ()
  (wspc-hydra-apply-style 'warn-white-space))

(defun my:warn-whitespace-mode (&optional on)
  (cl-assert (or (null on) (numberp on)))
  (if (or (null on) (> on 0))
      (my:warn-white-space-mode)
    (whitespace-mode -1)))

(make-obsolete 'my:warn-whitespace-mode 'my:warn-white-space-mode "2018-09-23")


;;; exec-path-from-shell

;; This package is out of alphabetical order in this file because it's
;; probably important to get `exec-path' set early before you, for
;; example, try to launch a spelling checker via `flyspell-mode'.

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;;; el-patch

;; This is out of alphabetical order so it gets installed before
;; anyone potentially starts using el-patch.

(my:load-recipe 'el-patch-clean-up-buffers-after-validation)


;;; AUCTeX, RefTeX, and other LaTeX-related stuff

(setq TeX-newline-function 'newline-and-indent
      ;; "AUCTeX depends heavily on being able to extract information
      ;; from the buffers by parsing them.  Since parsing the buffer
      ;; can be somewhat slow, the parsing is initially disabled.  You
      ;; are encouraged to enable them by adding the following lines
      ;; to your '.emacs' file."
      TeX-parse-self t
      TeX-auto-save t)

(setq LaTeX-includegraphics-read-file
      #'LaTeX-includegraphics-read-file-relative)

(setq font-latex-fontify-sectioning 1.3)

(with-eval-after-load 'font-latex
  (font-latex-update-sectioning-faces))

;; Let company-mode start idle completion after typing a hyphen, such
;; as in "\gls{foo-".
(put 'LaTeX-babel-insert-hyphen 'company-begin t)

(with-eval-after-load 'latex
  (my:load-recipes 'auctex-aggressively-load-styles
                   'auctex-auto-braces-mode
                   'auctex-glossaries-package
                   'auctex-normalize-space-in-outline-name
                   'auctex-wrap-in-gls-commands)

  (with-eval-after-load 'company
    (my:load-recipes 'auctex-company-glossaries-backend))

  (bind-keys :map LaTeX-mode-map
             ;; Spacemacs overrides the default LaTeX-insert-item binding
             ;; on M-RET, but we can put it on <M-S-return>.
             ("<M-S-return>" . LaTeX-insert-item)
             ;; I don't actually use glossaries that much these days, but
             ;; I'm keeping these utility functions and their bindings
             ;; around for future reference, I guess.
             ("C-c g" . my:LaTeX-convert-to-gls)
             ("C-c M-g" . my:LaTeX-backward-convert-to-gls)))

(underlings-move-menu-after-load 'latex 'LaTeX-mode
                                 '["Preview" "Command"]
                                 "LaTeX")

(with-eval-after-load 'company-reftex
  (setq company-reftex-labels-regexp
        (concat company-reftex-labels-regexp
                ;; I should upstream these additions.
                "\\|\\\\nameref{\\(?1:[^}]*\\)\\="
                "\\|\\\\hyperref\\[\\(?1:[^]]*\\)\\=")))

(defun my:LaTeX-mode-hook ()
  (my:setq-local er/try-expand-list (append er/try-expand-list
                                            '(mark-sentence mark-paragraph))

                 electric-pair-inhibit-predicate
                 #'my:electric-pair-default-plus-before-word-inhibit)
  (set (make-local-variable 'company-backends)
       (append '((company-yasnippet
                  company-reftex-labels
                  company-reftex-citations
                  company-auctex-macros
                  company-auctex-symbols
                  company-auctex-environments
                  my:company-auctex-glossaries)
                 company-dabbrev-code)
               company-backends)))

(my:add-hooks 'LaTeX-mode-hook
  #'reftex-mode
  #'company-mode
  #'electric-pair-local-mode
  #'flycheck-mode
  #'show-paren-mode
  #'auto-fill-mode
  #'outline-minor-mode
  #'my:warn-white-space-mode
  #'my:LaTeX-mode-hook)

;; This may be a problem if reftex gets loaded after AUCTeX.
(setq reftex-plug-into-AUCTeX t)

;; Change RefTeX's parameters for generating labels from
;; e.g. section titles.  List of articles and prepositions are
;; RefTeX's defaults.
(setq reftex-derive-label-parameters
      '(100 80 t nil "-"
        ("the" "on" "in" "off" "a" "for" "by" "of" "and" "is" "to")
        t))

(with-eval-after-load 'reftex
  ;; Teach RefTeX about hyperref's \nameref command.  (Really in the
  ;; nameref package now, but I have yet to load nameref explicitly,
  ;; letting hyperref do it for me.  Plus RefTeX already has commands
  ;; for hyperref, as you can see.)
  (let ((hyperref-commands (nth 2 (assoc "Hyperref" reftex-ref-style-alist))))
    (unless (assoc "\\nameref" hyperref-commands)
      ;; I'm being lazy here.
      (unless (> (length hyperref-commands) 1)
        (error "Oops, can't throw away `nconc' result anymore"))
      (nconc hyperref-commands '(("\\nameref" ?n))))))

(underlings-move-menu-after-load 'reftex 'reftex-mode "Ref"
                                 "LaTeX" :dest-map 'LaTeX-mode-map)


;;; abbrev

(with-eval-after-load 'abbrev
  (add-to-list 'which-key-replacement-alist
               '(("C-x a" . "Prefix Command") . (nil . "abbrev"))))



;;; ace-window

(with-eval-after-load 'ace-window
  (set-face-attribute 'aw-leading-char-face nil :height 10.0))

(bind-key "C-x o" 'ace-window)


;;; adaptive-wrap

(defun my:turn-on-adaptive-wrap-prefix-mode ()
  ;; This mode screws up `org-indent-mode'.
  (unless (memq major-mode '(org-mode))
    (adaptive-wrap-prefix-mode 1)))

(add-hook 'visual-line-mode-hook #'my:turn-on-adaptive-wrap-prefix-mode)


;;; aggressive-indent

(with-eval-after-load 'aggressive-indent
  (my:load-recipes 'aggressive-indent-fix-timer))


;;; all-the-icons

;; These apparently don't get autoloaded.
(autoload 'all-the-icons-alltheicon "all-the-icons")
(autoload 'all-the-icons-fileicon "all-the-icons")


;;; amx

(setq amx-history-length 500)


;;; anzu

(global-anzu-mode 1)


;;; atomic-chrome

(atomic-chrome-start-server)


;;; auto-highlight-symbol

(defun my:ahs-inhibit-multiple-cursors (_symbol)
  "Don't highlight symbols when I'm using multiple cursors.
Makes it hard to use things like `mc/mark-more-like-this-extended'."
  (bound-and-true-p multiple-cursors-mode))

(setq ahs-exclude #'my:ahs-inhibit-multiple-cursors)

(add-hook 'prog-mode-hook #'auto-highlight-symbol-mode)

(with-eval-after-load 'auto-highlight-symbol
  ;; Must trigger defcustom's :set.
  (customize-set-variable 'ahs-idle-interval 0.5)

  (add-to-list 'ahs-plugin-bod-modes 'python-mode)

  (add-to-list 'ahs-inhibit-face-list 'font-lock-keyword-face)

  ;; Don't sit on `negative-argument'.  (Particularly since winum is
  ;; sitting on C-<digit>, so I never use C-- to get at
  ;; `negative-argument'.)
  (unbind-key "M--" auto-highlight-symbol-mode-map)

  ;; Black on DodgerBlue is too hard to read, remove the :foreground
  ;; and I get white on DodgerBlue instead.
  (set-face-attribute 'ahs-plugin-bod-face nil :foreground nil))

(defun my:ahs-set-default-range-to-defun ()
  (ahs-change-range 'ahs-range-beginning-of-defun t))

(add-hook 'emacs-lisp-mode-hook #'my:ahs-set-default-range-to-defun)

;; Prevent auto-highlight-symbol from highlighting while in the middle
;; of `avy-goto-char'---and then forgetting to re-highlight when
;; `avy-goto-char' ends.  Of course, I'm sure this very general advice
;; will apply to other commands that sit idle waiting for input; we'll
;; see if this needs to be narrowed.

(defun my:ahs-dont-highlight-while-command-in-progress (&rest _args)
  ;; https://emacs.stackexchange.com/a/20126
  (not this-command))

(advice-add 'ahs-highlight-p :before-while
            #'my:ahs-dont-highlight-while-command-in-progress)


;;; auto-yasnippet

;; Bindings copied from Spacemacs
(bind-keys
 ("M-m i c" . aya-create)
 ("M-m i e" . aya-expand)
 ("M-m i w" . aya-persist-snippet))

(with-eval-after-load 'auto-yasnippet
  (add-to-list 'which-key-replacement-alist
               '(("M-m i S" . "Prefix Command") . (nil . "auto-yasnippet"))))


;;; autorevert

(setq global-auto-revert-non-file-buffers t)

(global-auto-revert-mode 1)

;; Don't need an echo area message every time a buffer (including
;; "TAGS"!) is reverted.
(setq auto-revert-verbose nil)


;;; avy

(bind-keys ("C-'" . avy-goto-char)
           ("M-g g" . avy-goto-line)
           ("M-g M-g" . avy-goto-line))


;;; bibtex

(setq bibtex-comma-after-last-field t
      bibtex-dialect 'biblatex
      bibtex-maintain-sorted-entries t)

(with-eval-after-load 'bibtex
  (mapc (apply-partially #'add-to-list 'bibtex-entry-format)
        '(realign last-comma whitespace unify-case braces sort-fields))

  (bind-keys :map bibtex-mode-map
             ("C-c C-e o" . bibtex-Online)))


;;; blackout

;; No autoloads.
(require 'blackout)

(blackout 'python-mode "Py")


;;; bookmark

;; My hints for using bookmarks:
;;
;; * C-x r m: set bookmark *at point*
;; * C-x r l or C-x p e: show bookmark list
;; * (In bookmark list) C-u a: edit annotation
;; * C-x p s: save bookmarks file (should also happen at exit)

(my:load-recipes 'bookmark-auto-save)


;;; bookmark+

;; I require all of bookmark+ to get its keys loaded.  bookmark+ also
;; sets up a mode hook that tries to change menus in `dired-mode-map',
;; menus which I move elsewhere.  Best of all, the mode hook is a
;; lambda, so I can't get at it by name.  Therefore I engage in the
;; following horrible, awful maneuver to try and keep the hook happy.

(if (featurep 'bookmark+)
    (warn "Too late, bookmark+ loaded, dired menus are screwed")

  (add-hook
   'dired-mode-hook
   (underlings-define-menu-mover 'dired-mode "Subdir" "Dired"
                                 :func-name 'my:dired-bmkp-hide-subdir-menu))

  (require 'bookmark+)

  (add-hook
   'dired-mode-hook
   (underlings-define-menu-mover 'dired-mode
                                 '[("Dired" "Subdir")]
                                 nil
                                 :func-name 'my:dired-bmkp-show-subdir-menu)))

(setq bmkp-auto-light-when-set 'all-in-buffer
      bmkp-auto-light-when-jump 'any-bookmark)

(with-eval-after-load 'which-key
  (add-to-list 'which-key-replacement-alist
               '(("C-x p c" . "Prefix Command") . (nil . "create")))
  (add-to-list 'which-key-replacement-alist
               '(("C-x p t" . "Prefix Command") . (nil . "tags"))))

(my:load-recipes 'bookmark+-speed-up)


;;; browse-url

;; OK, this doesn't really belong here, probably.  `my:find-url' only
;; came into existence because I really needed `browse-url-emacs' one
;; day, but it was broken in Emacs master.  See comments in recipe for
;; more details.

(autoload 'my:find-url (expand-file-name "find-url" my:recipes-dir)
  "Load a URL into a buffer."
  t)


;;; bs

(bind-key "C-x C-b" 'bs-show)

(with-eval-after-load 'persp-mode
  (my:load-recipes 'bs-persp-mode))

(defun my:visits-non-dired-non-file (buffer)
  "Returns T if buffer is neither a file nor a Dired buffer."
  (and (bs-visits-non-file buffer)
       (not (eq (buffer-local-value 'major-mode buffer) 'dired-mode))))

(with-eval-after-load 'bs
  (bind-keys :map bs-mode-map
             ("k" . bs-up)
             ("j" . bs-down)
             ;; t is bound to bs-visit-tags-table by default.  I manage to
             ;; hit this often enough when I hit C-x C-b by accident
             ;; instead of C-x b, then start typing "todo" to switch to my
             ;; to-do list.  The result is that I unwittingly have turned
             ;; off undo in the buffer, since bs-visit-tags-table seems to
             ;; set buffer-undo-list to t.  I never use this
             ;; functionality, so let's just stop me from hurting myself.
             ("t" . nil))

  ;; Dired+ names buffers differently, have to tell bs about these names.
  (add-to-list 'bs-mode-font-lock-keywords
               '("..\\(.*Dired/.*\\)$" 1 font-lock-function-name-face))

  (add-to-list 'bs-configurations
               '("files-and-dirs" nil nil nil my:visits-non-dired-non-file
                 bs-sort-buffer-interns-are-last))

  (my:load-recipes 'bs-abbreviate-file-names))

(setq bs-default-configuration "files-and-dirs")

(setq my:bs-file-name-abbrev-alist
      `((,(concat "\\`" (regexp-quote (expand-file-name "~")) "/git/pippin/")
          . ":pippin:/")))


;;; buttercup

(bind-keys :map emacs-lisp-mode-map
           ("M-m m t" . buttercup-run-at-point))

;; `buttercup-run-at-point' does `save-selected-window' so Shackle's
;; :select t can't help us here.
(define-advice buttercup-run-at-point
    (:after (&rest _args) my:buttercup-select-results)
  (when-let ((win (get-buffer-window "*Buttercup*")))
    (select-window win)))


;;; carousel

(carousel-mode 1)


;;; cider

(my:add-hooks 'cider-mode-hook
  #'eldoc-mode)

(my:add-hooks 'cider-repl-mode-hook
  #'paredit-mode)

(with-eval-after-load 'cider-mode
  (bind-keys :map cider-mode-map
             ;; Conflicts with nav-stack.
             ("M-," . nil)
             ;; Conflicts with link-hint.
             ("C-c C-o" . nil)))

(underlings-move-menu-after-load 'cider-mode 'cider-mode
                                 ["CIDER Eval" "CIDER Interactions"]
                                 "CIDER")

;; CIDER has an autoload form that uses `with-eval-after-load' to put
;; sesman into `clojure-mode-map' after `clojure-mode' has been
;; loaded.  Just use a one-time hook on `clojure-mode-hook' to avoid
;; any kind of loading order problems with moving this menu item.
(underlings-move-menu-with-one-time-hook 'clojure-mode ["Sesman"] "Clojure")

(underlings-move-menu-after-load 'cider-repl 'cider-repl-mode
                                 ["Sesman"]
                                 "REPL")


;;; cl-indent

;; These variables claim to have been introduced in 28.1 and their
;; default value is 6, which looks whack to me, even/especially in
;; Elisp.

(setq lisp-loop-keyword-indentation 2
      lisp-loop-forms-indentation 3)


;;; clean-aindent-mode

(clean-aindent-mode 1)

(bind-keys :map clean-aindent-mode--keymap
           ([remap backward-kill-word] . nil))


;;; clojure-mode

(defun my:clojure-mode-hook ()
  ;; CIDER's CAPF is fine, but it only completes things that have been
  ;; eval'ed, which is really annoying.  (Is this a bug in CIDER?)
  (my:company-group-existing-backend 'company-capf '(company-dabbrev-code)))

(my:add-hooks 'clojure-mode-hook
  #'paredit-mode
  #'aggressive-indent-mode
  #'eldoc-mode
  #'my:clojure-mode-hook)


;;; comint

(setq comint-scroll-to-bottom-on-input 'this
      comint-input-ignoredups t)


;;; comment-dwim-2

(setq cd2/region-command #'cd2/comment-or-uncomment-region)

(bind-keys ("M-;" . comment-dwim-2))


;;; company

(add-hook 'prog-mode-hook #'company-mode)

(setq company-selection-wrap-around t
      company-show-numbers t
      company-tooltip-align-annotations t
      company-minimum-prefix-length 2
      company-idle-delay 0.2
      ;; Pretty sure I want company-dabbrev-code to search all
      ;; buffers, not even just buffers of the same major mode.
      ;; (Consider, for example, sql-interactive-mode vs. sql-mode.)
      ;; Also search *SQL* buffers, damn it.
      company-dabbrev-ignore-buffers "\\` "

      company-occurrence-weight-function
      'company-occurrence-prefer-any-closest)

;; The default for `company-backends' seems a bit insane.  It includes
;; things like `company-bbdb' (I don't use BBDB) and `company-eclim'
;; (sorry, JetBrains IDE is too good for Java compared to Emacs) that
;; I will never care to use.  I'm sure they're fast to skip, but why
;; leave them there at all?  Going to take some out.  Put them back in
;; as buffer-local values if/where needed.
(with-eval-after-load 'company
  (setq company-backends
        (cl-delete-if (lambda (elt)
                        (member elt '(company-bbdb company-eclim
                                      company-semantic company-clang
                                      company-xcode company-cmake
                                      company-oddmuse)))
                      company-backends))

  (with-eval-after-load 'counsel
    (define-key company-mode-map [remap completion-at-point] 'counsel-company)
    (define-key company-mode-map [remap complete-symbol] 'counsel-company)

    (if (and (listp company-continue-commands)
             (eq (car company-continue-commands) 'not))
        ;; C-M-i will be `flyspell-auto-correct-word' if you have
        ;; `flyspell-prog-mode' on, but if it has nothing to do then
        ;; it'll go to call whatever C-M-i was previously bound to,
        ;; i.e. `counsel-company'.
        (dolist (cmd '(counsel-company flyspell-auto-correct-word))
          (cl-pushnew cmd (cdr company-continue-commands)))
      (warn (concat "`company-continue-commands' is no longer a `not' list,"
                    " can't make `counsel-company' end completion")))))

(my:load-recipes 'company-dont-complete-numbers
                 'company-dabbrev-code-work-with-other-prefixes
                 'company-remove-dabbrev-code-duplicates
                 'company-complete-or-other-backend)

(with-eval-after-load 'company
  ;; Among a few other things this makes it so TAB completes, not RET,
  ;; which I really need because it's annoying to hit enter at EOL to
  ;; insert a newline and instead a completion slips in instead.
  ;;
  ;; NOTE: Turning on `company-tng-mode' makes many changes, such as
  ;; changes to `company-active-map', that are NOT reversed by
  ;; turning it off.
  (company-tng-mode 1)

  (bind-keys :map company-mode-map
             ("<C-return>" . my:company-complete-or-other-backend)
             :map company-active-map
             ;; If you don't unbind these then hitting e.g. <down>
             ;; will either select the next candidate for completion,
             ;; or else it will FUCKING ABORT COMPLETION, neither of
             ;; which is what I want in tng mode.  (Sorry, but I'm mad
             ;; because it took me way too long to figure out that
             ;; company-mode has <up> and <down> behave differently
             ;; when there's only one candidate.)
             ;;
             ;; With these unbound you can hit <up> or <down> to move
             ;; the cursor, leaving a selected candidate if any.
             ("<down>" . nil)
             ("<up>" . nil))

  ;; C-<digit> to select a completion
  (dotimes (n 10)
    (bind-key (kbd (format "C-%d" n)) 'company-complete-number
              company-active-map)))

(defun my:company-group-existing-backend (backend other-backends
                                          &optional globally no-warn)
  "Group BACKEND with OTHER-BACKENDS."
  (let* ((backends (copy-tree company-backends))
         (cell (memq backend backends)))
    (if (not cell)
        (unless (or no-warn
                    (seq-some (lambda (group)
                                (and (consp group)
                                     (memq backend group)
                                     (seq-every-p (lambda (other-backend)
                                                    (memq other-backend group))
                                                  other-backends)))
                              backends))
          (warn "Couldn't group %S with %S" other-backends backend))
      (setf (car cell) (cons backend other-backends))
      (unless globally
        (make-local-variable 'company-backends))
      (setq company-backends backends))))


;;; conf-mode

;; conf-mode doesn't derive from anything.  Run prog-mode-hook by
;; hand.  What could go wrong?
(defun my:conf-mode-hook ()
  (run-hooks 'prog-mode-hook))

(add-hook 'conf-mode-hook #'my:conf-mode-hook)

(add-hook 'conf-mode-hook #'faux-indent-mode)

(with-eval-after-load 'conf-mode
  (bind-keys :map conf-mode-map
             ("RET" . newline-and-indent)))


;;; counsel

(counsel-mode 1)

(setq counsel-find-file-ignore-regexp
      (rx (regexp (regexp-opt completion-ignored-extensions)) eos))

;; Include directory in prompt when searching.
(ivy-set-prompt 'counsel-ag #'counsel-prompt-function-dir)

(my:load-recipes 'counsel-limit-grep-result-length
                 'counsel-trace-function
                 'counsel-git-grep-use-re-builder)

;; `counsel-find-file' doesn't leave the file you just found as the
;; current buffer.  This is because `counsel-find-file' →
;; `counsel-find-file-action' → `with-ivy-window' →
;; `with-selected-window' → `save-current-buffer'.  Your window does
;; end up showing the new buffer, but Emacs's "current buffer" is
;; still your old buffer during `post-command-hook', which breaks
;; nav-stack.  I should probably report this upstream: it seems
;; harmless but I bet it could break more than just nav-stack.  (OTOH,
;; changing its behavior now might break even more...?)
(define-advice counsel-find-file-action
    (:after (&rest args) my:set-current-buffer-like-find-file)
  (set-buffer (with-ivy-window (window-buffer))))


;;; counsel-auto-grep

(with-eval-after-load 'counsel
  (bind-keys :map counsel-mode-map
             ("M-m /" . counsel-auto-grep-maybe-projectile)
             ("M-m s f" . counsel-auto-grep-ask-dir)))


;;; counsel-css

(add-hook 'css-mode-hook #'counsel-css-imenu-setup)


;;; counsel-projectile

(with-eval-after-load 'projectile
  (counsel-projectile-mode 1))


;;; cperl-mode

(add-to-list 'auto-mode-alist '("\\.pl\\'" . cperl-mode))


;;; crux

;; Make `crux-smart-open-line-above' take a prefix arg to open
;; multiple lines, so that I can use it as a substitute for
;; `open-line', below.
(defun my:crux-smart-open-line-above-repeatable (&optional n)
  (interactive "*p")
  (dotimes (_ n)
    (crux-smart-open-line-above)))

;; Remap `open-line' when used interactively.
(bind-keys ([remap open-line] . my:crux-smart-open-line-above-repeatable))

;; Now remap uses of `open-line' inside other functions.

(defun my:redefine-open-line-to-crux-smart-open-line (orig-fun &rest args)
  (cl-letf* (((symbol-function 'open-line)
              #'my:crux-smart-open-line-above-repeatable))
    (apply orig-fun args)))

(dolist (func '(aya-open-line org-open-line))
  (advice-add func :around #'my:redefine-open-line-to-crux-smart-open-line))


;;; css-mode

;; `css-fill-paragraph' narrows comments, cutting off any indentation
;; on the first line, which can result in a stupidly-long first line.
;; We'll work around this by first de-denting the region if it's a
;; comment.

(defun my:css-mode-fill-paragraph-indented-comments (orig-fun &rest args)
  (let (line-start-mark indent end-mark)
    (cond
      ((save-excursion
         (and (integerp fill-column)
              (progn
                (let ((ppss (syntax-ppss)))
                  (when (nth 4 ppss)
                    (goto-char (nth 8 ppss))))
                (beginning-of-line)
                (setq line-start-mark (point-marker)
                      indent (current-indentation))
                (forward-comment 1))
              (setq end-mark (point-marker))))
       (prog1
           (save-excursion
             (indent-rigidly line-start-mark end-mark (- indent))
             (unwind-protect
                  (let ((fill-column (max (- fill-column indent) 1)))
                    (apply orig-fun args))
               (indent-rigidly line-start-mark end-mark indent)))
         ;; When you M-q at the starting "/" of an indented comment,
         ;; point gets moved to BOL for some reason, so move point
         ;; somewhere more useful.
         (when (bolp)
           (back-to-indentation))))
      (t
       (apply orig-fun args)))))

(advice-add 'css-fill-paragraph :around
            #'my:css-mode-fill-paragraph-indented-comments)


;;; deft

(setq deft-directory "~/Dropbox/dropsync/Notes"
      deft-recursive t
      deft-default-extension "md"
      deft-use-filename-as-title t
      deft-use-filter-string-for-filename t)


;;; delsel

(delete-selection-mode 1)


;;; descr-text

(bind-keys ("M-m h d c" . describe-char))


;;; diff-hl

(global-diff-hl-mode 1)

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)


;;; dired-ranger

(with-eval-after-load 'dired
  (bind-keys :map dired-mode-map
             ;; Taken from
             ;; http://pragmaticemacs.com/emacs/copy-and-paste-files-with-dired-ranger/.
             ;; These actually overwrite some default bindings from
             ;; `dired-mode', so I'm not sure if this is a good idea.
             ;; Might want to change these.
             ("W" . dired-ranger-copy)
             ("X" . dired-ranger-move)
             ("Y" . dired-ranger-paste)))


;;; dired

;; Combine all bordering-on-abusively-numerous dired menus into a
;; single menu item.  macOS users rejoice.  Note that I also move the
;; "Subdir" menu, but I have to do that in a stupid way because of
;; bookmark+; see my init code for that package elsewhere in this file
(underlings-move-menu-with-one-time-hook 'dired-mode
                                         ;; Quoted for indentation, sigh.
                                         '["Operate" "Mark" "Regexp"
                                           "Immediate"]
                                         "Dired")


;;; dired-x

;; Binds C-x C-j.  Probably does other stuff I care about.
(require 'dired-x)


;;; dtrt-indent

;; Do this before dtrt-indent gets loaded, hopefully.
(setq dtrt-indent-active-mode-line-info nil)

(setq dtrt-indent-min-quality 70.0)

;; I get real annoyed with dtrt-indent doesn't engage in sh-mode
;; buffers, just because sh-mode uses SMIE.
(setq dtrt-indent-run-after-smie t)

(dtrt-indent-global-mode 1)

;; If dtrt-indent changed buffer settings like indent-tabs-mode,
;; whitespace-mode may/probably will need to be reset.
(define-advice dtrt-indent-try-set-offset
    (:after (&rest args) my:dtrt-indent-reset-whitespace-mode)
  (when (and (boundp 'whitespace-mode) whitespace-mode)
    (whitespace-mode -1)
    (whitespace-mode 1)))


;;; dumb-jump

(setq dumb-jump-selector 'ivy)

;; `dumb-jump-default-project' seems to default to "~", which I think
;; means it will search your entire home directory by default?  That
;; is... an interesting choice, one that I wish to avoid, personally.
(setq dumb-jump-default-project nil)

(when (executable-find "rg")
  ;; Per the docs, prefer rg, just in case ag is installed.  (ag's
  ;; gitignore handling is still unsatisfactory.)
  (setq dumb-jump-prefer-searcher 'rg))

(my:load-recipes 'dumb-jump-only-if-no-tags)


;;; edebug

(with-eval-after-load 'edebug
  (add-to-list 'which-key-replacement-alist
               '(("C-x X" . "Prefix Command") . (nil . "edebug"))))


;;; ediff

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; I think `ediff-setup-windows-plain' really does not like it when
;; you have `window-combination-resize' turned on.  I should probably
;; report this upstream as a bug.  To reproduce, just turn on
;; `window-combination-resize' and then invoke, say, ediff from Magit.
(define-advice ediff-setup-windows-plain
    (:around (orig-fun &rest args) my:disable-window-combination-resize)
  (let ((window-combination-resize nil))
    (apply orig-fun args)))


;;; edit-indirect

(bind-keys ("C-c '" . edit-indirect-region))

(my:load-recipes 'edit-indirect-copy-vars-from-parent)

(defun my:edit-indirect-guess-mode (parent-buffer _beg _end)
  (my:edit-indirect-set-up-cloned-vars parent-buffer)
  (normal-mode))

(setq edit-indirect-guess-mode-function #'my:edit-indirect-guess-mode)


;;; elec-pair

;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=44245

(defun my:electric-pair-wont-use-region-if-mode-is-not-on (orig-fun &rest args)
  (and electric-pair-mode
       (apply orig-fun args)))

(advice-add 'electric-pair-will-use-region :around
            #'my:electric-pair-wont-use-region-if-mode-is-not-on)

(add-hook 'prog-mode-hook #'electric-pair-local-mode)

(defun my:electric-pair-default-plus-before-word-inhibit (char)
  "Default inhibit behavior on CHAR, plus don't pair before a word.
This is because I'm often typing the first character of some pair
like \"(\" with point just before \"foo\" because I am about to
surround \"foo\" with (in this example) parentheses.  I want
\"(foo\" not \"()foo\"."
  (or (electric-pair-default-inhibit char)
      (eq (char-syntax (following-char)) ?w)))


;;; electric

;; Problem: `electric-indent-mode' is enabled by default.  Take
;; following `text-mode' buffer with point at |:
;;
;;     * blah blah blah
;;       blah blah blah
;;       |
;;
;; Note that the indentation of point was automatically inserted by
;; `electric-indent-mode' after hitting RET at the end of the second
;; line.  If you now hit RET again, the point moves to the next line,
;; and no indentation is inserted, however the previous line now has
;; two characters of trailing white space.
;;
;; The white space is not removed as it might be in other modes
;; because `indent-line-function' is `indent-relative' by default in
;; `text-mode', and `indent-relative' is (probably quite correctly) in
;; `electric-indent-functions-without-reindent'.  If not for this,
;; `electric-indent-mode' would actually `delete-horizontal-space' at
;; the end of the third line.  (Of course, it would also screw up
;; indentation on the second line when you hit RET at the end of that
;; line.)
;;
;; In programming-esque modes, `clean-aindent-mode' deals with this
;; for me.  However, clean-aindent works by advising
;; `newline-and-indent'.  By default, RET is unbound in
;; `text-mode-map', and so RET is `newline' via `global-map' in
;; `text-mode' buffers, meaning clean-aindent never runs in
;; `text-mode'.
;;
;; This applies to any mode where RET is not `newline-and-indent' and
;; where `electric-indent-mode' is not otherwise inhibited/disabled.
;; Therefore solutions that affect only `text-mode' are not sufficient
;; for me.  (I tried solving this in just `text-mode-map' and then
;; *immediately* ran into this same problem in a `conf-mode' buffer.)
;;
;; A non-exhaustive list of possible solutions to this problem:
;;
;; 1. Turn off `electric-indent-mode' globally, turn on locally where
;;    I want it.
;;
;; 2. Bind RET to `newline-and-indent' in `global-map'.  This seems
;;    invasive.
;;
;; 3. Write my own thing like `clean-aindent-mode' that works with
;;    `newline'.  (clean-aindent's code may already work with
;;    `newline'.)
;;
;; 4. Remove \n from `electric-indent-chars' globally.  Maybe add it
;;    back in modes where it makes sense.
;;
;; `electric-indent-mode' angered me enough that I now do #1, but
;; experience has taught me that I usually want `electric-indent-mode'
;; on in every prog-mode-derived buffer.

(with-eval-after-load 'electric
  (electric-indent-mode -1)

  (add-hook 'prog-mode-hook #'electric-indent-local-mode))

;;; elisp-mode

(my:add-hooks 'emacs-lisp-mode-hook
  #'paredit-mode
  #'aggressive-indent-mode
  ;; Useful for colors in e.g. `set-face-attribute'.
  #'rainbow-mode
  #'my:warn-white-space-mode)

(defun my:emacs-lisp-mode-hook()
  ;; Make name shorter in mode line.
  (setq mode-name "ELisp")
  (setq indent-tabs-mode nil)
  (setq imenu-generic-expression
        (append imenu-generic-expression
                '(("Sections" "^;;;;?\\s-+\\(.*\\)" 1))))
  ;; Trying out case-insensitive dabbrev-code completion in Emacs
  ;; Lisp.  Would have saved me time figuring out why I couldn't
  ;; complete "my:LaTex-" (note lower case "X"--oops).
  (setq-local company-dabbrev-code-ignore-case t)
  (my:company-group-existing-backend 'company-capf '(company-dabbrev-code)))

(add-hook 'emacs-lisp-mode-hook #'my:emacs-lisp-mode-hook)

(defun my:ielm-switch-to-ielm ()
  "Switch to the IELM buffer, creating it if needed."
  (interactive)
  ;; `ielm' uses `pop-to-buffer-same-window', unconditionally.  This
  ;; seems like the easiest way to get the IELM buffer, creating it if
  ;; it doesn't exist.
  (pop-to-buffer (save-window-excursion
                   (ielm)
                   (current-buffer))))

(with-eval-after-load 'elisp-mode
  (bind-keys :map emacs-lisp-mode-map
             ("C-c C-z" . my:ielm-switch-to-ielm)
             ("C-c C-r" . eval-region)
             ("C-c C-b" . eval-buffer)
             ("M-m m d m" . macrostep-expand)))

(my:load-recipe 'indent-elisp-like-common-lisp)


;;; elpy

(setq elpy-eldoc-show-current-function nil)

(advice-add 'python-mode :before #'elpy-enable)

(with-eval-after-load 'elpy
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))

  ;; https://github.com/jorgenschaefer/elpy/blob/1beb3b5ddb0590e7ccec744f353d7c71c2fbda09/docs/customization_tips.rst#use-flycheck-instead-of-flymake
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  (bind-keys :map elpy-mode-map
             ("<C-return>" . nil)
             ;; Not having these as forward/backward-paragraph drives
             ;; me nuts.
             ("<C-up>" . nil)
             ("<C-down>" . nil)
             ;; auto-highlight-symbol uses these.
             ("<M-left>" . nil)
             ("<M-right>" . nil)
             ;; I use this for opening links (and I never use
             ;; `elpy-occur-definitions').
             ("C-c C-o" . nil)))

(my:load-recipes 'elpy-nav-block-always-move)


;;; embrace

(bind-keys ("C-\"" . embrace-commander))


;;; emmet-mode

(setq emmet-preview-default t)


;;; etags

(setq tags-revert-without-query t
      tags-case-fold-search nil)


;;; expand-region

;; I'm loading this up front because I have several mode hooks that
;; want to frob at `er/try-expand-list', and putting modification of
;; buffer-local variables in an `with-eval-after-load' would
;; be... weird.
(require 'expand-region)

(bind-key "M-'" 'er/expand-region)


;;; face-search

(require 'custom)
(require 'cus-edit)
(custom-load-symbol 'hi-lock-faces)

(setq face-search-default-faces
      (mapcar #'car (seq-filter (lambda (item) (eq (cadr item) 'custom-face))
                                (custom-group-members 'hi-lock-faces nil))))

(bind-keys ("C-x w C-s" . face-search-forward)
           ("C-x w C-r" . face-search-backward))


;;; faces

(when (display-graphic-p)
  (when (x-list-fonts "Fira Mono")
    (set-face-attribute 'default nil :font "Fira Mono 8")
    ;; On macOS this was ending up as... Courier.
    (cl-pushnew "Fira Mono" (alist-get "Monospace" face-font-family-alternatives
                                       nil nil #'equal)
                :test #'equal))

  (when (x-list-fonts "Helvetica")
    (set-face-attribute 'variable-pitch nil :font "Helvetica 10"))

  ;; On macOS this was ending up as... Consolas?!  Courier looks just
  ;; fine IMHO.
  (cl-pushnew "Courier" (alist-get "Monospace Serif" face-font-family-alternatives
                                   nil nil #'equal)
              :test #'equal)

  ;; Trigger the customize :set function.
  (customize-set-variable 'face-font-family-alternatives
                          face-font-family-alternatives))

;;; faux-indent

(define-advice faux-indent-mode (:before (&optional arg)
                                         my:turn-off-sqlind-minor-mode)
  (when (and (bound-and-true-p sqlind-minor-mode)
             (my:minor-mode-arg-will-turn-on arg faux-indent-mode))
    (sqlind-minor-mode -1)))


;;; files

(setq make-backup-files nil
      confirm-kill-emacs 'y-or-n-p)

;; Detect SQL in strings!  Turns out that `magic-mode-alist' (and
;; `magic-fallback-mode-alist') set `case-fold-search' to nil when
;; matching their regexps, hence the dance with `upcase' here.
(add-to-list 'magic-fallback-mode-alist
             (let* ((keywords '("select"
                                "insert" "update" "delete"
                                "create" "alter" "drop"
                                "copy"))
                    (keywords-regexp
                     (regexp-opt (nconc (mapcar #'upcase keywords)
                                        keywords))))
               (cons (rx (* (any space ?\n))
                         symbol-start
                         (regexp keywords-regexp))
                     'sql-mode)))

(my:load-recipes 'files-delete-auto-save-after-revert-buffer
                 'files-save-some-buffers-default-no-query)


;;; fill

;; Make `adaptive-fill-mode' recognize numbered lists as well.

(unless (equal (my:get-standard-value 'adaptive-fill-regexp)
               "[ \t]*\\([-–!|#%;>*·•‣⁃◦]+[ \t]*\\)*")
  (warn "`adaptive-fill-regexp' changed from 27.0.50 value, check your mod"))

(setq adaptive-fill-regexp (rx (* (any ?\s ?\t))
                               (* (group (or
                                          ;; List of "bullet-like" characters
                                          ;; taken from original value.
                                          (1+ (any "-–!|#%;>*·•‣⁃◦"))
                                          ;; Here's the numbered list bit.
                                          (: (1+ digit) ?.))
                                         (* (any ?\s ?\t))))))


;;; find-func

(bind-keys ("M-m j e f" . find-function)
           ("M-m j e v" . find-variable)
           ("M-m j e l" . find-library))

(which-key-add-key-based-replacements "M-m j e" "emacs")


;;; flash-full-path

(bind-keys ("M-m u p" . flash-full-path-in-mode-line))


;;; frame

(bind-keys ("<s-return>" . toggle-frame-fullscreen)
           ;; Alternate shortcut, which is standard for macOS:
           ("C-s-f" . toggle-frame-fullscreen))


;;; frame-resize

(defun my:frame-resize-window-default (window)
  (unless (or (window-preserved-size window t)
              (window-parameter window 'window-side))
    '(80 nil)))

(with-eval-after-load 'frame-resize
  (add-to-list 'frame-resize-window-size-functions
               #'my:frame-resize-window-default t)

  (dolist (func '(
                  kill-buffer-and-window
                  ediff-toggle-split
                  my:delete-window-that-direction
                  imenu-list-quit-window
                  transpose-frame
                  treemacs
                  winner-undo
                  ))
    (add-to-list 'auto-frame-resize-commands func)))

(when (display-graphic-p)
  (auto-frame-resize-mode 1))

(bind-keys ("M-+" . frame-resize))

;; I abuse `frame-resize' for redrawing the display, which is
;; necessary distressingly often on NeXTStep term.
(when (eq window-system 'ns)
  (define-advice frame-resize (:after (&rest _args) my:redraw-display)
    (redraw-display)))


;;; free-keys

(with-eval-after-load 'free-keys
  ;; Add function keys.
  (setq free-keys-keys (nconc (split-string free-keys-keys "" t)
                              (mapcar (lambda (n) (format "<f%d>" n))
                                      (number-sequence 1 12))
                              '("_" "+")))

  ;; Add super modifier.
  (dolist (mod '("s" "C-s" "M-s" "C-M-s"))
    (add-to-list 'free-keys-modifiers mod t)))


;;; flycheck

(setq flycheck-mode-line-prefix "✓"
      flycheck-global-modes '(not
                              ;; flycheck feedback in elisp buffers is
                              ;; not really helpful, and far too noisy
                              ;; (though it is occasionally very
                              ;; useful).
                              emacs-lisp-mode
                              org-mode
                              ;; flycheck doesn't really support
                              ;; `web-mode', and anyway you might not
                              ;; know what language (including
                              ;; template languages) your `web-mode'
                              ;; buffer is really using.  flycheck
                              ;; maintainer says that there used to be
                              ;; an HTML Tidy checker, I believe, but
                              ;; it croaks badly on anything like a
                              ;; template.
                              web-mode)
      ;; Defaults to 400, sadly too few for some of my files at work.
      flycheck-checker-error-threshold 2000
      ;; These default to "python3", which annoys me when I have to
      ;; work on Python 2.
      flycheck-python-pycompile-executable "python"
      flycheck-python-pylint-executable "python")

(global-flycheck-mode 1)

(with-eval-after-load 'flycheck
  ;; I'm trying to avoid labeling C-c ! as Flycheck unless Flycheck
  ;; mode is on in this buffer.  I'm not really sure this is a good
  ;; way to accomplish this.
  (add-to-list 'which-key-replacement-alist
               '(("C-c !" . "Prefix Command")
                 . (lambda (key-binding)
                     (if flycheck-mode
                         (cons (car key-binding) "flycheck")
                       key-binding))))

  ;; Run Pylint, then Mypy.

  (flycheck-add-next-checker 'python-pylint 'python-mypy)

  (setf (flycheck-checker-get 'python-mypy 'next-checkers) nil)

  ;; Prefer pylint to flake8.
  (setq flycheck-checkers (cons 'python-pylint
                                (delq 'python-pylint flycheck-checkers)))

  (my:load-recipes 'flycheck-python-pylint-detect-tabs))

;; "terraform validate" checker

(defun my:flycheck-parse-terraform-validate (output checker buffer)
  "Parse \"terraform validate\" errors from JSON OUTPUT."
  (when-let ((diagnostics (alist-get 'diagnostics
                                     (car (flycheck-parse-json output)))))
    (mapcar (lambda (diag)
              (let* ((range (alist-get 'range diag))
                     (start (alist-get 'start range))
                     (end (alist-get 'end range))
                     (summary (alist-get 'summary diag))
                     (detail (alist-get 'detail diag)))
                (flycheck-error-new
                 :line (or (alist-get 'line start) 1)
                 :column (alist-get 'column start)
                 :end-line (alist-get 'line end)
                 :end-column (alist-get 'column end)
                 :buffer buffer
                 :checker checker
                 :filename (alist-get 'filename range)
                 :message (if detail
                              (format "%s: %s" summary detail)
                            summary)
                 :level (intern (alist-get 'severity diag)))))
            diagnostics)))

(with-eval-after-load 'terraform-mode
  (with-eval-after-load 'flycheck
    (flycheck-define-checker my:terraform-validate
      "A Terraform checker with `terraform validate'.

See URL `https://www.terraform.io/docs/commands/validate.html'."
      :command ("terraform" "validate" "-json")
      :error-parser my:flycheck-parse-terraform-validate
      :predicate flycheck-buffer-saved-p
      :modes (terraform-mode))

    (unless (memq 'my:terraform-validate flycheck-checkers)
      (setq flycheck-checkers (append flycheck-checkers
                                      '(my:terraform-validate))))

    (let ((after-tflint (flycheck-get-next-checkers 'terraform-tflint)))
      (cond
        ((equal after-tflint '(my:terraform-validate))
         ;; No action necessary.
         )
        (after-tflint
         (warn (concat "terraform-tflint checker has a next-checker already,"
                       " not adding mine")))
        (t
         (flycheck-add-next-checker 'terraform-tflint
                                    'my:terraform-validate))))))


;;; flycheck-clj-kondo

(with-eval-after-load 'clojure-mode
  (require 'flycheck-clj-kondo))


;;; flycheck-package

(with-eval-after-load 'flycheck
  (flycheck-package-setup))


;;; flycheck-pos-tip

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode 1))


;;; flyspell

(add-hook 'text-mode-hook #'flyspell-mode)

(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(my:load-recipes 'flyspell-fix-key-lookup)


;;; frame

;; Bug: `display-monitor-attributes-list' documents that a "display
;; name" is acceptable, but `ns-display-monitor-attributes-list' (ObjC
;; function) calls terminal_live_p, which does not accept a "display
;; name".  Cf. `x-display-monitor-attributes-list' (xfns.c) which
;; starts with check_x_display_info instead, and I assume that
;; function accepts a display name.
;;
;; I discovered this when `fit-frame-to-buffer' raised the error.
;;
;; I should open a bug in Emacs (search keyword: upstream).

(when (fboundp 'ns-display-monitor-attributes-list)
  (define-advice ns-display-monitor-attributes-list
      (:filter-args (args) my:accept-display-name-bug)
    (if (and args (stringp (car args)))
        ;; I'm really not even sure if this is proper, but it does
        ;; work on macOS.
        ;;
        ;; Also, the `cdr' here is future-proofing: at the time of
        ;; writing, `ns-display-monitor-attributes-list' only takes
        ;; 0..1 arguments.
        (cons (get-device-terminal (car args)) (cdr args))
      args)))


;;; git-commit

(with-eval-after-load 'git-commit
  (add-to-list 'git-commit-style-convention-checks
               'overlong-summary-line))

(setq git-commit-summary-max-length 50)


;;; goto-addr --- Keywords: link url follow open

;; `goto-address-mode' "[buttonizes] URLs and e-mail addresses".
;;
;; I can never remember the name of this fucking mode, nor its
;; bindings.  Its key mapping is done with an overlay, so point must
;; be on a link for e.g. `counsel-descbinds' to even see this binding.

(add-hook 'prog-mode-hook #'goto-address-mode)
(add-hook 'text-mode-hook #'goto-address-mode)


;;; groovy-mode

(defun my:groovy-mode-hook ()
  ;; I'm pasting Groovy into Jira Cloud ScriptRunner, and tabs are not
  ;; pretty there.
  (setq-local indent-tabs-mode nil))

(add-hook 'groovy-mode-hook #'my:groovy-mode-hook)

;; I should submit a patch upstream to at least not give an error when
;; it tries to (get-buffer nil) because `groovy-buffer' starts nil.
(defun my:switch-to-groovy-auto-start-groovy (&rest _)
  (unless (and groovy-buffer (get-buffer groovy-buffer))
    (call-interactively #'run-groovy)))

(advice-add 'switch-to-groovy :before #'my:switch-to-groovy-auto-start-groovy)


;;; haskell-mode

(my:add-hooks 'haskell-mode-hook
  #'subword-mode
  #'interactive-haskell-mode
  #'flycheck-haskell-setup
  #'hindent-mode)

;; https://haskell.github.io/haskell-mode/manual/latest/Aligning-code.html
(with-eval-after-load 'align
  (add-to-list 'align-rules-list
               '(haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                 (modes . '(haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes . '(haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes . '(haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes . '(haskell-mode literate-haskell-mode)))))


;;; help

;; Select help windows always.
(setq help-window-select t)

(defun my:info-elisp ()
  (interactive)
  (info "elisp"))

(my:load-recipes 'help-describe-map)

(with-eval-after-load 'help
  (bind-keys :map help-map
             ("E" . my:info-elisp)
             ;; Copying help-fns+.el
             ("M-k" . my:describe-map)))


;;; hi-lock

(global-hi-lock-mode 1)


;;; hideshow

(with-eval-after-load 'hideshow
  ;; A kind of "change `hs-minor-mode' prefix" thing.  C-c @ is a bit
  ;; unreasonable IMHO.  https://emacs.stackexchange.com/a/33686
  ;;
  ;; (Have to use `bind-key' rather than `bind-keys' so I can eval
  ;; this `lookup-key' form.)
  (bind-key "s-2" (lookup-key hs-minor-mode-map (kbd "C-c @"))
            hs-minor-mode-map))

(defun my:hs-minor-mode-faster-which-key ()
  (make-local-variable 'my:which-key-no-delay-prefixes)
  (add-to-list 'my:which-key-no-delay-prefixes t t)
  (dolist (prefix '("C-c @" "s-2"))
    (add-to-list 'my:which-key-no-delay-prefixes prefix)))

(with-eval-after-load 'which-key
  (add-hook 'hs-minor-mode-hook #'my:hs-minor-mode-faster-which-key))

(defvar my:hs-minor-mode-slow-settings
  '((show-trailing-whitespace bool-var t)
    (highlight-parentheses-mode mode t))
  "List of variables or modes that can make `hs-minor-mode' slow.")

(defun my:hs-minor-mode-slow-settings (&optional make-changes)
  (interactive "P")
  (let ((slow-settings
         (delq nil
               (mapcar (lambda (setting)
                         (pcase-let
                             ((`(,var
                                 ,(and type
                                       (guard (memq type '(bool-var mode))))
                                 ,slow-value)
                               setting))
                           (when (and (boundp var)
                                      ;; Using `not' to convert both values
                                      ;; to t/nil so I can compare them with
                                      ;; `eq'.
                                      (eq (not (symbol-value var))
                                          (not slow-value)))
                             (when make-changes
                               (pcase type
                                 ('bool-var (set var (not slow-value)))
                                 ('mode (funcall var (if slow-value -1 1)))))
                             var)))
                       my:hs-minor-mode-slow-settings))))
    (when slow-settings
      (message (if make-changes
                   "Changed %s to speed up `hs-minor-mode'"
                 (concat "%s may make `hs-minor-mode' slow, run"
                         " C-u M-x my:hs-minor-mode-slow-settings to change "
                         (if (> (length slow-settings) 1) "them" "it")))
               (string-join (mapcar (apply-partially #'format-message "`%s'")
                                    slow-settings)
                            ", ")))))

(defun my:hs-minor-mode-hook ()
  (add-hook 'hack-local-variables-hook #'my:hs-minor-mode-slow-settings 0 t))

(add-hook 'hs-minor-mode-hook #'my:hs-minor-mode-hook)


;;; hideshow-tab

(with-eval-after-load 'hideshow
  (bind-keys :map hs-minor-mode-map
             ("TAB" . hideshow-tab)))


;;; highlight-indent-guides

(setq highlight-indent-guides-method 'character)


;;; highlight-parentheses

(add-hook 'prog-mode-hook #'highlight-parentheses-mode)


;;; hindent

(with-eval-after-load 'hindent
  (bind-keys :map hindent-mode-map
             ([remap unfill-toggle] . hindent-reformat-decl-or-fill)))


;;; hippie-exp

(bind-key "M-/" 'hippie-expand)

(with-eval-after-load 'hippie-exp
  (setq hippie-expand-try-functions-list
        (seq-difference hippie-expand-try-functions-list
                        '(try-expand-line try-expand-list))))


;;; hl-line+

(with-eval-after-load 'hl-line
  (require 'hl-line+)

  ;; This makes hl-line appear above org-mode columns view, which is
  ;; the effect I was going for.  I'm setting this globally for now,
  ;; but this might turn out to be a bad idea, and instead I should
  ;; set it only in org-mode or something like that.
  (setq hl-line-overlay-priority 1))


;;; hl-todo

(global-hl-todo-mode 1)

(if-let ((xxx-face (assoc "XXX+" hl-todo-keyword-faces)))
    (setf (cdr xxx-face)
          '(:inherit hl-todo :foreground "yellow" :background "red"))
  (warn "Couldn't find XXX+ in `hl-todo-keyword-faces'"))


;;; ielm

(my:add-hooks 'ielm-mode-hook
  #'paredit-mode)

(underlings-move-menu-with-one-time-hook 'ielm-map
                                         ["Complete" "In/Out" "Signals"]
                                         "IELM"
                                         :hook-var 'ielm-mode-hook)


;;; imenu

(bind-key "M-m j i" 'imenu)

(setq imenu-auto-rescan t
      imenu-auto-rescan-maxout (* 1024 1024 10))

(add-hook 'imenu-after-jump-hook #'my:highlight-line-after-movement)

(my:load-recipes 'imenu-only-rescan-after-changes)


;;; imenu-list

;; After using the recipe above, this parameter specifies the
;; `window-total-width' of the imenu-list side window.  On my system,
;; today, I need 54 to get a `window-text-width' of 50.
(setq imenu-list-size 54)

(bind-keys ("M-m u i" . imenu-list-smart-toggle))

(with-eval-after-load 'imenu-list
  (my:load-recipes 'imenu-list-in-side-buffer
                   'imenu-list-sort
                   'imenu-list-stop-when-window-deleted))


;;; impatient-mode

(setq impatient-mode-delay 1)

;; We have this bound in `web-mode-map'.
(autoload 'imp-visit-buffer "impatient-mode" nil t)


;;; info

;; Default is t, which makes `Info-mode' precede everything with
;; "see", which reads stupid in things like "...consult see The other
;; section".
(setq Info-hide-note-references 'hide)


;;; intero

(with-eval-after-load 'haskell-mode
  (intero-global-mode 1))

(defun my:intero-mode-hook ()
  (my:company-group-existing-backend 'intero-company '(company-dabbrev-code)))

(add-hook 'intero-mode-hook #'my:intero-mode-hook)


;;; isearch

(my:load-recipes 'isearch-exit-at-beginning-of-match)

(bind-keys :map isearch-mode-map
           ("C-'" . my:avy-isearch))


;;; iso-transl

(with-eval-after-load 'iso-transl
  (add-to-list 'which-key-replacement-alist
               '(("C-x 8" . "Prefix Command") . (nil . "unicode"))))


;;; ivy

(ivy-mode 1)

(require 'ivy-avy)

(bind-keys ("<f6>" . ivy-resume)
           :map ivy-minibuffer-map
           ("<next>" . ivy-scroll-up-command)
           ("<prior>" . ivy-scroll-down-command)
           ;; Move ivy-restrict-to-matches to C-SPC because that's
           ;; where I think I'm used to it from ido-mode, and also
           ;; because I apparently sometimes hold down the space
           ;; modifier when typing the space in something like "foo: "
           ;; and then I wonder "WTF where did my input go?"
           ("C-SPC" . ivy-restrict-to-matches)
           ("S-SPC" . nil))

;; ivy-initial-inputs-alist makes "^" the default for a bunch of
;; commands, like counsel-M-x, which I generally dislike.
(setq ivy-initial-inputs-alist nil)

;; I switch between ivy--regex-fuzzy (rarely) and
;; ivy--regex-ignore-order, but as you'll see below, I'm currently
;; using my own "hybrid" matcher.
;;(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
;;(setq ivy-re-builders-alist '((t . ivy--regex-plus)))
;;(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

(setq ivy-use-virtual-buffers t
      ivy-virtual-abbreviate 'full
      ivy-magic-tilde nil
      ivy-read-action-function #'ivy-hydra-read-action
      ;; Trying this for a while, helps when you have long matches
      ;; from something like counsel-ag.  Better fix might be to show
      ;; the matching portion (somehow), or bind some kind of "scroll
      ;; right"?  Below is fast and easy to try before those more
      ;; involved ideas.
      ivy-truncate-lines nil)

(my:load-recipes 'ivy-special-switch-buffers
                 'ivy-fuzzy-regex-combo-matcher
                 'ivy-regex-plus-or-literal-regex)

(dolist (command '(counsel-rg swiper-isearch))
  (setf (alist-get command ivy-re-builders-alist)
        #'my:ivy--regex-plus-or-literal-regex))


;;; ivy-bibtex

(setq ivy-bibtex-default-action 'ivy-bibtex-insert-key)


;;; ivy-xref

(setq xref-show-definitions-function #'ivy-xref-show-defs
      xref-show-xrefs-function #'ivy-xref-show-xrefs
      ivy-xref-use-file-path t)


;;; js2-mode

;; https://elpa.gnu.org/packages/js2-mode.html
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(defun my:js2-mode-hook ()
  ;; Prevailing JS style seems to be indenting with spaces.
  (my:setq-local indent-tabs-mode nil
                 tab-width 4))

(add-hook 'js2-mode-hook #'my:js2-mode-hook)


;;; js2-refactor

(add-hook 'js2-mode-hook #'js2-refactor-mode)

(with-eval-after-load 'js2-refactor
  (my:load-recipes 'js2-refactor-hydra)

  (js2r-add-keybindings-with-prefix "M-m m r")

  (bind-keys :map js2-refactor-mode-map
             ("M-m m r SPC" . js2-refactor-hydra/body)))


;;; json-mode

(with-eval-after-load 'json-mode
  (my:load-recipes 'json-mode-show-path
                   'json-mode-beautify)

  (bind-keys :map json-mode-map
             ("C-c C-p" . my:json-mode-show-path-jq)))

(my:add-hooks 'json-mode-hook
  #'hs-minor-mode)


;;; link-hint

(my:load-recipes 'link-hint-multi-dispatch)

(bind-key "C-c C-o" #'my:link-hint-multi-dispatch)


;;; lisp

;; This is apparently only on ESC <C-backspace> by default?  WTF.
(bind-keys ("<C-M-backspace>" . backward-kill-sexp))


;;; lisp-comment-dwim

(require 'lisp-comment-dwim)

(with-eval-after-load 'paredit
  (bind-keys :map paredit-mode-map
             ("M-;" . lisp-comment-dwim)))


;;; lisp-mode

(defun my:lisp-mode-hook ()
  (setq indent-tabs-mode nil))

(my:add-hooks 'lisp-mode-hook
  #'paredit-mode
  #'aggressive-indent-mode
  #'my:lisp-mode-hook)


;;; loccur

;; This should be autoloaded.  Maybe push upstream.
(autoload 'loccur "loccur" nil t)


;;; lua-mode

;; There doesn't seem to be a great deal of consensus over how to
;; indent Lua.

(setq lua-indent-level 4)

(defun my:lua-mode-hook ()
  (my:setq-local tab-width lua-indent-level))

(add-hook 'lua-mode-hook #'my:lua-mode-hook)

(smart-tabs-advise 'lua-indent-line 'lua-indent-level)


;;; magit

;; By default (as of https://github.com/magit/magit/pull/4237), Magit
;; will bind:
;;
;;     key             binding
;;     ---             -------
;;     C-x g           magit-status
;;     C-x M-g         magit-dispatch
;;     C-c M-g         magit-file-dispatch
;;
;; I was already doing C-x g.  I actually have C-c M-g bound in
;; AUCTeX, and while I don't currently use that binding much (and in
;; fact I am currently questioning whether it should exist), I see no
;; reason to clobber it.  Therefore I'm disabling Magit's global
;; binding of these keys and I'll set up the bindings I like below.

(setq magit-define-global-key-bindings nil)

(bind-keys ("C-x g" . magit-status)
           ;; These bindings were originally derived from what is in
           ;; Spacemacs, just in case you're wondering.
           ("M-m v f" . magit-find-file)
           ("M-m v F" . magit-file-dispatch)
           ("M-m v d" . magit-diff-buffer-file))

;; Sometimes I might still M-x magit.
(autoload 'magit "magit" nil t)

(setq magit-diff-refine-hunk 'all
      magit-diff-refine-ignore-whitespace nil
      magit-bury-buffer-function 'magit-mode-quit-window)

;; truncate-lines does not work well for me when viewing diffs and such.

(defun my:magit-mode-hook ()
  (setq truncate-lines nil))

(add-hook 'magit-mode-hook #'my:magit-mode-hook)

(which-key-add-major-mode-key-based-replacements 'magit-status-mode
    "j p" "unpushed"
    "j f" "unpulled")


;;; markdown-mode

(with-eval-after-load 'markdown-mode
  (bind-keys :map markdown-mode-map
             ("M-m m c e" . my:markdown-mode-copy-as-html-email))

  (my:load-recipes 'markdown-mode-copy-as-html-email
                   ;; Note that I do not wrap the loading of
                   ;; markdown-mode-unindent-when-editing-code-blocks
                   ;; in (with-eval-after-load 'edit-indirect ...)
                   ;; because it installs a hook into
                   ;; `markdown-mode-hook', so it needs to run when a
                   ;; `markdown-mode' buffer is created, before we
                   ;; even know whether or not we're going to be using
                   ;; edit-indirect.
                   'markdown-mode-unindent-when-editing-code-blocks
                   'markdown-mode-better-previous-line-indent
                   'markdown-mode-delete-trailing-space))

(setq markdown-command "pandoc -f markdown -t html --standalone"
      markdown-enable-math t
      markdown-footnote-location 'immediately
      markdown-header-scaling t)

(defun my:markdown-mode-hook ()
  (setq-local indent-tabs-mode nil)
  (visual-line-mode 1))

(my:add-hooks 'markdown-mode-hook
  ;; I give up, GitHub Flavored Markdown is popular and treats line feed
  ;; characters like <br>.  Default to visual-line-mode.
  #'visual-line-mode
  #'electric-pair-local-mode
  #'my:markdown-mode-hook)


;;; minibuffer

(setq completion-styles '(basic partial-completion initials flex))


;;; minions

(setq minions-mode-line-lighter "🄼"
      minions-direct '(
                       black-format-on-save-mode
                       docformatter-on-save-mode
                       flycheck-mode
                       isort-format-on-save-mode
                       multiple-cursors-mode
                       persp-mode
                       yapf-format-on-save-mode
                       ))

(minions-mode 1)


;;; modus-vivendi-theme

(when (featurep 'modus-vivendi-theme)
  (warn (concat "modus-vivendi-theme has been loaded too early,"
                " `modus-vivendi-theme-override-colors-alist' may not"
                " be respected")))

(setq modus-vivendi-theme-intense-hl-line t
      ;; Doesn't play well with F.lux unless you also have my mod to
      ;; bg-paren-match-intense color, below.
      modus-vivendi-theme-intense-paren-match t)

(setq modus-vivendi-theme-override-colors-alist
      '(("bg-paren-match-intense" . "#8E00C2")))

(with-eval-after-load 'modus-vivendi-theme
  (modus-vivendi-theme-with-color-variables
    (custom-theme-set-faces
     'modus-vivendi
     ;; Remove :box from these because it screws up column alignment
     ;; in column mode in agenda buffers.
     `(org-done ((,class :background ,bg-dim :foreground ,green
                         :inherit ,modus-theme-variable-pitch)))
     `(org-todo ((,class :background ,bg-dim :foreground ,red-alt
                         :inherit ,modus-theme-variable-pitch))))))


;;; move-text

(move-text-default-bindings)


;;; mule

;; Note that C-x RET is actually bound in mule-cmds.el, but that
;; doesn't `provide' anything.
(with-eval-after-load 'mule
  (add-to-list 'which-key-replacement-alist
               '(("C-x RET" . "Prefix Command") . (nil . "mule"))))


;;; multi-line

(bind-key "C-c d" 'multi-line)


;;; mwim

(bind-keys ("C-a" . mwim-beginning-of-code-or-line)
           ("C-e" . mwim-end-of-line-or-code))

(my:load-recipes 'mwim-set-bol-eol-funcs-in-visual-line-mode)


;;; multiple-cursors

;; You must actually require this, otherwise you will not load some of
;; the more interesting multiple-cursors functionality, such as C-' to
;; hide lines without a cursor.
(require 'multiple-cursors)

(defun my:mc/mark-more-like-this-extended-dwim-advice (&rest _args)
  (unless (use-region-p)
    (mc--mark-symbol-at-point)))

(advice-add 'mc/mark-more-like-this-extended :before
            #'my:mc/mark-more-like-this-extended-dwim-advice)

(bind-keys ("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("<s-mouse-1>" . mc/add-cursor-on-click)
           ("C-?" . mc/mark-all-dwim)
           ("C-M-?" . mc/mark-more-like-this-extended))

(with-eval-after-load 'multiple-cursors-core
  (bind-keys :map mc/keymap
             ;; Return should not end multiple-cursors-mode.
             ("<return>" . nil)
             ;; isearch doesn't work with multiple cursors, phi-search is
             ;; the suggested substitute.
             ("C-s" . phi-search)
             ("C-r" . phi-search-backward)))


;;; nav-stack

;; (setq nav-stack-debug-enabled t)

(nav-stack-mode 1)

(require 'nav-stack-ivy)

(dolist (mode '(imenu-list-major-mode treemacs-mode))
  (add-to-list 'nav-stack-post-command-excluded-major-modes mode))

(defun my:nav-stack-move-predicate (location)
  (let ((buffer (nav-stack-location-buffer location)))
    (not (or (my:pop-up-buffer-p buffer)
             (with-current-buffer buffer
               ;; Empty *Backtrace* buffers.  Maybe I should just
               ;; delete these.
               (and (derived-mode-p 'debugger-mode)
                    (= (point-max) 1)))))))

(add-to-list 'nav-stack-move-predicates
             #'my:nav-stack-move-predicate t)

(add-hook 'nav-stack-after-move-hook #'my:highlight-line-after-movement)


;;; newcomment

(setq comment-empty-lines 'eol)


;;; olivetti

(setq-default olivetti-body-width 80)


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
 ;; Maybe don't be executing Elisp in table formulas in some random
 ;; org file I downloaded.  (Thanks wasamasa.)
 org-table-allow-automatic-line-recalculation nil
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
 org-special-ctrl-a/e t
 org-speed-commands-user '(("a" . org-archive-subtree-default)
                           ("P" . org-set-property)
                           ("S" . widen)
                           ("x" . org-cut-subtree))
 ;; 'expert stops C-c C-t from popping up a (jarring) temporary
 ;; window.
 org-use-fast-todo-selection 'expert
 org-use-speed-commands (lambda ()
                          (and (org-at-heading-p)
                               (looking-back "^\\**"
                                             (line-beginning-position))))
 ;; I think I always want indentation preserved in my source
 ;; blocks.
 org-src-preserve-indentation t
 org-src-window-setup 'other-window
 org-tags-column -76)

(with-eval-after-load 'org-agenda
  ;; This actually visits org-default-notes-file, so we don't load
  ;; this until we really have to.
  (setq org-agenda-files (org-add-archive-files (list org-default-notes-file)))

  ;; org-agenda must not sit on M-m, I use it for too many other
  ;; things.
  (bind-keys :map org-agenda-mode-map
             ("M-m" . nil)))

(bind-keys ("C-c r" . org-capture)
           ("M-m a o k i" . org-clock-in-last)
           ("M-m a o k o" . org-clock-out)
           ("M-m a o k g" . org-clock-goto))

(with-eval-after-load 'org
  (my:load-recipes
   'org-babel-read-table-in-dblock
   'org-columns-delete-property
   'org-daily-time-summary
   'org-fix-faces-after-goto
   'org-insert-heading-ignore-invisibility
   'org-jump-over-priority-after-setting-it
   'org-make-nice-id-from-headline-text
   'org-property-drawer-fixes)

  (bind-keys :map org-mode-map
             ("C-c a" . org-agenda)
             ;; This interferes with avy, and I don't use
             ;; org-cycle-agenda-files anyway.
             ("C-'" . nil)
             ;; This interferes with my binding for `frame-resize'.
             ("M-+" . nil)
             ;; Org manual suggests setting this globally, but let's do
             ;; locally in org-mode buffers for now.
             ("C-c l" . org-store-link)
             ;; Yasnippet overrides C-c &, which is the default shortcut
             ;; for this command.  M-* not currently bound in my org-mode
             ;; buffer.
             ("M-*" . org-mark-ring-goto)
             ;; org commit 68b076bf5238 stopped binding C-a/C-e in
             ;; favor of command remapping
             ;; move-beginning-of-line/move-end-of-line, but I have
             ;; C-a/C-e bound to mwim functions instead, so that
             ;; basically breaks C-a/C-e in org.  Hence, just map
             ;; C-a/C-e here to what org intends.
             ("C-a" . org-beginning-of-line)
             ("C-e" . org-end-of-line)
             ;; Somewhere along the lines I think the default binding
             ;; for `org-priority' got superceded by one of my other
             ;; modes/mods/settings (`org-replace-disputed-keys'?).
             ("C-M-," . org-priority))

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

;; Clock persistence between restarts.

(setq org-clock-persist t
      org-clock-persist-query-resume nil)

(org-clock-persistence-insinuate)

(with-eval-after-load 'org-clock
  (my:load-recipes 'org-compact-clock-in-mode-line
                   'org-clock-update-mode-line-on-clock-in-out
                   'org-switch-to-pending-on-clock-in))

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

(defvar my:org-todo-files (mapcar #'expand-file-name
                                  '("~/todo.org" "~/todo.org_archive")))

(defun my:org-in-todo-file-p ()
  (member (buffer-file-name) my:org-todo-files))

(defface my:org-waiting
    '((t (:background "#fee470" :foreground "#950759D00000" :weight bold)))
  "Used for WAITING or HOLD to-do states.")

(defun my:org-todo-file-specific-hook ()
  (when (my:org-in-todo-file-p)
    (my:setq-local org-refile-targets '((nil . (:tag . "refile"))
                                        ("~/someday.org" . (:level . 1)))
                   org-tags-exclude-from-inheritance '("refile")
                   org-todo-keyword-faces '(("WAITING" . my:org-waiting)
                                            ("HOLD" . my:org-waiting))
                   ;; org-confirm-babel-evaluate nil
                   )))

(add-hook 'org-mode-hook #'my:org-todo-file-specific-hook)

(autoload 'org-table-get-remote-range "org-table")

(defun my:org-todo-allowed-projects (prop-name)
  (when (and (my:org-in-todo-file-p)
             (equal (upcase prop-name) "PROJECT"))
    (with-current-buffer (find-file-noselect "~/todo.org")
      (mapcar #'substring-no-properties
              (org-table-get-remote-range "projects" "@2$2..@>$2")))))

;; I apply this globally, rather than buffer local, *presumably*
;; because that's necessary to get it to take effect in agenda buffers
;; too.  But there's probably a better way to do this.
(add-hook 'org-property-allowed-value-functions
          #'my:org-todo-allowed-projects)

;; Private stuff that doesn't get checked into Git.
(dolist (file-name '("org" "open-ticket"))
  (load (expand-file-name file-name my:private-lisp-dir) t))


;;; osx-dictionary

(bind-keys ("C-$" . osx-dictionary-search-pointer))


;;; outline

(with-eval-after-load 'outline
  (bind-keys :map outline-minor-mode-map
             ("C-c C-2" . outline-hydra/body)))


;;; package-build

(setq package-build-recipes-dir "~/repositories/melpa/recipes")


;;; paredit

(with-eval-after-load 'paredit
  (bind-keys :map paredit-mode-map
             ("M-m j s" . paredit-split-sexp))

  (my:load-recipes 'paredit-delsel
                   'paredit-kill-whole-line))


;;; paren

(add-hook 'prog-mode-hook #'show-paren-mode)


;;; persp-mode

(setq persp-add-buffer-on-after-change-major-mode 'free
      ;; C-x 5 2 shouldn't copy e.g. window layout.  I hope this still
      ;; means additional frames will be saved with the perspective!
      ;; (If not, perhaps see `persp-ignore-wconf-once' here?)
      persp-init-new-frame-behaviour-override nil)

(with-eval-after-load 'persp-mode
  ;; Document C-c p o, which is a lambda.
  (add-to-list 'which-key-replacement-alist
               '(("C-c p o" . nil) . (nil . "persp-mode off"))))

(add-hook 'persp-mode-hook
          (underlings-define-menu-mover 'persp-mode "Perspectives" "Minors"
                                        :dest-map global-map
                                        :visible t))

;; Don't save buffers that aren't backed by a file, lest you get a
;; bunch of useless Magit and EPC buffers in `fundamental-mode' after
;; loading the last saved perspective.  We insert this just before the
;; last built-in persp-mode handler, which is the default that tries
;; to save every buffer.  This way we let things like TRAMP and
;; `dired-mode' buffers get handled by the earlier (and also built-in)
;; handlers.

(defun my:persp-mode-dont-save-buffers-without-files (b)
  (unless (buffer-file-name b)
    'skip))

(with-eval-after-load 'persp-mode
  (unless (memq #'my:persp-mode-dont-save-buffers-without-files
                persp-save-buffer-functions)
    (let ((last-cell (last persp-save-buffer-functions)))
      (setf (cdr last-cell) (list (car last-cell))
            (car last-cell) #'my:persp-mode-dont-save-buffers-without-files))))

;; Perspective-aware buffer switching with Ivy, courtesy
;; https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-ivy-el
;; (linked from persp-mode.el project).  I decided that I did not need
;; all of the code there, so this is just a subset, and a modified
;; subset at that.

(defun my:persp-mode-ivy-filter-buffers (buffer)
  (when-let ((persp (and persp-mode (get-current-persp))))
    (not (persp-contain-buffer-p buffer persp))))

(my:with-eval-after-all-load '(persp-mode ivy)
  (add-hook 'ivy-ignore-buffers #'my:persp-mode-ivy-filter-buffers))

(my:load-recipes 'persp-mode-save-load-frame-configuration
                 'persp-mode-auto-save-configuration)

;; Must set this before turning on persp-mode for it to have an effect.
(setq persp-auto-resume-time 0.1)

(when (bound-and-true-p persp-mode)
  (warn "Set `persp-auto-resume-time' too late, persp-mode already on"))

;; Never try and turn `persp-mode' on during init.
;; `persp-auto-resume-time' being non-zero (see above) will cause
;; `persp-mode' to set a timer, and that timer might run during init,
;; especially in my case where `el-patch-validate' might do things
;; that cause timers to fire.  You will be in a world of hurt.  Weird
;; shit like `enable-local-variables' getting set to nil (by el-patch)
;; will happen while your buffers are restored by persp-mode.
(add-hook 'after-init-hook #'persp-mode)


;;; prescient.el

;; NOTE: Package docs say that this must be loaded after counsel.
;; Thankfully not a problem as long as "c" comes before "p" in the
;; alphabet.

(with-eval-after-load 'prescient
  (prescient-persist-mode 1))

(with-eval-after-load 'company
  (company-prescient-mode 1))

(setq ivy-prescient-enable-filtering nil
      ivy-prescient-retain-classic-highlighting t)

(ivy-prescient-mode 1)

(if (not (eq (car ivy-prescient-sort-commands) :not))
    (warn (concat "`ivy-prescient-sort-commands' is no longer a :not"
                  " by default, fix init.el"))
  (cl-loop
     for func in '(counsel-rg
                   counsel-yank-pop)
     if (memq func ivy-prescient-sort-commands)
     do (warn "`%S' is already in `ivy-prescient-sort-commands', fix init.el"
              func)
     else
     collect func into funcs-to-add
     finally
       (setq ivy-prescient-sort-commands (nconc ivy-prescient-sort-commands
                                                funcs-to-add))))


;;; prog-mode

;; This may be a bad idea.
(with-eval-after-load 'prog-mode
  (bind-keys :map prog-mode-map
             ("RET" . newline-and-indent)))


;;; projectile

(projectile-mode 1)

(setq projectile-completion-system 'ivy)

(bind-keys ("C-c C-p" . projectile-command-map)
           ;; Spacemacs bindings, particularly useful when comint
           ;; binds something to C-c C-p.
           ("M-m p" . projectile-command-map))

(underlings-move-menu-with-one-time-hook 'projectile-mode "Projectile" "Minors"
                                         :dest-map global-map :visible t)


;;; pulse

(my:load-recipes 'find-cursor)

(bind-keys ("<f8>" . my:find-cursor))


;;; python

(setq python-indent-def-block-scale 1)

;; dtrt-indent does this for us, everywhere.
(setq python-indent-guess-indent-offset nil)

(defun my:python-mode-inhibit-electric-indent (char)
  "Don't indent when CHAR is : and we're already at a plausible indent."
  ;; I have a more generous comment about this below.
  (python-indent-calculate-levels)
  (when (and (eq char ?:)
             (memq (current-indentation) (python-indent-calculate-levels)))
    'no-indent))

;; Double quotes don't work right in comments when
;; electric-pair-mode is on.  Try:
;;
;;     # foo's "bar|
;;
;; You want one " but instead you get a pair "".  I think this
;; happens because electric-pair thinks the preceding ' is the
;; start of a string and that " in front of bar is the end of the
;; string.  This is hard as fuck to fix, just don't pair quotes in
;; comments.
;;
;; Eventually I would like to switch from electric-pair-mode to
;; smartparens-mode, which holds the promise of working better, but
;; first I have to find a solution to
;; https://github.com/Fuco1/smartparens/issues/731.
(defun my:python-electric-pair-inhibit (char)
  (or
   ;; Don't pair quotes inside a comment.
   (and (memq char '(?\" ?\'))
        (nth 4 (syntax-ppss)))
   ;; Don't pair when wrapping a string in () or [] or {}.  This
   ;; accompanies the logic I'm currently already getting by virtue
   ;; of calling the default predicate, below, which as of this
   ;; writing includes `electric-pair-conservative-inhibit', which
   ;; will not pair before a word.
   (and (memq char '(?\( ?\[ ?\{))
        (looking-at-p "[\"']"))
   ;; This inhibit function right here is local to python-mode
   ;; buffers.  Also give our default predicate function a chance to
   ;; inhibit pairing.
   (let ((default-predicate
          (when (default-boundp 'electric-pair-inhibit-predicate)
            (default-value 'electric-pair-inhibit-predicate))))
     (when (functionp default-predicate)
       (funcall default-predicate char)))))

(defun my:python-electric-pair-skip-whitespace-p ()
  (let ((string-start
         (nth 8 (syntax-ppss))))
    (and (not (and string-start
                   (save-excursion
                     (goto-char string-start)
                     (looking-at "[\"']\\{3\\}"))))
         'chomp)))

(defun my:python-mode-hook ()
  (my:setq-local indent-tabs-mode nil
                 tab-width 4
                 ;; No space inserted when I use M-(.
                 parens-require-spaces nil

                 ;; python.el's default forward-sexp-function
                 ;; behavior is not acceptable to me.  Also, it
                 ;; makes autopair very slow.
                 ;;
                 ;; Test case, point at |:
                 ;;
                 ;;     |x, y, z = [], [], []
                 ;;
                 ;; Hit `forward-sexp' or `kill-sexp'.  It ends up
                 ;; moving over/killing the entire line!
                 forward-sexp-function nil

                 electric-pair-inhibit-predicate
                 #'my:python-electric-pair-inhibit

                 electric-pair-skip-whitespace
                 #'my:python-electric-pair-skip-whitespace-p)
  ;; Must toggle `whitespace-mode' if we just changed `tab-width'.
  ;; (In case you're wondering, this is how `whitespace-mode' itself
  ;; enacts any options you change with `whitespace-toggle-options'.)
  (when whitespace-mode
    (whitespace-mode -1)
    (whitespace-mode 1))

  ;; Don't reindent if we're already at an acceptable level.  For
  ;; example:
  ;;
  ;;     if True:
  ;;         if False:
  ;;             print("foo")
  ;;     else|
  ;;
  ;; With point at "|", typing ":" moves the "else" to be in the "if
  ;; False" block, rather than the "if True" block *where you
  ;; manually de-indented to*.
  (add-hook 'electric-indent-functions
            #'my:python-mode-inhibit-electric-indent nil t))

(my:add-hooks 'python-mode-hook
  #'my:python-mode-hook
  #'my:warn-white-space-mode
  #'electric-pair-local-mode
  #'smart-tabs-mode
  #'subword-mode)

(smart-tabs-advise 'python-indent-line 'python-indent-offset)
(smart-tabs-advise 'python-indent-region 'python-indent-offset)
(smart-tabs-advise 'python-indent-shift-left 'python-indent-offset)
(smart-tabs-advise 'python-indent-shift-right 'python-indent-offset)
(smart-tabs-advise 'python-indent-calculate-levels 'python-indent-offset)
(smart-tabs-advise 'python-indent-post-self-insert-function
                   'python-indent-offset)

(which-key-add-major-mode-key-based-replacements 'python-mode
    "C-c C-t" "skeletons")

(add-hook 'inferior-python-mode-hook #'electric-pair-local-mode)

(defun my:python-reformat-region-or-buffer ()
  (interactive)
  (if (use-region-p)
      (black-format-region (region-beginning) (region-end))
    (save-some-buffers)
    (isort-format-buffer)
    (black-format-buffer)))

(defun my:python-shell-send-dwim ()
  (interactive)
  (cond
    ((use-region-p)
     (call-interactively #'python-shell-send-region))
    ((python-info-current-defun)
     (call-interactively #'python-shell-send-defun))
    (t
     (save-excursion
       (python-nav-beginning-of-statement)
       (setq start (point))
       (python-nav-end-of-statement)
       (setq end (point)))
     (python-shell-send-region start end))))

(with-eval-after-load 'python
  (bind-keys :map python-mode-map
             ("M-m m q" . my:python-toggle-triple-quotes)
             ("C-c '" . my:python-edit-indirect-dwim)
             ("M-m m f" . my:python-reformat-region-or-buffer)
             ("M-m m F" . black-format-on-save-mode)
             ("C-c C-c" . my:python-shell-send-dwim)
             ("C-c C-b" . python-shell-send-buffer)
             ("M-m m i" . my:python-add-import))

  (my:load-recipes 'python-magic-quotes
                   'python-toggle-triple-quotes
                   'python-fix-dead-shell-font-lock-buffer
                   'expand-region-python-fix-strings
                   'python-edit-indirect-in-strings
                   'python-add-import))
(require 'editorconfig)

(defun my:maybe-enable-black-format-on-save ()
  ;; Guard on `buffer-file-name' needed to avoid Elpy stuffing Python
  ;; into temporary buffers when sending code to the REPL.
  (when buffer-file-name
    (let ((props (funcall editorconfig-get-properties-function)))
      (when (equal (gethash 'org.codefu/python_formatter props) "black")
        (black-format-on-save-mode 1)))))

(add-hook 'python-mode-hook #'my:maybe-enable-black-format-on-save)


;;; pyvenv

(exec-path-from-shell-copy-envs '("PYTHONPATH"  "WORKON_HOME"))

(setq pyvenv-mode-line-indicator nil)

(underlings-move-menu-with-one-time-hook 'pyvenv-mode "Virtual Envs" "Minors"
                                         :dest-map global-map :visible t)

(pyvenv-tracking-mode 1)


;;; rainbow-mode

(with-eval-after-load 'rainbow-mode
  (add-to-list 'rainbow-html-colors-major-mode-list 'web-mode))


;;; recentf-mode

(setq recentf-max-saved-items 1000)

(recentf-mode 1)


;;; rect

(my:load-recipes 'emacs-yank-rectangle-to-new-lines)


;;; reformatter

(reformatter-define black-format
    :program "black"
    :args '("-l" "80" "--quiet" "--fast" "-")
    :lighter "Bl")

(reformatter-define isort-format
    :program "isort"
    :args '("-")
    :lighter "Ist")

(reformatter-define yapf-format
    :program "yapf"
    :lighter "Yapf")

(reformatter-define autopep8-format
    :program "autopep8"
    :args '("-aa" "-")
    :lighter "AP8")

(reformatter-define docformatter
    :program "docformatter"
    :args '("-")
    :lighter "Df")

(reformatter-define rufo
    :program "rufo-emacs-wrapper"
    :lighter " Rufo")


(reformatter-define eslint-format
    ;; This is a script of my own.
    :program "eslint-reformat"
    :lighter " ESL")


;;; replace

;; I get bothered when I can't do M-s o for `occur' in buffers using
;; `paredit-mode', which squats on M-s.
(bind-keys ("M-m s o" . occur))


;;; saveplace

(save-place-mode 1)


;;; scroll-bar-mode

(scroll-bar-mode -1)


;;; server

(server-start)


;;; sh-script

(defun my:sh-mode-hook ()
  (my:setq-local indent-tabs-mode t
                 tab-width 4
                 ;; Mode name "Shell-script" too long!
                 mode-name "sh"
                 company-backends (cons '(company-shell
                                          company-keywords
                                          company-dabbrev-code)
                                        company-backends)))

;; Probably do want `my:warn-white-space-mode' to come after
;; `my:sh-mode-hook' which sets `tab-width'.
(my:add-hooks 'sh-mode-hook
  #'my:warn-white-space-mode
  #'smart-tabs-mode
  #'my:sh-mode-hook)

;; XXX This is a bad idea, since it potentially steps on any other
;; modes that use smie.  Need to find a better way.
(smart-tabs-advise 'smie-indent-line 'sh-basic-offset)

;; I wasn't getting completions for "then" and "fi".  "fi" was
;; really screwing me up since a short pause let company-mode kick
;; in and I end up hitting RET and turning my intended "fi" into
;; "finish_some_other_stuff" or something equally ridiculous.  This
;; fixes that.  I am also worried that I am somehow overriding
;; normal completions, which may not be to my benefit.  May also
;; want/need to not complete in strings/comments, not sure.

(defun my:sh-add-company-keywords ()
  (set (make-local-variable 'company-keywords-alist)
       (list (cons 'sh-mode (seq-mapcat #'sh-feature
                                        (list sh-builtins
                                              sh-leading-keywords
                                              sh-other-keywords))))))

(add-hook 'sh-set-shell-hook #'my:sh-add-company-keywords)


;;; shackle

(shackle-mode 1)

(my:load-recipes 'shackle-dismiss-pop-up-window)

(setq my:shackle-pop-up-buffer-predicate #'my:pop-up-buffer-p)

(setq shackle-rules
      '(
        ((:custom my:pop-up-buffer-p)
         :custom my:shackle-display-pop-up-window
         :popup t :align below :select t :size 0.33)
        ))


;;; shift-number

(bind-keys ("C-s-=" . shift-number-up)
           ("C-s--" . shift-number-down))


;;; simple

(column-number-mode 1)

(setq set-mark-command-repeat-pop t
      backward-delete-char-untabify-method nil
      save-interprogram-paste-before-kill t)

;; My Moom (macOS) configuration will maximize window on M-=, the
;; default binding for `count-words-region'.  Put it elsewhere so I
;; can use it.
(bind-key "C-M-=" 'count-words-region)

(with-eval-after-load 'simple
  (add-to-list 'which-key-replacement-alist
               '(("C-x @" . "Prefix Command") . (nil . "add modifier"))))


;;; sly

(when (executable-find "sbcl")
  (setq inferior-lisp-program "sbcl"))

(defun my:sly-dont-enable-for-fennel (&rest args)
  (not (derived-mode-p 'fennel-mode)))

(advice-add 'sly-editing-mode :before-while #'my:sly-dont-enable-for-fennel)


;;; smartparens

;; Need this so ' is configured in lisp modes correctly, so that my
;; lisp-comment-dwim works.
(with-eval-after-load 'smartparens
  (require 'smartparens-config))

;; I don't usually care for smartparens, but it does have some useful
;; functions that duplicate paredit functionality I've found fond of,
;; but which work outside of `paredit-mode' (and `smartparens-mode',
;; apparently).
;;
;; So maybe you'd say I do "use smartparens", but I just don't use
;; `smartparens-mode'.  `paredit-mode' kills it in lisps, and
;; `electric-pair-mode' is fine/more reliable everywhere else.

(dolist (command '(sp-split-sexp
                   sp-join-sexp
                   sp-splice-sexp
                   sp-raise-sexp
                   sp-forward-barf-sexp
                   sp-backward-barf-sexp
                   sp-forward-slurp-sexp
                   sp-backward-slurp-sexp))
  (unless (commandp command)
    (autoload command "smartparens" nil t))
  (my:make-repeatable-command (intern (format "my:%S-repeat" command)) command))

(bind-keys
 ("M-m x s" . my:sp-split-sexp-repeat)
 ("M-m x j" . my:sp-join-sexp-repeat)
 ("M-m x S" . my:sp-splice-sexp-repeat)
 ("M-m x r" . my:sp-raise-sexp-repeat)
 ("M-m x u" . my:sp-forward-slurp-sexp-repeat)
 ("M-m x U" . my:sp-backward-slurp-sexp-repeat)
 ("M-m x b" . my:sp-forward-barf-sexp-repeat)
 ("M-m x B" . my:sp-backward-barf-sexp-repeat))

(my:load-recipes 'smartparens-split-python-triple-quoted-strings)


;;; sql

;; Don't select the SQLi window when eval'ing some SQL.  That's
;; annoying.
(setq sql-display-sqli-buffer-function #'display-buffer)

;; But do select the SQLi window when I run `sql-postgres' or the
;; like.
(define-advice sql-product-interactive
    (:around (orig-fun &rest args) my:select-sqli-buffer)
  (let ((sql-display-sqli-buffer-function t))
    (apply orig-fun args)))

(with-eval-after-load 'sql
  (bind-keys :map sql-mode-map
             ;; C-c C-z should select the SQLi buffer, not just show it.
             ("C-c C-z" . sql-product-interactive)))

;; Set the default to my most commonly-used RDBMS.
(setq sql-product 'postgres)

(with-eval-after-load 'sql
  ;; Make Postgres's name shorter in the mode line.
  (sql-set-product-feature 'postgres :name "Pg")

  ;; Missing the 'db$#' and 'db"#' variant prompts from psql when
  ;; you're inside of $$ or "" quoting, respectively.  Add them here.
  ;; I should upstream this.
  (unless (equal (sql-get-product-feature 'postgres :prompt-cont-regexp)
                 "^[[:alnum:]_]*[-(][#>] ")
    (warn "sql.el changed PostgreSQL :prompt-cont-regexp, edit your init."))

  (sql-set-product-feature 'postgres :prompt-cont-regexp
                           "^[[:alnum:]_]*[-($\"'][#>] ")

  ;; With readline on (or libedit on macOS?) things like
  ;; `sql-send-buffer' will sometimes apparently corrupt the data sent
  ;; to psql.
  (add-to-list 'sql-postgres-options "--no-readline")

  ;; PostgreSQL connections should ask for the port.
  (add-to-list 'sql-postgres-login-params 'port t))

;; Used by my expand-region setup, see below.
(defun my:sql-mark-statement ()
  (interactive)
  (sql-end-of-statement 1)
  (push-mark nil t t)
  (sql-beginning-of-statement 1))

(defun my:sql-mode-hook ()
  (my:setq-local tab-width 4

                 electric-pair-inhibit-predicate
                 #'my:electric-pair-default-plus-before-word-inhibit

                 electric-pair-skip-whitespace 'chomp)
  (my:warn-white-space-mode)
  (set (make-local-variable 'er/try-expand-list)
       (copy-tree er/try-expand-list))
  (my:add-to-list-before 'er/try-expand-list
                         'my:sql-mark-statement 'er/mark-next-accessor))

(my:add-hooks 'sql-mode-hook
  #'sqlind-minor-mode
  #'smart-tabs-mode
  #'my:sql-mode-hook)

;; sql-mode installs hook(s) that will screw up things like
;; column-marker or whitespace-mode initially in a buffer, and also
;; (I think) whenever you change products.  This is here to
;; reinitialize whitespace-mode after changing products (which, to
;; reiterate, happens during creating a new sql-mode buffer).
;;
;; BTW, I think the hooks in question are either the one going in to
;; hack-local-variables-hook or else font-lock-mode-hook.
(define-advice sql-product-font-lock
    (:around (orig-fun &rest args) my:restore-whitespace-mode activate)
  (let ((whitespace-mode-was-active (and (boundp 'whitespace-mode)
                                         whitespace-mode)))
    (when whitespace-mode-was-active
      (whitespace-mode -1))
    (prog1
        (apply orig-fun args)
      (when whitespace-mode-was-active
        (whitespace-mode 1)))))

(my:add-hooks 'sql-interactive-mode-hook
  #'company-mode
  #'sql-set-sqli-buffer-generally)

(with-eval-after-load 'company-dabbrev-code
  (add-to-list 'company-dabbrev-code-modes 'sql-interactive-mode))

(defun my:sql-format-region (start end)
  (interactive "r")
  (let* ((exec-path (cons (expand-file-name "~/bin") exec-path))
         (sqlformat (executable-find "sqlformat")))
    (unless sqlformat
      (error "Can't find sqlformat program"))
    (call-process-region start end sqlformat t t nil
                         "-r" "--indent_width" "4" "-a" "-s"
                         "--wrap_after" "79" "-k" "upper" "-")))


;;; sql-indent

(define-advice sqlind-minor-mode (:before (&optional arg)
                                          my:turn-off-faux-indent-mode)
  (when (and (bound-and-true-p faux-indent-mode)
             (my:minor-mode-arg-will-turn-on arg sqlind-minor-mode))
    (faux-indent-mode -1)))

;; Test case:
;;
;;     SELECT 1 FROM
;;         foo;
(defun my:sqlind-indent-to-start-of-anchor-line (syntax base-indentation)
  (save-excursion
    (goto-char (cdar syntax))
    (back-to-indentation)
    (current-column)))

;; Test cases:
;;
;;     SELECT
;;         (foo
;;          - bar);
;;
;;
;;     SELECT * FROM
;;         foo
;;         JOIN (
;;             bar
;;             JOIN baz
;;                 ON bar.k = baz.k
;;         ) USING (k)
;;
;;     CREATE TABLE foo (
;;         bar INT,
;;         baz INT
;;             REFERENCES foo (bar)
;;     );
(defun my:sqlind-line-up-nested-continuations (syntax base-indentation)
  (let ((start (cdar syntax)))
    ;; Test if we're inside something like a CREATE TABLE comment list.
    (if (save-excursion
          (goto-char start)
          (and (zerop (nth 0 (syntax-ppss)))
               (progn (sqlind-beginning-of-statement)
                      ;; Ugly (internal) API.
                      (eq (nth 2 (catch 'finished
                                   (sqlind-maybe-create-statement)))
                          'table))))
        ;; We're inside some kind of CREATE TABLE column list or
        ;; something like that.
        (+
         ;; Start with the indentation of the anchor line.  (Don't use
         ;; the column where the anchor is (AKA `base-indenation'),
         ;; since that's probably like an open parenthesis after
         ;; CREATE TABLE foo, so column is likely to be totally
         ;; wrong.)
         (save-excursion
           (goto-char start)
           (back-to-indentation)
           (current-column))
         ;; Add one indent level on top of that.
         sqlind-basic-offset
         ;; If previous line ended with a comma, add no further
         ;; indentation.  Otherwise, add an additional level.
         (if (save-excursion
               (and (zerop (forward-line -1))
                    (progn
                      (end-of-line)
                      (save-restriction
                        ;; `forward-comment' moves over newline.
                        (narrow-to-region (line-beginning-position) (point))
                        (while (or
                                (let ((state (syntax-ppss)))
                                  ;; Move out of a comment we're in.
                                  (and (nth 4 state)
                                       (goto-char (nth 8 state))))
                                ;; Move over white space and/or
                                ;; comment before point.  This may
                                ;; move over white space and then
                                ;; return nil because it didn't find a
                                ;; comment, but point is still moved
                                ;; so we use this as a cute way to
                                ;; skip white space.
                                (forward-comment -1))))
                      (eq (char-before) ?,))))
             0
           sqlind-basic-offset))
      (save-excursion
        (goto-char start)
        (skip-syntax-forward "(-")
        (if (not (eolp))
            (+ base-indentation 1)
          (back-to-indentation)
          (+ (current-column) sqlind-basic-offset))))))

;; Indent successive lines of ALTER TABLE by one step:
;;
;;     ALTER TABLE foo
;;         ALTER COLUMN bar SET NOT NULL;
(defun my:sqlind-indent-alter-table (syntax base-indentation)
  (save-excursion
    (goto-char (cdar syntax))
    (+ base-indentation
       (if (let ((case-fold-search t))
             (looking-at-p "alter\\s-+table"))
           sqlind-basic-offset
         0))))

;; Indent one extra level within a transaction.  Does not handle
;; nested transactions and just generally isn't super-smart, but
;; hopefully gets the job done.
;;
;;     BEGIN WORK;
;;
;;         SELECT 'this is indented one extra level';
;;
;;     COMMIT;
(defun my:sqlind-indent-inside-transaction (syntax base-indentation)
  (save-excursion
    (let ((case-fold-search t))
      ;; I bet this setup isn't even remotely fool-proof.
      (forward-line 0)
      (skip-syntax-forward "-")
      (cond
        ;; Are we actually indenting a COMMIT/ROLLBACK/END WORK?  If
        ;; so, remove a level of indentation.
        ((looking-at-p (rx symbol-start
                           (or (: (or "commit" "rollback"))
                               ;; We require WORK or TRANSACTION after
                               ;; END, since END may have other
                               ;; meanings.
                               (: "end" (1+ space) (or "work" "transaction")))
                           symbol-end))
         (max (- base-indentation sqlind-basic-offset) 0))
        ;; Look backwards for nearest BEGIN/COMMIT/END WORK and add a level
        ;; of indentation if the first match is BEGIN WORK.
        ((and (sqlind-search-backward (point)
                                      ;; BEGIN (WORK|TRANSACTION)
                                      ;; COMMIT [WORK|TRANSACTION]
                                      ;; ROLLBACK [WORK|TRANSACTION]
                                      ;; END (WORK|TRANSACTION)
                                      ;;
                                      ;; As mentioned in a prior
                                      ;; comment, BEGIN and END may be
                                      ;; used for other purposes, so
                                      ;; we require them to have WORK
                                      ;; or TRANSACTION afterwards.
                                      ;; We don't have the same
                                      ;; requirement on COMMIT and
                                      ;; ROLLBACK, which are less
                                      ;; ambiguous.
                                      (rx symbol-start
                                          (or (or "commit" "rollback")
                                              (: (or "begin" "end")
                                                 (1+ space)
                                                 (or "work" "transaction")))
                                          symbol-end)
                                      nil)
              (sqlind-looking-at-begin-transaction))
         (+ base-indentation sqlind-basic-offset))
        ;; No change in indentation.
        (t base-indentation)))))

(defun my:sqlind-indent-line-comment (syntax base-indentation)
  "Indent the \"--\" comment on this line to the next or previous line."
  ;; Ensure we're called on a comment line.
  (or
   (when (save-excursion
           (back-to-indentation)
           (looking-at-p "\\s-*--"))
     ;; Is this a continuation of a comment from the previous line?
     ;; If so, indent to match.
     (save-excursion
       (when (zerop (forward-line -1))
         (back-to-indentation)
         (when (looking-at-p "\\s-*--")
           (current-column))))
     ;; Previous line is not a comment.  Is the next line something
     ;; other than a blank line?  If so, use its indent.
     (save-excursion
       (when (zerop (forward-line 1))
         (back-to-indentation)
         (unless (eolp)
           (current-column)))))
   ;; Use sql-indent's default behavior.
   (sqlind-indent-comment-start syntax base-indentation)))

;; Remove one level of indentation from BEGIN after CREATE FUNCTION so
;; that the BEGIN is at the same level as the CREATE FUNCTION.
;;
;;     CREATE FUNCTION foo() RETURNS void AS $$
;;     BEGIN
;;     END;
;;     $$ LANGUAGE plpgsql;
(defun my:sqlind-indent-in-begin-block (syntax base-indentation)
  (if (eq (nth 1 (caar syntax)) 'defun)
      base-indentation
    (+ base-indentation sqlind-basic-offset)))

;; Skip over psql \... commands when finding beginning of statement.
;; This makes indenting work right.  (But maybe it breaks motion.)
;; Should this get pushed upstream?  (Do I still need this after I
;; modified `sqlind-beginning-of-directive', which see?)

(defun my:sqlind-beginning-of-statement-skip-psql (&rest _args)
  (while (looking-at-p "^\\s-*\\\\")
    (forward-line 1)
    (sqlind-forward-syntactic-ws)))

(advice-add 'sqlind-beginning-of-statement :after
            #'my:sqlind-beginning-of-statement-skip-psql)

;; Teach sql-indent about psql directives.  I should definitely push
;; this upstream.

(defconst sqlind-psql-directive
  "^\\s-*\\\\[a-zA-Z?!]"
  "Match a psql directive at the beginning of a line.
psql directives are always on a single line, by themselves.")

(el-patch-feature sql-indent)

(with-eval-after-load 'sql-indent
  (el-patch-defun sqlind-beginning-of-directive ()
    "Return the position of an SQL directive, or nil.
We will never move past one of these in our scan.  We also assume
they are one-line only directives."
    (let ((rx (cl-case (and (boundp 'sql-product) sql-product)
                (ms sqlind-ms-directive)
                (sqlite sqlind-sqlite-directive)
                (oracle sqlind-sqlplus-directive)
                (el-patch-add
                  (postgres sqlind-psql-directive))
                (t nil))))
      (when rx
        (save-excursion
          (when (re-search-backward rx nil 'noerror)
            (forward-line 1)
            (point))))))

  (el-patch-validate 'sqlind-beginning-of-directive 'defun t))

;; Another patch: need to indent a continued join inside an UPDATE's
;; FROM clause:
;;
;;     UPDATE foo SET bar = TRUE
;;     FROM
;;         t1
;;         JOIN t2
;;             -- This adds this extra level of indent here:
;;             ON t1.k = t2.k
;;     WHERE foo.k = t1.k;

(el-patch-feature sql-indent)

(with-eval-after-load 'sql-indent
  (el-patch-defun sqlind-syntax-in-update (pos start)
    "Return the syntax at POS which is inside an \"update\" statement at START."
    (save-excursion
      (catch 'finished
        (goto-char pos)

        ;; all select query components are indented relative to the start of the
        ;; select statement)
        (when (looking-at sqlind-update-clauses-regexp)
          (throw 'finished (cons 'update-clause start)))

        (while (re-search-backward sqlind-update-clauses-regexp start t)
          (let* ((match-pos (match-beginning 0))
                 (clause (sqlind-match-string 0)))
            (setq clause (replace-regexp-in-string "[ \t\r\n\f]" " " clause))
            (when (sqlind-same-level-statement (point) start)
              (throw 'finished
                (el-patch-wrap 3
                    (if (string= clause "from")
                        (sqlind-syntax-in-select pos start)
                      (cons (list 'in-update-clause clause) match-pos))))))))))

  (el-patch-validate 'sqlind-syntax-in-update 'defun t))

;; Pretty sure this is fixing a bug in this function, and so pretty
;; sure I should upstream this.

(el-patch-feature sql-indent)

(with-eval-after-load 'sql-indent
  (el-patch-defun sqlind-same-level-statement (point start)
    "Return t if POINT is at the same syntactic level as START.
This means that POINT is at the same nesting level and not inside
a string or comment."
    (save-excursion
      (let ((ppss-point (syntax-ppss point))
            (ppss-start (syntax-ppss start)))
        (and (equal (nth 3 ppss-point) (nth 3 ppss-start)) ; string
             (equal (nth 4 (el-patch-swap ppss-start ppss-point))
                    (nth 4 ppss-start)) ; comment
             (= (nth 0 ppss-point) (nth 0 ppss-start)))))) ; same nesting

  (el-patch-validate 'sqlind-same-level-statement 'defun t))

;; Add DO.  Should probably push this upstream.
(with-eval-after-load 'sql-indent
  (defconst sqlind-start-block-regexp
    (concat "\\(\\_<"
            (regexp-opt '("if" "then" "else" "elsif" "loop"
                          "begin" "declare" "create" "alter" "exception"
                          "procedure" "function" "end" "case"
                          "do")
                        t)
            "\\_>\\)\\|)\\|\\$\\$")
    "Regexp to match the start of a block."))

;; Maybe make DO work like an anonymous defun-start?  Trying this out.
;; May want to upstream.

(defun my:sqlind-maybe-defun-from-do-statement (orig-fun &rest args)
  (if (looking-at-p "\\_<do\\_>")
      (throw 'finished (list 'defun-start ""))
    (apply orig-fun args)))

(advice-add 'sqlind-maybe-defun-statement :around
            #'my:sqlind-maybe-defun-from-do-statement)

;; I have chosen to "edit" `sqlind-default-indentation-offsets-alist'
;; to produce `my:sqlind-indentation-offsets-alist', rather than
;; redefining parts of it as the manual suggests.  Future-proof,
;; amirite?!

(defmacro my:set-sqlind-offset (syntax &rest values)
  (declare (indent 1))
  `(setf (alist-get ,syntax my:sqlind-indentation-offsets-alist) ',values))

(defmacro my:append-sqlind-offset (syntax &rest values)
  (declare (indent 1))
  (let ((syn-var (gensym)))
    `(let* ((,syn-var ,syntax))
       (setf (alist-get ,syn-var my:sqlind-indentation-offsets-alist)
             (append (alist-get ,syn-var my:sqlind-indentation-offsets-alist)
                     ',values)))))

(with-eval-after-load 'sql-indent
  (defvar my:sqlind-indentation-offsets-alist)

  ;; Breaking the assignment out into a separate `setq' so I can C-M-x
  ;; this whole block.  (`defvar' won't reassign inside a
  ;; `with-eval-after-load'.  Maybe I should fix that...)
  (setq my:sqlind-indentation-offsets-alist
        (copy-tree sqlind-default-indentation-offsets-alist))

  ;; Align to the left, as prescribed by sql-indent docs.
  (dolist (syntax '(select-clause
                    insert-clause
                    delete-clause
                    update-clause))
    (my:set-sqlind-offset syntax 0))

  (my:set-sqlind-offset 'create-statement
    ;; 0 is to not indentation after CREATE VIEW.
    0
    my:sqlind-indent-alter-table)

  ;; This plus my fancy function allows correct (to me) indentation of
  ;; the stuff in a FROM clause.
  (my:set-sqlind-offset 'select-table-continuation
    ;; Sometimes I write "SELECT 1 FROM" and I want to add an indent
    ;; level to that.  This function corrects the "base indentation"
    ;; to accommodate this.
    my:sqlind-indent-to-start-of-anchor-line
    +)

  (my:set-sqlind-offset 'select-join-condition +)

  (my:set-sqlind-offset 'nested-statement-continuation
    my:sqlind-line-up-nested-continuations)

  (my:set-sqlind-offset 'in-select-clause +)

  (my:set-sqlind-offset 'in-insert-clause +)

  (my:set-sqlind-offset 'in-update-clause +)

  (my:set-sqlind-offset 'comment-start my:sqlind-indent-line-comment)

  (my:set-sqlind-offset 'comment-continuation 0)

  ;; Work tells me they prefer one level indent inside BEGIN WORK.  I
  ;; defer to their preference.
  (my:set-sqlind-offset 'toplevel my:sqlind-indent-inside-transaction)

  (my:set-sqlind-offset 'in-begin-block my:sqlind-indent-in-begin-block)

  ;;     SELECT
  ;;         CASE
  ;;             WHEN foo
  ;;                 THEN bar
  ;;             WHEN
  ;;                 foo2
  ;;                 THEN
  ;;                     bar2
  ;;             ELSE baz
  ;;         END;

  (my:set-sqlind-offset 'case-clause +)

  (my:set-sqlind-offset 'case-clause-item-cont +)

  ;; Lone semicolon gets indented back to anchor (usually column 0).
  ;; These syntaxes are taken from sql-indent-left.el, which comes
  ;; with sql-indent.
  (dolist (syntax '(select-column
                    select-column-continuation
                    select-table-continuation
                    in-select-clause
                    in-delete-clause
                    in-insert-clause
                    in-update-clause))
    (my:append-sqlind-offset syntax sqlind-lone-semicolon))

  ;; For easy C-M-x of this form, let's see if we should apply these
  ;; settings to any existing buffers.
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'sql-mode)
        (setq sqlind-indentation-offsets-alist
              my:sqlind-indentation-offsets-alist)))))

(defun my:sqlind-minor-mode-hook ()
  (setq sqlind-basic-offset tab-width
        sqlind-indentation-offsets-alist my:sqlind-indentation-offsets-alist))

(add-hook 'sqlind-minor-mode-hook #'my:sqlind-minor-mode-hook)

(smart-tabs-advise 'sqlind-indent-line 'sqlind-basic-offset)


;;; sqlup-mode

(setq sqlup-blacklist '("name" "id" "label" "state" "sql" "operation"
                        ;; WTF sql.el?
                        "a" "c" "g" "k" "m" "p" "t"))

(with-eval-after-load 'sql
  (my:load-recipes 'sqlup-maybe-enable)

  (add-hook 'sql-mode-hook #'my:maybe-enable-sqlup-mode))


;;; startup

(setq inhibit-startup-screen t
      user-mail-address "dale@codefu.org")


;;; sticky-region

(sticky-region-mode 1)


;;; swiper

;; Note that `swiper-isearch' doesn't seem to be obeying
;; `ivy--regex-ignore-order'.  Maybe because "isearch is not
;; line-based"?

(bind-keys ("C-s" . swiper-isearch)
           ("C-r" . swiper-isearch-backward)
           ;; Put the old commands somewhere easy enough.
           ("s-s" . isearch-forward)
           ("s-r" . isearch-backward))

(setq swiper-goto-start-of-match t)


;;; terraform-mode

(defun my:terraform-mode-hook ()
  (push '(company-terraform company-dabbrev-code)
        (my:buffer-local-value 'company-backends)))

(my:add-hooks 'terraform-mode-hook
  #'terraform-format-on-save-mode
  #'my:terraform-mode-hook)


;;; tool-bar

(tool-bar-mode -1)


;;; treemacs

(setq treemacs-python-executable "/opt/local/bin/python3"
      treemacs-width 50)

(add-hook 'treemacs-hook #'treemacs-tag-follow-mode)

(bind-keys ("M-m u t" . treemacs))

(with-eval-after-load 'treemacs
  ;; Normal size says to use 22, or 44 if you have a high DPI display.
  ;; I have a high DPI display---but I don't think NS Emacs is able to
  ;; use it.  22 is too big, 44 is *way* too big!
  (treemacs-resize-icons 11))


;;; undo-tree

;; Undoing/redoing in a region confuses me, mostly because I'm always
;; doing it by accident.  I don't think I've ever used this
;; functionality on purpose.  Forcibly disable it.

(defun my:undo-only-but-never-in-a-region ()
  (interactive)
  (deactivate-mark)
  (call-interactively #'undo-only))

(defun my:undo-redo-but-never-in-a-region ()
  (interactive)
  (deactivate-mark)
  (call-interactively #'undo-redo))

(if (and (>= emacs-major-version 28) (fboundp 'undo-redo))
    ;; Trying new `undo-redo' in Emacs 28.
    (bind-keys ("C-/" . my:undo-only-but-never-in-a-region)
               ("s-z" . my:undo-only-but-never-in-a-region)
               ("M-_" . my:undo-redo-but-never-in-a-region)
               ("s-Z" . my:undo-redo-but-never-in-a-region))
  (global-undo-tree-mode)

  (bind-keys :map undo-tree-map
             ;; I want to use this for multiple-cursors.
             ("C-?" . nil))

  ;; I never expect to be able to undo in a region, but this is on by
  ;; default.  Turn it off.  Maybe someday I'll turn this back on (AKA
  ;; leave it on) if I find a good use for it.
  (setq undo-tree-enable-undo-in-region nil))


;;; unfill

(bind-key "M-q" 'unfill-toggle)


;;; unicode-fonts

;; The alternative:
;; (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil
;;                   'prepend)

(require 'unicode-fonts)

;; I build Emacs NeXTStep with the color fonts patch (for Apple Color
;; Emoji, natch).  Must override unicode-fonts turning said color
;; fonts off.
(when (memq window-system '(ns mac))
  (setq unicode-fonts-skip-font-groups (delq 'multicolor
                                             unicode-fonts-skip-font-groups)))

(unicode-fonts-setup)


;;; vc

(setq vc-follow-symlinks t)

(bind-keys ("M-m v g" . vc-annotate))

(my:load-recipes 'vc-use-icons-in-mode-line
                 'vc-truncate-long-branch-names)


;;; vcl-mode

(defun my:vcl-mode-hook ()
  (my:setq-local c-basic-offset 4))

(add-hook 'vcl-mode-hook #'my:vcl-mode-hook)


;;; volatile-highlights

(volatile-highlights-mode 1)


;;; web-mode

(setq web-mode-enable-auto-expanding t
      ;; web-mode's auto-pairing doesn't agree with
      ;; electric-pair-mode, which I actually prefer.  In reality, the
      ;; only pairs I really use on a regular basis are the ones for
      ;; {{ }} in Go, and electric-pair-mode handles those just fine.
      ;;
      ;; This does lose the pairing for <!-- -->.  Just hit M-; to
      ;; make a comment, lazy ass.
      web-mode-enable-auto-pairing nil
      ;; This will indent, like, all HTML in the vicinity when you
      ;; yank, which is often kind of awful.
      ;; https://github.com/fxbois/web-mode/issues/894
      web-mode-enable-auto-indentation nil
      web-mode-auto-close-style 2
      web-mode-extra-expanders '(("o/" . "<code>|</code>")))

;; `web-mode-use-tabs' sets up offset variables globally (and it will
;; use the current value of `tab-width' to do so).
(with-eval-after-load 'web-mode
  (let ((tab-width 4))
    (web-mode-use-tabs)))

(smart-tabs-advise 'web-mode-indent-line
                   ;; List of vars taken from `web-mode-use-tabs'.
                   'web-mode-attr-indent-offset
                   'web-mode-code-indent-offset
                   'web-mode-css-indent-offset
                   'web-mode-markup-indent-offset
                   'web-mode-sql-indent-offset)

;; Copied from http://web-mode.org/
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; We're "special" at work.
(add-to-list 'auto-mode-alist '("\\.html\\.translate\\'" . web-mode))

;; Open all HTML-looking files with `web-mode', not [m]html-mode.
;; BTW, can't `setf' the cdr of these pairs, I think the cons cells
;; are in "pure memory," because attempting to modify them with setf
;; fails saying they're read-only.  See use of purecopy in files.el,
;; where `magic-fallback-mode-alist' and `auto-mode-alist' are set.
(setq magic-fallback-mode-alist (mapcar (lambda (pair)
                                          (if (memq (cdr pair) '(html-mode
                                                                 mhtml-mode))
                                              (cons (car pair) 'web-mode)
                                            pair))
                                        magic-fallback-mode-alist))

(defun my:web-mode-hook ()
  (setq indent-tabs-mode t
        tab-width 4)
  (push '(company-web-html company-dabbrev-code)
        (my:buffer-local-value 'company-backends)))

(my:add-hooks 'web-mode-hook
  #'my:web-mode-hook
  #'smart-tabs-mode
  #'emmet-mode
  #'rainbow-mode
  #'my:warn-white-space-mode)

(defun my:web-mode-after-set-engine-hook ()
  (when (equal web-mode-engine "velocity")
    (setq-local web-mode-imenu-regexp-list
                (append web-mode-imenu-regexp-list
                        `((,(rx bol
                                (* space)
                                "#"
                                (group (| "def" "attr"))
                                (+ space)
                                (group symbol-start
                                       (+? not-newline)
                                       symbol-end))
                            1 2 ": "))))))

(advice-add 'web-mode-on-engine-setted :after
            #'my:web-mode-after-set-engine-hook)

(with-eval-after-load 'web-mode
  (bind-keys :map web-mode-map
             ("C-M-u" . web-mode-element-parent)
             ("C-M-d" . web-mode-element-child)
             ("C-M-n" . web-mode-element-end)
             ("M-m m i" . imp-visit-buffer))

  ;; I mostly work in Cheetah templates, which look a lot like
  ;; Velocity templates, which do interpolation like $foo.  I don't
  ;; want "$" to be part of the word "foo", it complicates things like
  ;; Swiper's "search word under point".
  ;;
  ;; I reserve the right to later consider this to be a bad idea.
  ;;
  ;; I'm using "\" (escape syntax) because that's what cperl-mode uses
  ;; for $.
  ;;
  ;; The syntax for $ was previously "w"ord, as of this writing.
  (modify-syntax-entry ?$ "\\" web-mode-syntax-table))

(which-key-add-major-mode-key-based-replacements 'web-mode
    "C-c C-a" "attribute"
    "C-c C-b" "block"
    "C-c C-d" "dom"
    "C-c C-e" "element"
    "C-c C-t" "tag")

(my:load-recipes 'web-mode-engine-local-variable)


;;; webpaste

;; I should upstream this.
(with-eval-after-load 'webpaste
  (let* ((provider (alist-get "ix.io" webpaste-providers-alist
                              nil nil #'string=))
         (langs (memq :lang-overrides provider)))
    (if (or (assq 'latex-mode webpaste--default-lang-alist)
            (assq 'latex-mode (cadr langs)))
        (warn (concat "webpaste has now configured latex-mode for us,"
                      " we can now remove some code from init.el"))
      (push '(latex-mode . "latex") (cadr langs)))))


;;; which-func

(add-hook 'prog-mode-hook #'which-function-mode)

(with-eval-after-load 'which-func
  (my:load-recipes 'which-function-in-header-line
                   'which-function-update-while-in-minibuffer))

;;; whitespace

;; This is the default value except when in space visualization mode
;; (the car, not the cdr, used when indent-tabs-mode is non-nil); in
;; that case, only visualize indentation that begins with spaces.
;; Otherwise it'll visualize spaces after initial tabs, which is
;; something I quite like ("smart tabs").
(setq whitespace-indentation-regexp
      '("^\\(\\( \\{%d\\}\\)+\\)[^\n\t]" . "^ *\\(\t+\\)[^\n]"))

(with-eval-after-load 'whitespace
  (face-spec-set 'whitespace-line '((t (:background "#ffe7e7"
                                        :foreground nil
                                        :distant-foreground "black")))))


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


;;; window-hydra

;; This is normally `split-line' which I have never knowingly used and
;; doubt I ever would.
(bind-key "C-M-o" 'window-hydra/body)


;;; winner-mode

(winner-mode 1)


;;; winum

(setq winum-auto-setup-mode-line nil)

(winum-mode 1)

(bind-keys :map winum-keymap
           ("C-0" . winum-select-window-0-or-10)
           ("C-1" . winum-select-window-1)
           ("C-2" . winum-select-window-2)
           ("C-3" . winum-select-window-3)
           ("C-4" . winum-select-window-4)
           ("C-5" . winum-select-window-5)
           ("C-6" . winum-select-window-6)
           ("C-7" . winum-select-window-7)
           ("C-8" . winum-select-window-8)
           ("C-9" . winum-select-window-9))

(my:load-recipes 'winum-sort-like-ace-window
                 'winum-fancy-numbers-in-mode-line)


;;; xref

;; Note: Maybe I should ask upstream to make `xref-etags-mode'
;; autoloaded, since I do use it in a .dir-locals.el.

;; These are versions of `xref-next-line' and `xref-prev-line' that
;; just move the cursor to the next/previous location without also
;; actually jumping to that location.

(defun my:xref-next-line-without-showing ()
  "Move to next xref item (without showing that match)."
  (interactive)
  (xref--search-property 'xref-item))

(defun my:xref-prev-line-without-showing ()
  "Move to previous xref item (without showing that match)."
  (interactive)
  (xref--search-property 'xref-item t))

(with-eval-after-load 'xref
  (bind-keys :map xref--xref-buffer-mode-map
             ;; I don't want n/p to show the match I'm moving to, just
             ;; move there and let me show it with RET/TAB if I want.
             ("n" . my:xref-next-line-without-showing)
             ("p" . my:xref-prev-line-without-showing)
             ;; Reverse these bindings from the default
             ("RET" . xref-quit-and-goto-xref)
             ("TAB" . xref-goto-xref)
             ;; And this is a map, set using text properties on
             ;; matches you can jump to in the *xref* buffer.  It also
             ;; binds RET (well, C-m I guess).
             :map xref--button-map
             ([(control ?m)] . xref-quit-and-goto-xref)
             ;; Shit, it doesn't explicitly bind RET but let's just
             ;; make sure.
             ("RET" . xref-quit-and-goto-xref)))


;;; yaml-mode

(defun my:yaml-backtab (arg)
  (interactive "*p")
  (let ((in-indentation (<= (current-column) (current-indentation))))
    (save-excursion
      (back-to-indentation)
      ;; Not really perfect: may just backspace a character.  But often
      ;; good enough?
      (if (bolp)
          (user-error "No more indentation on this line")
        (yaml-electric-backspace arg)))
    (when in-indentation
      ;; Make line with just " ", put point at end of line, call this
      ;; function: without this `back-to-indentation', you end up with
      ;; cursor somewhere other than end of line, in the middle of the
      ;; indentation.  I never looked into why that happens, but this
      ;; behavior is probably preferable in any case.
      (back-to-indentation))))

(with-eval-after-load 'yaml-mode
  (bind-keys :map yaml-mode-map
             ("RET" . newline-and-indent)
             ("<backtab>" . my:yaml-backtab))

  (add-to-list 'yaml-imenu-generic-expression
               '("Ansible" "^-\\s-+name: \\(.*\\)" 1)))


;;; yasnippet

(yas-global-mode 1)

;; Always prompt with (I hope) Ivy.
(setq yas-prompt-functions '(yas-completing-prompt))

;; Don't use TAB to expand snippets.  Then I do stuff like type
;; (cursor at |) "inseringd|foo", hit TAB to reindent my Python line,
;; and end up expanding a docstring (WTF).
;;
;; Instead we'll set up something under my "leader key" to emulate
;; Spacemacs bindings.
(bind-keys :map yas-minor-mode-map
           ("<tab>" . nil)
           ("TAB" . nil)
           ("M-m i s" . ivy-yasnippet))

;; Also let hippie-expand expand snippets.
(with-eval-after-load 'hippie-exp
  (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand))

(with-eval-after-load 'which-key
  (add-to-list 'which-key-replacement-alist
               '(("C-c &" . "Prefix Command") . (nil . "yasnippet") )))


;;; zop-to-char

(bind-keys ("M-z" . zop-up-to-char))


;;; Epilogue

;; Sometimes init.el doesn't get to load all the way to the end, but
;; errors get swallowed and don't appear in *Messages*?  WTF.
(message "init.el loaded successfully")
