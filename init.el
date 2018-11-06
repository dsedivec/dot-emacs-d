;; -*- lexical-binding: t; -*-

;;;; Prologue

;; Prefer loading a newer .el to an older .elc.  Probably keeps me
;; from getting in trouble if I forget to byte compile.
(setq load-prefer-newer t)

(defvar my:local-packages-dir
  (expand-file-name "lisp" (file-name-directory load-file-name)))

(add-to-list 'load-path my:local-packages-dir)

(require 'cl-lib)
(require 'subr-x)


;;; Local packages

(defvar my:local-packages-autoload-file
  (expand-file-name "autoloads.el" my:local-packages-dir))

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


;;; package.el with auto-package-update

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives
             '("my:org" . "~/repositories/org-mode/"))
(add-to-list 'package-pinned-packages '(org-plus-contrib . "my:org"))

(package-initialize)

(defvar my:packages
  '(
    ;; Best to make sure quelpa is here at the top, before any
    ;; following quelpa recipes which will require quelpa, naturally.
    quelpa

    adaptive-wrap
    ;; Until https://github.com/domtronn/all-the-icons.el/pull/106 gets merged:
    ;; all-the-icons
    (all-the-icons :fetcher github :repo "ubolonton/all-the-icons.el"
                   :branch "font-lock-fix" :files (:defaults "data"))
    amx
    auctex
    auto-package-update
    avy
    bind-key
    clean-aindent-mode
    comment-dwim-2
    company
    company-anaconda
    company-statistics
    counsel
    csv-mode
    dash
    deft
    dtrt-indent
    edit-indirect
    (eltu :fetcher github :repo "dsedivec/eltu"
          :files (:defaults "eltu_update_tags.py"))
    exec-path-from-shell
    expand-region
    flx
    flycheck
    flycheck-pos-tip
    graphviz-dot-mode
    highlight-parentheses
    highlight-symbol
    (hl-line+ :fetcher wiki)
    hydra
    imenu-list
    importmagic
    ivy
    ivy-xref
    macrostep
    magit
    markdown-mode
    minions
    multiple-cursors
    mwim
    org-plus-contrib
    orgtbl-aggregate
    osx-dictionary
    paredit
    persp-mode
    phi-search
    projectile
    (python :fetcher github :repo "dsedivec/python-el")
    pyvenv
    (smart-tabs :fetcher github :repo "dsedivec/smart-tabs")
    smartparens
    swiper
    transpose-frame
    treepy
    undo-tree
    unicode-fonts
    wgrep
    which-key
    window-purpose
    winum
    zop-to-char
    )
  "List of packages I want installed.  Will be installed in order.")

(defvar my:package-last-refresh 0)

(defvar my:package-max-age-before-refresh 3600)

(define-advice package-refresh-contents
    (:after (&rest args) my:note-last-refresh-time)
  (setq my:package-last-refresh (float-time)))

(defun my:package-refresh-maybe (&optional force-refresh)
  (when (or force-refresh
            (>= (- (float-time) my:package-last-refresh)
                my:package-max-age-before-refresh))
    (package-refresh-contents)))

(defun my:packages-install (&optional pkgs)
  "Install packages listed in `my:packages'.
pkgs is a list of either package names as symbols, or else quelpa
recipes.  Returns list of package names as symbols (even for
quelpa recipes)."
  (interactive)
  (mapcar (lambda (pkg) (let ((pkg-name (if (consp pkg) (car pkg) pkg)))
                          (unless (package-installed-p pkg-name)
                            (if (consp pkg)
                                (quelpa pkg)
                              (my:package-refresh-maybe)
                              (package-install pkg-name)))
                          pkg-name))
          (or pkgs my:packages)))

(defun my:packages-sync (&optional upgrade)
  "Install, (maybe) upgrade, and remove packages according to `my:packages'.

Only upgrade if UPGRADE is true, or invoked with a prefix
argument when called interactively.

Packages not in `my:packages' are removed.  Package removal is
suppressed when running Spacemacs.  Spacemacs probably takes care
of that for us, and I don't want to interfere with it."
  (interactive "P")
  ;; `auto-package-update-now' calls `package-refresh-contents', so we
  ;; call that first and let our advice update
  ;; `my:package-last-refresh', so that following calls to
  ;; `my:package-refresh-maybe' may end up being a no-op.  But if
  ;; `auto-package-update-now' isn't called here then we'll (maybe!)
  ;; need to refresh packages ourselves.
  (when upgrade
    ;; First upgrade quelpa packages.
    (let* ((quelpa-packages (mapcar (lambda (pkg)
                                      (with-demoted-errors
                                          (quelpa pkg :upgrade t))
                                      (car pkg))
                                    (seq-filter #'listp my:packages)))
           ;; Now do a dance to take quelpa packages out of
           ;; `package-activated-list' so that
           ;; `auto-package-upate-now' doesn't bitch about being
           ;; unable to update it.
           (package-activated-list (seq-remove (lambda (pkg)
                                                 (memq pkg quelpa-packages))
                                               package-activated-list)))
      (auto-package-update-now)))
  (let ((pkg-names (my:packages-install)))
    (my:unless-spacemacs
      ;; Is it bad to put names from quelpa recipes in here?  It makes
      ;; package-autoremove work real nice!
      (customize-save-variable 'package-selected-packages pkg-names)
      (package-autoremove))))

;; Don't show the package update buffer if nothing was updated.
(define-advice apu--write-results-buffer
    (:around (orig-fun contents &rest args) my:suppress-empty-results-buffer)
  (when (string-match-p "\n" contents)
    (apply orig-fun contents args)))

;; Delete old versions of packages.
(setq auto-package-update-delete-old-versions t)

(my:packages-sync)


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
  (dolist (hook-func hook-funcs)
    (add-hook hook-var hook-func)))

(defmacro my:with-spacemacs-company-backends-mode-var (mode temp-backends-var
                                                       &rest body)
  "Do something with the Spacemacs company-backends variable for a mode.

I needed this because it seems like Spacemacs has now introduced
more than one variable that holds the backends for a given mode,
and I guess I need to modify them both.  For now.  In the future
I can theoretically just change this macro if/when Spacemacs
changes underneath me, which should be convenient."
  (declare (indent 2))
  (let* ((raw-backends-var (intern (format "company-backends-%S-raw" mode)))
         (backends-var (intern (format "company-backends-%S" mode))))
    `(let ((,temp-backends-var ,raw-backends-var))
       ,@body
       (setq ,raw-backends-var ,temp-backends-var
             ,backends-var ,temp-backends-var))))

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


;;; "Leader" keys setup

(my:unless-spacemacs
  (define-prefix-command 'my:global-leader-map)
  (bind-key "M-m" 'my:global-leader-map))


;;;; Emacs built-ins

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
        ns-icon-type-alist nil))

(my:unless-spacemacs
  ;; Mode line mods

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

  ;; Remove fixed width for position output, causes too much extra space
  ;; after (line,col) in mode line.
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


;;; AUCTeX, RefTeX, and other LaTeX-related stuff

(setq TeX-newline-function 'newline-and-indent)

(setq LaTeX-includegraphics-read-file
      #'LaTeX-includegraphics-read-file-relative)

(setq font-latex-fontify-sectioning 1.3)

(with-eval-after-load 'font-latex
  (font-latex-update-sectioning-faces))

(defun my:LaTeX-insert-combining-acute-accent ()
  (interactive)
  (insert ?\u0301))

(defun my:LaTeX-convert-to-gls (&optional start end num-words)
  "Put region between START and END, or NUM-WORDS forward, in \gls or \glspl."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end) nil)
                 (list nil nil (or current-prefix-arg 1))))
  (assert (or (and start end (null num-words))
              (and (null start) (null end) num-words)))
  (unless (null num-words)
    (save-excursion
      (forward-word num-words)
      (setq end (point))
      (backward-word num-words)
      (setq start (point))))
  (when (string-match (rx "'s" eos) (buffer-substring start end))
    (setq end (- end 2)))
  (atomic-change-group
    (let ((end (copy-marker end))
          (is-plural (string-match (rx "s" (0+ space) eos)
                                   (buffer-substring start end))))
      (goto-char start)
      (insert (let ((case-fold-search nil))
                (cond ((looking-at (rx upper))
                       (prog1
                           (if (looking-at (rx (>= 2 upper)))
                               "\\gls"
                             "\\Gls")
                         (downcase-region start end)))
                      (t "\\gls"))))
      (when is-plural (insert "pl"))
      (insert "{")
      (while (re-search-forward (rx (1+ (any "\n" space))) end t)
        (replace-match "-"))
      (goto-char end)
      (when is-plural (delete-char -1))
      (insert "}")
      (fill-paragraph)
      (undo-boundary))))

(defun my:LaTeX-backward-convert-to-gls (&optional num-words)
  "Put NUM-WORDS before cursor in \gls or \glspl."
  (interactive "p")
  (atomic-change-group
    (backward-word num-words)
    (my:LaTeX-convert-to-gls nil nil num-words)))

;; Teach AUCTeX how to insert glossaries-related commands.
(defun my:LaTeX-arg-glossary-entry (optional &rest args)
  (assert (not optional))
  (insert TeX-grop "name=")
  (insert (read-string (TeX-argument-prompt nil "Entry name" "")))
  (insert ",\ndescription=" TeX-grop)
  (set-marker exit-mark (point))
  (insert TeX-grcl TeX-grcl)
  (LaTeX-indent-line))

(with-eval-after-load 'tex
  (TeX-add-style-hook "glossaries"
                      (lambda ()
                        (TeX-add-symbols
                         '("newacronym" "Key" "Acronym" "Expansion")
                         '("newglossaryentry" "Key" my:LaTeX-arg-glossary-entry)
                         '("gls" "Key")
                         '("glsdisp" "Key" "Text")))))

(with-eval-after-load 'latex
  (bind-keys :map LaTeX-mode-map
             ;; Spacemacs overrides the default LaTeX-insert-item binding
             ;; on M-RET, but we can put it on <M-S-return>.
             ("<M-S-return>" . LaTeX-insert-item)
             ;; I use the combining acute accent a lot when typing up
             ;; Russian coursework.
             ("M-'" . my:LaTeX-insert-combining-acute-accent)
             ;; I don't actually use glossaries that much these days, but
             ;; I'm keeping these utility functions and their bindings
             ;; around for future reference, I guess.
             ("C-c g" . my:LaTeX-convert-to-gls)
             ("C-c M-g" . my:LaTeX-backward-convert-to-gls)))

(defun my:LaTeX-mode-hook ()
  (my:setq-local er/try-expand-list (append er/try-expand-list
                                            '(mark-sentence mark-paragraph))

                 electric-pair-inhibit-predicate
                 #'my:electric-pair-default-plus-before-word-inhibit))

(add-hook 'LaTeX-mode-hook #'my:LaTeX-mode-hook)

(my:add-hooks 'LaTeX-mode-hook
  #'electric-pair-local-mode
  #'flycheck-mode
  #'show-paren-mode
  #'my:warn-white-space-mode)

(my:when-spacemacs
  ;; smartparens drives me nuts, Spacemacs.  Try typing just " twice,
  ;; which generates ``''|'' with cursor at the |.
  (remove-hook 'LaTeX-mode-hook 'smartparens-mode)

  ;; Spacemacs leaves company-dabbrev in its default backend list.
  ;; This makes typing in LaTeX kind of annoying, as it tries to
  ;; complete while I'm writing if I just pause for a bit.
  (my:with-spacemacs-company-backends-mode-var LaTeX-mode backends
    (setq backends (delq 'company-dabbrev backends))))

(my:load-recipe 'auctex-aggressively-load-styles)

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


;;; adaptive-wrap

(defun my:turn-on-adaptive-wrap-prefix-mode ()
  ;; This mode screws up `org-indent-mode'.
  (unless (memq major-mode '(org-mode))
    (adaptive-wrap-prefix-mode 1)))

(add-hook 'text-mode-hook #'my:turn-on-adaptive-wrap-prefix-mode)


;;; all-the-icons

;; These apparently don't get autoloaded.
(autoload 'all-the-icons-alltheicon "all-the-icons")
(autoload 'all-the-icons-fileicon "all-the-icons")


;;; amx

(setq amx-history-length 500)


;;; anaconda-mode

(with-eval-after-load 'anaconda-mode
  ;; anaconda-mode can't navigate around my (messy) code base nearly
  ;; as well as just using tags, so tell it to get off the xref
  ;; bindings.
  (dolist (key '("M-." "M-," "M-*"))
    (unbind-key key anaconda-mode-map)))


;;; autorevert

(global-auto-revert-mode 1)

;; Don't need an echo area message every time a buffer (including
;; "TAGS"!) is reverted.
(setq auto-revert-verbose nil)


;;; avy

(bind-keys ("C-'" . avy-goto-char)
           ("M-g g" . avy-goto-line)
           ("M-g M-g" . avy-goto-line))


;;; bs

(bind-key "C-x C-b" 'bs-show)

(defun my:bs-show-persp-aware (arg)
  "`bs-show' wrapped with `with-persp-buffer-list'."
  (interactive "P")
  (with-persp-buffer-list () (bs-show arg)))

(with-eval-after-load 'persp-mode
  (bind-key "C-x C-b" 'my:bs-show-persp-aware))

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
                 bs-sort-buffer-interns-are-last)))

(setq bs-default-configuration "files-and-dirs")


;;; carousel

(carousel-mode 1)


;;; clean-aindent-mode

(clean-aindent-mode 1)

(bind-keys :map clean-aindent-mode--keymap
           ([remap backward-kill-word] . nil))


;;; comment-dwim-2

(bind-keys ("M-;" . comment-dwim-2))


;;; company

(add-hook 'prog-mode-hook #'company-mode)

(with-eval-after-load 'company
  (bind-keys :map company-mode-map
             ("<C-return>" . company-complete))

  ;; Among a few other things this makes it so TAB completes, not RET,
  ;; which I really need because it's annoying to hit enter at EOL to
  ;; insert a newline and instead a completion slips in instead.
  (company-tng-configure-default)

  ;; C-<digit> to select a completion
  (dotimes (n 10)
    (bind-key (kbd (format "C-%d" n)) 'company-complete-number
              company-active-map)))

(setq company-selection-wrap-around t
      company-show-numbers t
      company-tooltip-align-annotations t
      company-minimum-prefix-length 2
      company-idle-delay 0.2
      ;; Pretty sure I want company-dabbrev-code to search all
      ;; buffers, not even just buffers of the same major mode.
      ;; (Consider, for example, sql-interactive-mode vs. sql-mode.)
      company-dabbrev-code-other-buffers 'all

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
                      company-backends)))

(my:load-recipes 'company-dont-complete-numbers
                 'company-dabbrev-code-defer-prefix
                 'company-remove-dabbrev-code-duplicates)

(cl-defun my:company-splice-backend (new-backend target-backends
                                     &key
                                       (warn-not-found :all)
                                       (backends-var 'company-backends)
                                       (make-backends-local t))
  "Combine NEW-BACKEND together with all TARGET-BACKENDS in BACKENDS-VAR.
TARGET-BACKENDS must be a list.  BACKENDS-VAR defaults to
`company-backends'."
  (let* (found
         changed
         (backends
          (mapcar (lambda (backend)
                    (cond
                      ((member backend target-backends)
                       (cl-pushnew backend found :test #'equal)
                       (setq changed t)
                       (if (listp backend)
                           (append backend (list new-backend))
                         (list backend new-backend)))
                      ((and (listp backend)
                            (memq new-backend backend))
                       (let ((unspliced-backend
                              (remq new-backend backend)))
                         (cond
                           ((and (null (cdr unspliced-backend))
                                 ;; unspliced-backend has only a
                                 ;; single element, so it's probably
                                 ;; in the backends list as just 'foo.
                                 ;; (If it is in there as '(foo) then
                                 ;; we'll get it on the next
                                 ;; condition.
                                 (member (car unspliced-backend)
                                         target-backends))
                            (cl-pushnew (car unspliced-backend) found
                                        :test #'equal))
                           ((member unspliced-backend target-backends)
                            (cl-pushnew unspliced-backend found
                                        :test #'equal))))
                       backend)
                      (t
                       backend)))
                  (symbol-value backends-var))))
    (cl-assert (if changed found t))
    (when changed
      (when make-backends-local
        (make-local-variable backends-var))
      (set backends-var backends))
    (when-let ((missing (and warn-not-found
                             (cond ((not found)
                                    target-backends)
                                   ((eq warn-not-found :all)
                                    (seq-difference target-backends found))))))
      (warn "Could not splice %S into %s backends: %S"
            new-backend (if (not found) "any" "some") missing))))

;; More than one mode hook wants to use this.
(defun my:company-splice-dabbrev-code-into-capf ()
  (my:company-splice-backend 'company-dabbrev-code '(company-capf)))

(add-hook 'prog-mode-hook #'my:company-splice-dabbrev-code-into-capf t)


;;; company-statistics

(with-eval-after-load 'company
  (company-statistics-mode 1))


;;; counsel

(counsel-mode 1)

(defun my:counsel-trace-function-toggle (func-name)
  (let ((func (intern func-name)))
    (if (trace-is-traced 'func)
        (untrace-function func)
      (trace-function func))))

(ivy-add-actions
 'counsel-describe-function
 '(("t" my:counsel-trace-function-toggle "toggle tracing")))

;; Include directory in prompt when searching.
(ivy-set-prompt 'counsel-ag #'counsel-prompt-function-dir)

(bind-keys :map counsel-mode-map
           ("M-m /" . counsel-auto-grep-maybe-projectile)
           ("M-m s f" . counsel-auto-grep-ask-dir))


;;; deft

(setq deft-directory "~/Dropbox/dropsync/Notes"
      deft-recursive t
      deft-default-extension "md"
      deft-use-filename-as-title t
      deft-use-filter-string-for-filename t)

(my:when-spacemacs
  (add-to-list 'spacemacs-useful-buffers-regexp "\\*Deft\\*"))


;;; delsel

(delete-selection-mode 1)


;;; descr-text

(bind-keys ("M-m h d c" . describe-char))


;;; dired-x

;; Binds C-x C-j.  Probably does other stuff I care about.

(require 'dired-x)


;;; dtrt-indent

;; Do this before dtrt-indent gets loaded, hopefully.
(setq dtrt-indent-active-mode-line-info nil)

(dtrt-indent-global-mode 1)

;; If dtrt-indent changed buffer settings like indent-tabs-mode,
;; whitespace-mode may/probably will need to be reset.
(define-advice dtrt-indent-try-set-offset
    (:after (&rest args) my:dtrt-indent-reset-whitespace-mode)
  (when (and (boundp 'whitespace-mode) whitespace-mode)
    (whitespace-mode -1)
    (whitespace-mode 1)))


;;; ediff

(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;;; elec-pair


(defun my:electric-pair-default-plus-before-word-inhibit (char)
  "Default inhibit behavior on CHAR, plus don't pair before a word.
This is because I'm often typing the first character of some pair
like \"(\" with point just before \"foo\" because I am about to
surround \"foo\" with (in this example) parentheses.  I want
\"(foo\" not \"()foo\"."
  (or (electric-pair-default-inhibit char)
      (eq (char-syntax (following-char)) ?w)))


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


;;; etags

(setq tags-revert-without-query t)


;;; expand-region

;; I'm loading this up front because I have several mode hooks that
;; want to frob at `er/try-expand-list', and putting modification of
;; buffer-local variables in an `with-eval-after-load' would
;; be... weird.
(require 'expand-region)

(bind-key "M-@" 'er/expand-region)


;;; faces

(dolist (frame '(t nil))
  (set-face-attribute 'default frame :font "Fira Mono 8"))


;;; files

(setq make-backup-files nil)


;;; find-func

(bind-keys ("M-m j f" . find-function)
           ("M-m j v" . find-variable)
           ("M-m f e l" . find-library))


;;; frame

(bind-key "<s-return>" 'toggle-frame-fullscreen)


;;; flycheck

(setq flycheck-mode-line-prefix "✓"
      ;; flycheck feedback in elisp buffers is not really helpful, and
      ;; far too noisy (though it is occasionally very useful).
      flycheck-global-modes '(not
                              emacs-lisp-mode
                              org-mode)
      ;; Defaults to 400, sadly too few for some of my files at work.
      flycheck-checker-error-threshold 2000)

(global-flycheck-mode 1)

(flycheck-pos-tip-mode 1)

(my:load-recipes 'flycheck-python-pylint-disable-switch)


;;; flyspell

(add-hook 'text-mode-hook #'flyspell-mode)

(add-hook 'prog-mode-hook #'flyspell-prog-mode)


;;; git-commit

(with-eval-after-load 'git-commit
  (add-to-list 'git-commit-style-convention-checks
               'overlong-summary-line))

(setq git-commit-summary-max-length 50)


;;; goto-addr

;; `goto-address-mode' "[buttonizes] URLs and e-mail addresses".

(add-hook 'prog-mode-hook #'goto-address-mode)
(add-hook 'text-mode-hook #'goto-address-mode)


;;; highlight-parentheses

(add-hook 'prog-mode-hook #'highlight-parentheses-mode)


;;; highlight-symbol

(setq highlight-symbol-idle-delay 0.5)


;;; hl-line+

(with-eval-after-load 'hl-line
  (require 'hl-line+)

  ;; This makes hl-line appear above org-mode columns view, which is
  ;; the effect I was going for.  I'm setting this globally for now,
  ;; but this might turn out to be a bad idea, and instead I should
  ;; set it only in org-mode or something like that.
  (setq hl-line-overlay-priority 1))


;;; ielm

(add-hook 'ielm-mode-hook 'paredit-mode)


;;; imenu

(bind-key "M-m j i" 'imenu)

(setq imenu-auto-rescan t
      imenu-auto-rescan-maxout (* 1024 1024 10))


;;; imenu-list

(bind-keys ("M-m b i" . imenu-list-smart-toggle))

(setq imenu-list-auto-resize t)


;;; importmagic

(with-eval-after-load 'importmagic
  (bind-keys :map importmagic-mode-map
             ("M-m m r f" . importmagic-fix-symbol-at-point)))


;;; isearch

(my:load-recipes 'isearch-exit-at-beginning-of-match)

(bind-keys :map isearch-mode-map
           ("C-'" . my:avy-isearch))


;;; ivy

(ivy-mode 1)

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

;; For some reason, ivy can't load flx on load?  Or something?
;; Spacemacs thing?  Not sure.  Workaround:
(when (and (not ivy--flx-featurep)
           (require 'flx nil t))
  (warn "flx is available but ivy couldn't load it?  fixing")
  (setq ivy--flx-featurep t))

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
      ;; Trying this for a while, helps when you have long matches
      ;; from something like counsel-ag.  Better fix might be to show
      ;; the matching portion (somehow), or bind some kind of "scroll
      ;; right"?  Below is fast and easy to try before those more
      ;; involved ideas.
      ivy-truncate-lines nil)

(my:load-recipes 'ivy-special-switch-buffers
                 'ivy-fuzzy-regex-combo-matcher)


;;; ivy-xref

(setq xref-show-xrefs-function #'ivy-xref-show-xrefs
      ivy-xref-use-file-path t)


;;; lisp

;; This is apparently only on ESC <C-backspace> by default?  WTF.
(bind-keys ("<C-M-backspace>" . backward-kill-sexp))


;;; lisp-mode

(add-hook 'lisp-mode-hook #'paredit-mode)

(defun my:lisp-mode-hook ()
  (setq indent-tabs-mode nil))

(add-hook 'lisp-mode-hook #'my:lisp-mode-hook)


;;; magit

(autoload 'magit "magit" nil t)

(setq magit-diff-refine-hunk 'all)


;;; markdown-mode

(with-eval-after-load 'markdown-mode
  (bind-keys :map gfm-mode-map
             ("C-c '" . my:gfm-fcb-edit))

  (my:when-spacemacs
    ;; Spacemacs overrides M-l!  WTH!  I remove this whole set of
    ;; bindings, since I want M-l back, but if I started using the
    ;; others it would be reasonable for my fingers to expect M-l to
    ;; keep working like Spacemacs intended.
    (bind-keys :map markdown-mode-map
               ("M-h" . nil)
               ("M-j" . nil)
               ("M-k" . nil)
               ("M-l" . nil))))

(setq markdown-command "pandoc -f markdown -t html --standalone"
      markdown-header-scaling t)

(my:when-spacemacs
  ;; Spacemacs turns on smartparens-mode, but I don't like it.
  ;; (Combined with show-smartparens-mode it also seems to lock up the
  ;; whole buffer.)
  (remove-hook 'markdown-mode-hook 'smartparens-mode))

(defun my:markdown-mode-hook ()
  (my:setq-local indent-tabs-mode nil)
  (visual-line-mode 1))

(add-hook 'markdown-mode-hook 'my:markdown-mode-hook)

;; I give up, GitHub Flavored Markdown is popular and treats line feed
;; characters like <br>.  Default to visual-line-mode.
(add-hook 'markdown-mode-hook #'visual-line-mode)

(my:load-recipes 'markdown-mode-edit-gfm-code-blocks)


;;; minions

(setq minions-mode-line-lighter "🄼"
      minions-direct '(flycheck-mode persp-mode))

(minions-mode 1)


;;; mwim

(bind-keys ("C-a" . mwim-beginning-of-code-or-line)
           ("C-e" . mwim-end-of-line-or-code))


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

(bind-keys ("C-c r" . org-capture)
           ("M-m a o k i" . org-clock-in-last)
           ("M-m a o k o" . org-clock-out)
           ("M-m a o k g" . org-clock-goto))

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
             ("C-e" . org-end-of-line)
             ;; Somewhere along the lines I think the default binding
             ;; for `org-priority' got superceded by one of my other
             ;; modes/mods/settings (`org-replace-disputed-keys'?).
             ("C-M-," . org-priority))

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

;; More compact clock display in mode line.
(define-advice org-clock-get-clock-string
    (:around (orig-fun) my:compact-clock-in-mode-line)
  (if org-clock-effort
      (funcall orig-fun)
    (let ((clocked-time (org-clock-get-clocked-time)))
      (concat (propertize "⏱" 'face '(:family "Apple Color Emoji"))
              (org-duration-from-minutes clocked-time)))))

;; Force update of clock display in mode line after starting/stopping
;; clock.  Can't just put `force-mode-line-update' into the hooks for
;; some reason?  It doesn't force the addition/removal of the clock
;; to/from the mode line.  Maybe clock not yet "stopped" from the
;; perspective of the mode line drawing functions when that hook is
;; run?

(defun my:org-clock-in-out-update-mode-line ()
  (run-at-time 0 nil #'force-mode-line-update))

(add-hook 'org-clock-in-hook #'my:org-clock-in-out-update-mode-line)
(add-hook 'org-clock-out-hook #'my:org-clock-in-out-update-mode-line)

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

(my:load-recipes 'org-property-drawer-fixes
                 'org-fix-faces-after-goto
                 'org-babel-read-table-in-dblock
                 'org-daily-time-summary
                 'org-columns-delete-property
                 'org-insert-heading-ignore-invisibility)


;;; osx-dictionary

(bind-keys ("C-$" . osx-dictionary-search-pointer))


;;; paredit

(require 'lisp-comment-dwim)

(with-eval-after-load 'paredit
  (bind-keys :map paredit-mode-map
             ("M-;" . lisp-comment-dwim)
             ("M-m j s" . paredit-split-sexp)))


;;; paren

(add-hook 'prog-mode-hook #'show-paren-mode)


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

;; Perspective-aware buffer switching with Ivy, courtesy
;; https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-ivy-el.
;; That Gist is linked from persp-mode.el project. Only a modified subset
;; of it is used here.

(defun my:persp-mode-ivy-filter-buffers (buffer)
  (when-let ((persp (and persp-mode (get-current-persp))))
    (not (persp-contain-buffer-p buffer persp))))

(with-eval-after-load 'ivy
  (add-hook 'ivy-ignore-buffers #'my:persp-mode-ivy-filter-buffers))

(my:load-recipes 'persp-mode-save-load-frame-configuration)


;;; projectile

(projectile-mode 1)

(setq projectile-completion-system 'ivy)

(bind-keys ("C-c C-p" . projectile-command-map)
           ;; Spacemacs bindings, particularly useful when comint
           ;; binds something to C-c C-p.
           ("M-m p" . projectile-command-map))


;;; python

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

  (set (make-local-variable 'company-backends)
       (cons '(company-anaconda company-dabbrev-code) company-backends))

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
            #'my:python-mode-inhibit-electric-indent nil t)

  (when (flycheck-find-checker-executable 'python-pylint)
    (my:setq-local flycheck-checker 'python-pylint
                   ;; Note: flycheck-pylint-disabled-messages is my
                   ;; own creation, see my (use-package flycheck).
                   ;;
                   ;; C0301: Don't warn about long lines, I use
                   ;; whitespace-mode for that.
                   ;;
                   ;; C0330: As of 1.3.1 this check for incorrect
                   ;; hanging and/or continued indentation is
                   ;; totally off, at least in our code base.  See
                   ;; also:
                   ;; https://github.com/PyCQA/pylint/issues/232
                   ;; https://github.com/PyCQA/pylint/issues/289
                   flycheck-pylint-disabled-messages "C0301,C0330")))

(my:add-hooks 'python-mode-hook
  #'my:python-mode-hook
  #'my:warn-white-space-mode
  #'electric-indent-local-mode
  #'electric-pair-local-mode
  #'anaconda-mode
  #'anaconda-eldoc-mode
  #'importmagic-mode
  #'smart-tabs-mode)

(smart-tabs-advise 'python-indent-line 'python-indent-offset)
(smart-tabs-advise 'python-indent-region 'python-indent-offset)
(smart-tabs-advise 'python-indent-shift-left 'python-indent-offset)
(smart-tabs-advise 'python-indent-shift-right 'python-indent-offset)
(smart-tabs-advise 'python-indent-calculate-levels 'python-indent-offset)
(smart-tabs-advise 'python-indent-post-self-insert-function
                   'python-indent-offset)

(my:load-recipes 'python-magic-quotes
                 'python-fix-dead-shell-font-lock-buffer)

(add-hook 'inferior-python-mode-hook #'electric-pair-local-mode)


;;; recentf-mode

(setq recentf-max-saved-items 1000)

(recentf-mode 1)


;;; saveplace

(save-place-mode 1)


;;; scroll-bar-mode

(scroll-bar-mode -1)


;;; server

(server-start)


;;; simple

(column-number-mode 1)

(setq set-mark-command-repeat-pop t
      backward-delete-char-untabify-method nil)


;;; smartparens

;; Need this so ' is configured in lisp modes correctly, so that my
;; lisp-comment-dwim works.
(with-eval-after-load 'smartparens
  (require 'smartparens-config))

;; `sp-split-sexp' is handy even when I'm not using
;; `smartparens-mode', such as when I want to split a string across
;; multiple lines in SQL or Python.  Bindings stolen from Spacemacs.
(bind-keys ("M-m j s" . sp-split-sexp))

;; This one is actually not a Spacemacs binding.  But it should be.
;; If I'm going to bind up `sp-split-sexp', might as well bind its
;; inverse as well.  Useful for basically the same situations
;; mentioned in the comment above, but inverted.
(bind-keys ("M-m j j" . sp-join-sexp))


;;; startup

(setq inhibit-startup-screen t)


;;; sql

(defun my:insert-tab-or-spaces (&optional count)
  (interactive "p")
  (let* ((num-chars (* (or count 1) (if indent-tabs-mode 1 tab-width))))
    (insert-char (if indent-tabs-mode ?\t ?\s) num-chars)))

(cl-defun my:remove-some-indentation (&optional (levels 1))
  "Remove LEVELS of indentation on current line or in region.
Does not move point.  When not operating on a region, will remove
alignment spaces after tabs and treat those spaces as a single
level of indentation."
  (interactive "p")
  (when (> levels 0)
    (let ((backward-delete-char-untabify-method 'untabify))
      (save-excursion
        (cond
          ((use-region-p)
           (let ((end (copy-marker (region-end))))
             (goto-char (region-beginning))
             (cl-loop
                do
                  (back-to-indentation)
                  (when indent-tabs-mode
                    ;; Don't delete spaces after tabs ("alignment
                    ;; spaces").
                    (skip-chars-backward " "))
                  (backward-delete-char-untabify (min (current-column)
                                                      (* tab-width levels)))
                while (and (zerop (forward-line 1)) (< (point) end)))))
          (t
           (back-to-indentation)
           (let ((indent-start (point))
                 (levels levels))
             ;; When operating on a single line instead of a region,
             ;; delete alignment spaces if they exist and count them
             ;; as a single indentation level.
             (when (not (zerop (skip-chars-backward " ")))
               (delete-region (point) indent-start)
               (cl-decf levels))
             (backward-delete-char-untabify (min (current-column)
                                                 (* tab-width levels))))))))))

(with-eval-after-load 'sql
  (bind-keys :map sql-mode-map
             ("TAB" . my:insert-tab-or-spaces)
             ("C-c C-z" . sql-product-interactive)
             ("<backtab>" . my:remove-some-indentation)))

;; Set the default to my most commonly-used RDBMS.
(setq sql-product 'postgres)

(with-eval-after-load 'sql
  ;; With readline on (or libedit on macOS?) things like
  ;; `sql-send-buffer' will sometimes apparently corrupt the data sent
  ;; to psql.
  (add-to-list 'sql-postgres-options "--no-readline")

  ;; PostgreSQL connections should ask for the port.
  (add-to-list 'sql-postgres-login-params 'port t))

(my:when-spacemacs
  (with-eval-after-load 'smartparens
    (sp-local-pair
     '(sql-mode sql-interactive-mode) "(" ")"
     ;; (|), hit RET, should insert a newline
     :post-handlers '(:add
                      (spacemacs/smartparens-pair-newline-and-indent "RET"))
     ;; Don't pair when looking at some SQL, just insert ( please
     :unless '(:add sp-point-before-word-p))))

;; Used by my expand-region setup, see below.
(defun my:sql-mark-statement ()
  (interactive)
  (sql-end-of-statement 1)
  (push-mark nil t t)
  (sql-beginning-of-statement 1))

(my:load-recipes 'indent-match-last-line)

(defun my:sql-mode-hook ()
  (my:setq-local tab-width 4
                 ;; This is necessary starting ca. 24.3.91 because
                 ;; indent-according-to-mode now nukes white space
                 ;; after a blank line, so if you try to put a blank
                 ;; line in the middle of, say, an indented
                 ;; transaction body, you lose your indent.  See git
                 ;; commit 39c6030a.
                 indent-line-function #'my:indent-match-last-line

                 electric-pair-inhibit-predicate
                 #'my:electric-pair-default-plus-before-word-inhibit)
  (my:warn-white-space-mode)
  ;; Spacemacs's sql layer currently doesn't enable completion.
  (my:when-spacemacs
    (if company-backends
        (warn (concat "Looks like Spacemacs started configuring company-mode"
                      " in the SQL layer, update your config"))
      (company-mode 1)
      (setq company-backends spacemacs-default-company-backends)))
  (my:add-to-list-before (make-local-variable 'er/try-expand-list)
                         'my:sql-mark-statement 'er/mark-next-accessor))

(add-hook 'sql-mode-hook #'my:sql-mode-hook)

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

(my:when-spacemacs
  ;; Spacemacs shouldn't try to auto-indent my SQL yanks, Emacs
  ;; doesn't have (my) auto-indentation for SQL.
  (add-to-list 'spacemacs-indent-sensitive-modes 'sql-mode)

  ;; Without this, C-x b won't suggest switching back to *SQL*.  I
  ;; should probably push this upstream.
  (add-to-list 'spacemacs-useful-buffers-regexp "\\*SQL\\*")

  (defun my:sql-interactive-mode-hook ()
    (if (or company-backends company-mode)
        (warn (concat "`company-mode' already on in SQLi buffer,"
                      " did Spacemacs change?"))
      (setq-local company-backends '((company-dabbrev-code :with company-capf)))
      (company-mode 1))
    ;; Spacemacs adds a *fucking lambda* to sql-interactive-mode-hook
    ;; that turns truncate-lines on.  I hate this.  I should push
    ;; upstream to get this as a named function, if not a setting as
    ;; well.
    (if truncate-lines
        (toggle-truncate-lines nil)
      (warn "`truncate-lines' not on in SQLi buffer, did Spacemacs change?")))

  ;; Must append this hook so we turn off truncate-lines after
  ;; Spacemacs turns it on.
  (add-hook 'sql-interactive-mode-hook #'my:sql-interactive-mode-hook t))


;;; swiper

(bind-key "s-s" 'swiper)


;;; tool-bar

(tool-bar-mode -1)


;;; undo-tree

(global-undo-tree-mode)

(bind-keys :map undo-tree-map
           ;; I want to use this for multiple-cursors.
           ("C-?" . nil))


;;; unicode-fonts

(require 'unicode-fonts)

;; I build Emacs NeXTStep with the color fonts patch (for Apple Color
;; Emoji, natch).  Must override unicode-fonts turning said color
;; fonts off.
(when (memq window-system '(ns mac))
  (setq unicode-fonts-skip-font-groups (delq 'multicolor
                                             unicode-fonts-skip-font-groups)))

(unicode-fonts-setup)


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


;;; window-purpose

(purpose-mode 1)

(with-eval-after-load 'ivy
  (require 'ivy-switch-with-purpose))

;; Docs for window-purpose don't really mention users modifying
;; `purpose-action-sequences` to suit their tastes, but Spacemacs is
;; doing it to get the behavior I want: do *not* try to reuse an
;; existing window when C-x b is invoked.  Some day perhaps I should
;; make a PR to document this modification.  (Huh: and this variable.
;; It has no doc string.)
;;
;; An alternative to this would be modifying the advice here which
;; removes force-same-window rather forcibly:
;; https://github.com/bmag/emacs-purpose/blob/a302340e183d20baa4445858d321f43449298829/window-purpose-switch.el#L957-L962
(let ((stb-actions (assq 'switch-to-buffer purpose-action-sequences))
      (action 'purpose-display-maybe-same-window))
  (setcdr stb-actions (cons action (delq action (cdr stb-actions)))))

;; Purpose extensions
;;
;; Note that window-purpose provides an extension for
;; *perspective.el*, not persp-mode.el.  I'm using the latter, so the
;; extension in window-purpose doesn't apply to me.

(require 'window-purpose-x)
(purpose-x-magit-single-on)
;; This gives popwin-like functionality, but better default popup
;; window heights.
(purpose-x-popwin-setup)


;;; winner-mode

(winner-mode 1)


;;; winum

(setq winum-auto-setup-mode-line nil)

(winum-mode 1)

(defvar my:winum-number-string-base #x2780)

(defvar my:winum-number-string-min 1)

(defvar my:winum-number-string-max 10)

(defun my:winum-get-number-string (&optional window)
  (let* ((n (winum-get-number window))
         (s (if (numberp n)
                (concat
                 (if (and (>= n my:winum-number-string-min)
                          (<= n my:winum-number-string-max))
                     (char-to-string (+ my:winum-number-string-base
                                        (- n my:winum-number-string-min)))
                   (int-to-string n))
                 " ")
              "")))
    (propertize s 'face 'winum-face)))

(push '(:eval (my:winum-get-number-string)) (cdr mode-line-format))

(set-face-attribute 'winum-face nil :height 1.2)

(with-eval-after-load 'winum
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
             ("C-9" . winum-select-window-9)))


;;; zop-to-char

(bind-keys ("M-z" . zop-up-to-char))


;;; Epilogue

;; Sometimes init.el doesn't get to load all the way to the end, but
;; errors get swallowed and don't appear in *Messages*?  WTF.
(message "init.el loaded successfully")
