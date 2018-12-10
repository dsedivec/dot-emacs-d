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


;;; Customization

;; Set this early before I potentially install packages, which will
;; modify customizable variable `package-selected-packages'.
(setq custom-file (expand-file-name "customizations.el" user-emacs-directory))
(load custom-file)

(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(load-theme 'dsedivec t)

(my:load-recipes 'custom-format-selected-packages)


;;; package.el with auto-package-update

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives
             '("my:org" . "~/repositories/org-mode/"))
(add-to-list 'package-pinned-packages '(org-plus-contrib . "my:org"))

(package-initialize)

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
    (:after (&rest args) my:note-last-refresh-time)
  (setq my:package-last-refresh (float-time)))

(defun my:package-refresh-maybe (&optional force-refresh)
  (when (or force-refresh
            (>= (- (float-time) my:package-last-refresh)
                my:package-max-age-before-refresh))
    (package-refresh-contents)))

(defvar my:quelpa-packages
  `(
    ;; Until https://github.com/domtronn/all-the-icons.el/pull/106 gets merged:
    ;; all-the-icons
    (all-the-icons :fetcher github :repo "ubolonton/all-the-icons.el"
                   :branch "font-lock-fix" :files (:defaults "data"))
    (bookmark+ :fetcher wiki
               :files ,(cons "bookmark+.el"
                             (mapcar (lambda (suffix)
                                       (format "bookmark+-%s.el" suffix))
                                     '(mac bmu 1 key lit))))
    (eltu :fetcher github :repo "dsedivec/eltu"
          :files (:defaults "eltu_update_tags.py"))
    (hl-line+ :fetcher wiki)
    (ns-copy-html :fetcher git
                  :url ,(concat "file://" (expand-file-name
                                           "~/repositories/ns-copy-html/")))
    (python :fetcher github :repo "dsedivec/python-el")
    (smart-tabs :fetcher github :repo "dsedivec/smart-tabs")
    (sticky-region :fetcher github :repo "dsedivec/sticky-region")
    ))

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

;; When you're truly reluctant to type `(current-buffer)' as that last
;; arg to `buffer-local-value', here's a version that defaults to the
;; current buffer.  It's a generalized variable, too.

(defun my:buffer-local-value (variable &optional buffer)
  "Like `buffer-local-value' but BUFFER defaults to the current buffer."
  (buffer-local-value variable (or buffer (current-buffer))))

(gv-define-setter my:buffer-local-value (val variable &optional buffer)
  `(set (make-local-variable ,variable) ,val))

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

;; Inspired by Spacemacs.

(define-prefix-command 'my:global-leader-map)
(bind-key "M-m" 'my:global-leader-map)


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
        ns-icon-type-alist nil)

  ;; Saving/restoring frames via persp-mode seems to affect this
  ;; somehow.  Setting this seems to put us back where we belong,
  ;; without a friggin' white title bar (WTF).
  (setf (alist-get 'ns-transparent-titlebar default-frame-alist) nil))

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
  (warn "couldn't remove width specification from `mode-line-position'"))


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


;;; el-patch

;; This is out of alphabetical order so it gets installed before
;; anyone potentially starts using el-patch.

(my:load-recipe 'el-patch-clean-up-buffers-after-validation)


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


;;; ace-window

(with-eval-after-load 'ace-window
  (set-face-attribute 'aw-leading-char-face nil :height 10.0))

(bind-key "C-x o" 'ace-window)


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

(setq global-auto-revert-non-file-buffers t)

(global-auto-revert-mode 1)

;; Don't need an echo area message every time a buffer (including
;; "TAGS"!) is reverted.
(setq auto-revert-verbose nil)


;;; avy

(bind-keys ("C-'" . avy-goto-char)
           ("M-g g" . avy-goto-line)
           ("M-g M-g" . avy-goto-line))


;;; bookmark

;; Saving bookmarks after every change (ex. value of "1" here) works
;; poorly, particularly with auto-named bookmarks in bookmark+ where
;; things like *moving to the next/previous bookmark* will end up
;; calling `bmkp-maybe-save-bookmarks' *twice* for each movement (from
;; `bookmark-rename' and later `bmkp-update-autonamed-bookmark'
;; itself).  Therefore we set only to save when exiting Emacs, but
;; also with `auto-save-hook'.
(setq bookmark-save-flag t)

(defun my:bookmark-maybe-auto-save ()
  (when (and bookmark-save-flag
             (> bookmark-alist-modification-count 0))
    (bookmark-save)))

(add-hook 'auto-save-hook #'my:bookmark-maybe-auto-save)


;;; bookmark+

;; Doing this to get the keys loaded, but there might be a better way.
(require 'bookmark+)

(setq bmkp-auto-light-when-set 'all-in-buffer
      bmkp-auto-light-when-jump 'any-bookmark)

(with-eval-after-load 'which-key
  (add-to-list 'which-key-replacement-alist
               '(("C-x p c" . "Prefix Command") . (nil . "create")))
  (add-to-list 'which-key-replacement-alist
               '(("C-x p t" . "Prefix Command") . (nil . "tags"))))

(my:load-recipes 'bookmark+-speed-up)


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

  ;; But now I'm starving for a key that means "complete the selected
  ;; one".
  (bind-keys :map company-active-map
             ("<C-return>" . company-complete-selection))

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
                      company-backends)))

(my:load-recipes 'company-dont-complete-numbers
                 'company-dabbrev-code-work-with-other-prefixes
                 'company-remove-dabbrev-code-duplicates)

(defun my:company-group-existing-backend (backend other-backends
                                          &optional globally no-warn)
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

(defvar my:company-generally-useful-prog-mode-backends
  '(company-dabbrev-code company-files))

;; More than one mode hook wants to use this.
(defun my:company-group-useful-backends-with-capf ()
  (my:company-group-existing-backend
   'company-capf
   my:company-generally-useful-prog-mode-backends))

(add-hook 'prog-mode-hook #'my:company-group-useful-backends-with-capf t)


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


;;; counsel-css

(add-hook 'css-mode-hook #'counsel-css-imenu-setup)


;;; counsel-projectile

(with-eval-after-load 'projectile
  (counsel-projectile-mode 1))


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


;;; dired-narrow

;; I think this is how you expect dired-narrow--regexp-filter to work.
;; I should push this upstream.
;;
;; Use `string-match-p' instead of `re-search-forward', because in the
;; latter case ^ doesn't work like you'd expect since you're never
;; matching the file name at start of line (unless you have a unique
;; dired configuration, perhaps).
;;
;; Catch `invalid-regexp' and just return T in that case, so that
;; nothing is filtered if you have an invalid regexp.
(define-advice dired-narrow--regexp-filter
    (:override (filter) my:better-regexp-filter)
  (condition-case err
      (string-match-p filter (buffer-substring (point) (line-end-position)))
    (invalid-regexp t)))

;; Without removing the :dired-narrow text property as well as the
;; invisibility we set, narrowing with regexp like "{" (where that
;; char doesn't occur in a file name) but then hitting backspace and
;; then just hitting return will actually narrow away *all* files,
;; which is almost certainly not what you want.
;;
;; I should probably push this upstream, but need to test that this
;; isn't something unique to my Emacs config first.
(define-advice dired-narrow--restore
    (:before (&rest args) my:remove-dired-narrow-text-property)
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max) '(:dired-narrow))))


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


;;; dumb-jump

(setq dumb-jump-selector 'ivy)

(bind-keys ("C-M-." . dumb-jump-go))


;;; ediff

(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;;; edit-server

(edit-server-start)

(setq edit-server-new-frame-alist
      '((name . "Edit in Emacs from Browser")))


;;; elec-pair

(add-hook 'prog-mode-hook #'electric-pair-local-mode)

(defun my:electric-pair-default-plus-before-word-inhibit (char)
  "Default inhibit behavior on CHAR, plus don't pair before a word.
This is because I'm often typing the first character of some pair
like \"(\" with point just before \"foo\" because I am about to
surround \"foo\" with (in this example) parentheses.  I want
\"(foo\" not \"()foo\"."
  (or (electric-pair-default-inhibit char)
      (eq (char-syntax (following-char)) ?w)))


;;; elisp-mode

(my:add-hooks 'emacs-lisp-mode-hook
  #'paredit-mode
  #'aggressive-indent-mode
  #'my:warn-white-space-mode)

(defun my:emacs-lisp-mode-hook()
  ;; Make name shorter in mode line.
  (setq mode-name "ELisp")
  (setq indent-tabs-mode nil)
  (setq imenu-generic-expression
        (append imenu-generic-expression
                '(("Sections" "^;;;;?\\s-+\\(.*\\)" 1))))
  ;; XXX
  ;; (add-hook 'completion-at-point-functions
  ;;           #'my:elisp-feature-completion-at-point nil t)
  ;; Trying out case-insensitive dabbrev-code completion in Emacs
  ;; Lisp.  Would have saved me time figuring out why I couldn't
  ;; complete "my:LaTex-" (note lower case "X"--oops).
  (setq-local company-dabbrev-code-ignore-case t))

(add-hook 'emacs-lisp-mode-hook #'my:emacs-lisp-mode-hook)

(bind-keys :map emacs-lisp-mode-map
           ("C-c C-r" . eval-region)
           ("C-c C-b" . eval-buffer)
           ("M-m m d m" . macrostep-expand))

(my:load-recipe 'indent-elisp-like-common-lisp)


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


;;; free-keys

(with-eval-after-load 'free-keys
  ;; Add function keys.
  (setq free-keys-keys (nconc (split-string free-keys-keys "" t)
                              (mapcar (lambda (n) (format "<f%d>" n))
                                      (number-sequence 1 12))))

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
      flycheck-checker-error-threshold 2000)

(global-flycheck-mode 1)

(flycheck-pos-tip-mode 1)

(my:load-recipes 'flycheck-python-pylint-disable-switch)


;;; flycheck-package

(with-eval-after-load 'flycheck
  (flycheck-package-setup))


;;; flyspell

(add-hook 'text-mode-hook #'flyspell-mode)

(add-hook 'prog-mode-hook #'flyspell-prog-mode)


;;; git-commit

(with-eval-after-load 'git-commit
  (add-to-list 'git-commit-style-convention-checks
               'overlong-summary-line))

(setq git-commit-summary-max-length 50)


;;; god-mode

(defun my:get-standard-value (var)
  (eval (car (get var 'standard-value))))

(defun my:revert-to-standard-value (&rest vars)
  (dolist (var vars)
    (set var (my:get-standard-value var))))

;; `global' just makes it easy for me to toggle how I want to manage
;; god-mode, since I vacillate.
(let ((global t))
  (key-chord-mode 1)
  ;; Turn off god-mode in all buffers.  (Turn it on here, then call
  ;; `god-mode-all' which toggles in all buffers including this one.)
  (god-local-mode 1)
  (god-mode-all)
  (key-chord-define-global "fj" (if global 'god-mode-all 'god-local-mode))
  (if global
      (setq god-exempt-major-modes nil
            god-exempt-predicates nil)
    (my:revert-to-standard-value 'god-exempt-major-modes
                                 'god-exempt-predicates)))

(with-eval-after-load 'god-mode
  (bind-keys :map god-local-mode-map ("." . repeat)))

(defface my:god-mode-enabled-mode-line-face '((t (:inherit mode-line
                                                  :background "#060")))
  "Replacement for the mode-line face when god-mode is active in a buffer.")

(defun my:god-mode-enabled-hook ()
  (setq cursor-type 'hbar)
  (set (make-local-variable 'face-remapping-alist)
       (cons '(mode-line . my:god-mode-enabled-mode-line-face)
             face-remapping-alist)))

(add-hook 'god-mode-enabled-hook #'my:god-mode-enabled-hook)

(defun my:god-mode-disabled-hook ()
  (setq cursor-type 'box)
  (setq face-remapping-alist
        (rassq-delete-all 'my:god-mode-enabled-mode-line-face
                          face-remapping-alist)))

(add-hook 'god-mode-disabled-hook #'my:god-mode-disabled-hook)


;;; goto-addr --- Keywords: link url follow open

;; `goto-address-mode' "[buttonizes] URLs and e-mail addresses".
;;
;; I can never remember the name of this fucking mode, nor its
;; bindings.  Its key mapping is done with an overlay, so point must
;; be on a link for e.g. `counsel-descbinds' to even see this binding.

(add-hook 'prog-mode-hook #'goto-address-mode)
(add-hook 'text-mode-hook #'goto-address-mode)


;;; help

;; Select help windows always.
(setq help-window-select t)


;;; highlight-indent-guides

(setq highlight-indent-guides-method 'character)


;;; highlight-parentheses

(add-hook 'prog-mode-hook #'highlight-parentheses-mode)


;;; highlight-symbol

(setq highlight-symbol-idle-delay 0.5)

(add-hook 'prog-mode-hook #'highlight-symbol-mode)


;;; hippie-exp

(bind-key "M-/" 'hippie-expand)


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

(setf (cdr (assoc "XXX" hl-todo-keyword-faces))
      '(:inherit hl-todo :foreground "yellow" :background "red"))


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

;; https://gist.githubusercontent.com/anachronic/7af88c62db136727cd1fed17ee0a662f/raw/a2839e7989297293621789d669594d7820e1a52e/init-javascript.el

(defhydra js2-refactor-hydra (:color blue :hint nil)
  ;; Must escape "[" in column zero.  See
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Left-Margin-Paren.html.
  "
^Functions^                    ^Variables^               ^Buffer^                      ^sexp^               ^Debugging^
------------------------------------------------------------------------------------------------------------------------------
\[_lp_] Localize Parameter      [_ev_] Extract variable   [_wi_] Wrap buffer in IIFE    [_k_]  js2 kill      [_lt_] log this
\[_ef_] Extract function        [_iv_] Inline variable    [_ig_] Inject global in IIFE  [_ss_] split string  [_dt_] debug this
\[_ip_] Introduce parameter     [_rv_] Rename variable    [_ee_] Expand node at point   [_sl_] forward slurp
\[_em_] Extract method          [_vt_] Var to this        [_cc_] Contract node at point [_ba_] forward barf
\[_ao_] Arguments to object     [_sv_] Split var decl.    [_uw_] unwrap
\[_tf_] Toggle fun exp and decl [_ag_] Add var to globals
\[_ta_] Toggle fun expr and =>  [_ti_] Ternary to if
\[_q_]  quit"
  ("ee" js2r-expand-node-at-point)
  ("cc" js2r-contract-node-at-point)
  ("ef" js2r-extract-function)
  ("em" js2r-extract-method)
  ("tf" js2r-toggle-function-expression-and-declaration)
  ("ta" js2r-toggle-arrow-function-and-expression)
  ("ip" js2r-introduce-parameter)
  ("lp" js2r-localize-parameter)
  ("wi" js2r-wrap-buffer-in-iife)
  ("ig" js2r-inject-global-in-iife)
  ("ag" js2r-add-to-globals-annotation)
  ("ev" js2r-extract-var)
  ("iv" js2r-inline-var)
  ("rv" js2r-rename-var)
  ("vt" js2r-var-to-this)
  ("ao" js2r-arguments-to-object)
  ("ti" js2r-ternary-to-if)
  ("sv" js2r-split-var-declaration)
  ("ss" js2r-split-string)
  ("uw" js2r-unwrap)
  ("lt" js2r-log-this)
  ("dt" js2r-debug-this)
  ("sl" js2r-forward-slurp)
  ("ba" js2r-forward-barf)
  ("k" js2r-kill)
  ("q" nil))

(with-eval-after-load 'js2-refactor
  (js2r-add-keybindings-with-prefix "M-m m r")

  (bind-keys :map js2-refactor-mode-map
             ("M-m m r SPC" . js2-refactor-hydra/body)))


;;; link-hint

(autoload 'link-hint--get-link-at-point "link-hint")

(defun my:link-hint-multi-dispatch (copy-link)
  "Open or copy links using link-hint.
If there is a link at point, `link-hint-open-link-at-point' is
used.  Otherwise, `link-hint-open-link' is called.  If COPY-LINK
is true, or if used interactively with a prefix argument, copy
the selected link instead of opening it."
  (interactive "P")
  (if (link-hint--get-link-at-point)
      (link-hint--action-at-point (if copy-link :copy :open))
    (call-interactively (if copy-link
                            #'link-hint-copy-link
                          #'link-hint-open-link))))

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

(add-hook 'lisp-mode-hook #'paredit-mode)

(defun my:lisp-mode-hook ()
  (setq indent-tabs-mode nil))

(add-hook 'lisp-mode-hook #'my:lisp-mode-hook)


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

;; I want this everywhere (recommended by the Magit manual's "Getting
;; Started" section).
(bind-key "C-x g" 'magit-status)

(autoload 'magit "magit" nil t)

(setq magit-diff-refine-hunk 'all)

;; truncate-lines does not work well for me when viewing diffs and such.

(defun my:magit-mode-hook ()
  (setq truncate-lines nil))

(add-hook 'magit-mode-hook #'my:magit-mode-hook)

(which-key-add-major-mode-key-based-replacements 'magit-status-mode
    "j p" "unpushed"
    "j f" "unpulled")


;;; markdown-mode

(with-eval-after-load 'markdown-mode
  (bind-keys :map gfm-mode-map
             ("C-c '" . my:gfm-fcb-edit)))

(setq markdown-command "pandoc -f markdown -t html --standalone"
      markdown-header-scaling t)

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
      minions-direct '(flycheck-mode persp-mode multiple-cursors-mode))

(minions-mode 1)


;;; move-text

(move-text-default-bindings)


;;; multi-line

(bind-key "C-c d" 'multi-line)


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

;; Put the name of the clocked item in the tooltip (help-echo
;; property).  Note that this doesn't take `org-clock-string-limit'
;; into account, which is a bit of a deficiency, though looking at
;; `org-clock-update-mode-line' source I will say that your tooltip
;; will look stupi dif `org-clock-string-limit' kicks in, so I don't
;; feel that bad about ignoring it.
(define-advice org-clock-update-mode-line
    (:after (&rest args) my:add-task-name-to-clock-help-echo)
  (let* ((start-idx (if (and org-clock-task-overrun-text
                             (string-prefix-p org-clock-task-overrun-text
                                              org-mode-line-string))
                        (length org-clock-task-overrun-text)
                      0))
         (old-help-echo (get-text-property start-idx 'help-echo
                                           org-mode-line-string)))
    (put-text-property start-idx (- (length org-mode-line-string) start-idx)
                       'help-echo (concat "Task: " org-clock-heading "\n"
                                          old-help-echo)
                       org-mode-line-string)))

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

(with-eval-after-load 'window-purpose-x
  (add-to-list 'purpose-x-popwin-major-modes 'osx-dictionary-mode)
  (purpose-x-popwin-update-conf))


;;; package-build

(setq package-build-recipes-dir "~/repositories/melpa/recipes")


;;; paredit

(with-eval-after-load 'paredit
  (bind-keys :map paredit-mode-map
             ("M-m j s" . paredit-split-sexp)))

(my:load-recipes 'paredit-delsel
                 'paredit-kill-whole-line)


;;; paren

(add-hook 'prog-mode-hook #'show-paren-mode)


;;; persp-mode

;; Must set this before turning on persp-mode for it to have an effect
;; at startup.
(setq persp-auto-resume-time 0.1)

(when (bound-and-true-p persp-mode)
  (warn "Set `persp-auto-resume-time' too late, persp-mode already loaded"))

(setq persp-add-buffer-on-after-change-major-mode 'free
      ;; C-x 5 2 shouldn't copy e.g. window layout.  I hope this still
      ;; means additional frames will be saved with the perspective!
      ;; (If not, perhaps see `persp-ignore-wconf-once' here?)
      persp-init-new-frame-behaviour-override nil)

;; Don't attempt to reactivate persp-mode if it's already active
;; (Spacemacs will have it enabled, I believe).  Doing so does weird
;; things.
(unless persp-mode
  (persp-mode 1))

(bind-keys :map persp-mode-map
           ("M-m l l" . persp-frame-switch))

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

(unless (memq #'my:persp-mode-dont-save-buffers-without-files
              persp-save-buffer-functions)
  (let ((last-cell (last persp-save-buffer-functions)))
    (setf (cdr last-cell) (list (car last-cell))
          (car last-cell) #'my:persp-mode-dont-save-buffers-without-files)))


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


;;; pyvenv

(exec-path-from-shell-copy-envs '("PYTHONPATH"  "WORKON_HOME"))


;;; recentf-mode

(setq recentf-max-saved-items 1000)

(recentf-mode 1)


;;; rect

(my:load-recipes 'emacs-yank-rectangle-to-new-lines)

;;; saveplace

(save-place-mode 1)


;;; scroll-bar-mode

(scroll-bar-mode -1)


;;; server

(server-start)


;;; sh-script

(defun my:sh-mode-hook ()
  (my:setq-local indent-tabs-mode t
                 tab-width 4)
  (setq-local company-backends (cons '(company-shell
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


;;; simple

(column-number-mode 1)

(setq set-mark-command-repeat-pop t
      backward-delete-char-untabify-method nil)

;; My Moom (macOS) configuration will maximize window on M-=, the
;; default binding for `count-words-region'.  Put it elsewhere so I
;; can use it.
(bind-key "C-M-=" 'count-words-region)


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

(setq inhibit-startup-screen t
      user-mail-address "dale@codefu.org")


;;; sticky-region

(sticky-region-mode 1)


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

(my:add-hooks 'sql-interactive-mode-hook
  ;; Reverse order to make sure company gets loaded first.
  #'my:company-group-useful-backends-with-capf
  #'company-mode)

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


;;; swiper

(bind-key "s-s" 'swiper)


;;; tool-bar

(tool-bar-mode -1)


;;; undo-tree

(global-undo-tree-mode)

(bind-keys :map undo-tree-map
           ;; I want to use this for multiple-cursors.
           ("C-?" . nil))

;; I never expect to be able to undo in a region, but this is on by
;; default.  Turn it off.  Maybe someday I'll turn this back on (AKA
;; leave it on) if I find a good use for it.
(setq undo-tree-enable-undo-in-region nil)


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

(my:load-recipes 'vc-use-icons-in-mode-line
                 'vc-truncate-long-branch-names)


;;; volatile-highlights

(volatile-highlights-mode 1)


;;; web-mode

(setq web-mode-enable-auto-expanding t
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
  #'my:warn-white-space-mode)

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

(with-eval-after-load 'web-mode
  (bind-keys :map web-mode-map
             ("C-M-u" . web-mode-element-parent)
             ("C-M-d" . web-mode-element-child)
             ("C-M-n" . web-mode-element-end)))

(which-key-add-major-mode-key-based-replacements 'web-mode
    "C-c C-a" "attribute"
    "C-c C-b" "block"
    "C-c C-d" "dom"
    "C-c C-e" "element"
    "C-c C-t" "tag")


;;; which-func

(add-hook 'prog-mode-hook #'which-function-mode)

;; Move `which-function-mode' out of the mode line and into the header
;; line.
;;
;; We move it out of the mode line globally, but into the header line
;; only via a hook in `which-function-mode-hook'.  If you put it in
;; `header-line-format' globally, you'll have an empty, useless header
;; line in buffers where you're not using `which-function-mode'.

(defvar my:which-function-mode-line-spec nil)

(with-eval-after-load 'which-func
  (setq my:which-function-mode-line-spec
        (my:treepy-edit-mode-line-var (mode-line-misc-info zip)
          (eq (car-safe (treepy-node zip)) 'which-function-mode)
          (treepy-remove zip)))
  (unless my:which-function-mode-line-spec
    (warn "Could not find `which-function-mode' in `mode-line-misc-info'")))

(defun my:which-function-move-into-header ()
  (when my:which-function-mode-line-spec
    (make-local-variable 'header-line-format)
    (my:treepy-edit-mode-line-var
        (header-line-format zip)
      (equal (treepy-node zip) my:which-function-mode-line-spec)
      (treepy-remove zip))
    (push my:which-function-mode-line-spec header-line-format)))

(add-hook 'which-function-mode-hook #'my:which-function-move-into-header)


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

(if (get 'purpose-set-extension-configuration 'lisp-indent-function)
    (warn (concat "`purpose-set-extension-configuration'"
                  " now sets `lisp-indent-function'"))
  (put 'purpose-set-extension-configuration 'lisp-indent-function 1))


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


;;; xref

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
