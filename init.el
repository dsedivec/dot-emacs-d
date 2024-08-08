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
  ;; the one from Homebrew that might know about libgccjit.  Note that
  ;; this will later be overwritten by my use of exec-path-from-shell.
  (setenv "PATH" (concat "/opt/homebrew/bin:/usr/local/bin:"
                         (or (getenv "PATH") "/bin:/usr/bin")))

  ;; Don't pop up *Warnings* for native-comp warnings because the
  ;; async native-comp warnings are far too numerous.
  (with-eval-after-load 'warnings
    (add-to-list 'warning-suppress-types '(comp))))

(require 'cl-lib)
(require 'subr-x)
(require 'seq)

;; Load local init.el stuff early, since it might e.g. set up SSL
;; settings or something.
(load (expand-file-name "init-local" user-emacs-directory) t)


;;; Local packages

(defvar my:local-packages-dir (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'load-path my:local-packages-dir)

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


;;; Set custom-file

;; Set this early before anything in customize can haul off and do
;; stupid shit I will later find disagreeable.
(setq custom-file (expand-file-name "customizations.el" user-emacs-directory))
;; I can't remember why I have to do this myself.  I think it's just
;; the way things are meant to work.
(load custom-file)


;;; straight.el, for all your packaging needs

(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; I'm going to get auto-compile very early and turn it on, in hopes
;; that it'll auto-compile anything that needs it.  This is probably
;; pointless, at least for packages installed by straight.el, but it
;; should also be harmless.  I suspect nothing will need to get
;; compiled until I start doing some `my:load-recipes' later on.
(straight-use-package 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; I'm (possibly) using emacs-lsp-booster and lsp-mode.  To use
;; emacs-lsp-booster, I need lsp-mode to be compiled with plist
;; support, apparently.  That means getting some of these variables
;; set early.
;;
;; Make sure that I have my (probably) XDG user bin dir on `exec-path'
;; so that I can find emacs-lsp-booster.
(let ((xdg-user-bin (expand-file-name "~/.local/bin")))
  (unless (seq-some (apply-partially #'string-match-p
                                     (rx string-start
                                         (literal xdg-user-bin)
                                         (? ?/)
                                         string-end))
                    exec-path)
    (push xdg-user-bin exec-path)))
(defvar my:use-emacs-lsp-booster (executable-find "emacs-lsp-booster"))
(when my:use-emacs-lsp-booster
  (setenv "LSP_USE_PLISTS" "true")
  (setq lsp-use-plists t))

;; As of Emacs 485622bbd1a you/I need AUCTeX > 13.0.5, or else you
;; get errors when reftex tries to create labels.
;;
;; On macOS, MacTeX might not be on my path by the time you get here.
;; The AUCTeX build process wants to use pdftex, so let's temporarily
;; put it on the path.

(defvar my:texbin-dir "/Library/TeX/texbin")

(defvar my:tex-available-p (file-directory-p my:texbin-dir))

;; AUCTeX will not build without LaTeX.
(when my:tex-available-p
  (let ((process-environment process-environment))
    (setenv "PATH" (concat (or (getenv "PATH") "") ":" my:texbin-dir))
    ;; The el-get recipe won't work for AUCTeX because (I assume) el-get
    ;; runs Elisp right out of the repo it clones to, which contains
    ;; support files (ex. styles/*.el) that won't be there if you use
    ;; that same recipe with straight.el.  Hence we make our own recipe.
    (straight-use-package '(auctex :source el-get
                            :files ("*.el" "*.info" "dir"
                                    "doc" "etc" "images" "latex" "style")))
    ;; See the :load bits of
    ;; https://github.com/dimitri/el-get/blob/master/recipes/auctex.rcp,
    ;; which are not supported by straight.el as of this writing.  Without
    ;; these you will get built-in Emacs LaTeX modes, not AUCTeX.
    (require 'tex-site)
    (require 'preview-latex)))

;; Let's install some packages.

(straight-use-package '(org :repo "git@github-personal:dsedivec/org-mode.git"))
(straight-use-package 'org-contrib)

(dolist (pkg-def '(
                   (eltu :files (:defaults "eltu_update_tags.py"))
                   ns-copy-html
                   smart-tabs
                   sticky-region
                   )
         )
  (straight-use-package (nconc (if (consp pkg-def)
                                   (copy-tree pkg-def)
                                 (list pkg-def))
                               (list :host 'github
                                     :repo (format "dsedivec/%s"
                                                   (if (consp pkg-def)
                                                       (car pkg-def)
                                                     pkg-def))))))

(defun my:straight-use-packages (packages)
  (mapc #'straight-use-package packages))

(my:straight-use-packages '(
                            ace-window
                            adaptive-wrap
                            aggressive-indent
                            all-the-icons
                            all-the-icons-completion
                            anaconda-mode
                            apheleia
                            atomic-chrome
                            auto-highlight-symbol
                            auto-yasnippet
                            avy
                            bind-key
                            blackout
                            bm
                            buttercup
                            cider
                            clean-aindent-mode
                            clj-refactor
                            command-log-mode
                            comment-dwim-2
                            company
                            company-anaconda
                            company-prescient
                            company-shell
                            company-terraform
                            company-web
                            crontab-mode
                            crux
                            csv-mode
                            ctrlf
                            dash
                            deft
                            diff-hl
                            dired-narrow
                            dired-ranger
                            dockerfile-mode
                            dotenv-mode
                            dtrt-indent
                            dumb-jump
                            edit-indirect
                            editorconfig
                            el-patch
                            embrace
                            emmet-mode
                            envrc
                            eterm-256color
                            exec-path-from-shell
                            expand-region
                            fennel-mode
                            ;; Need this recipe to get the dynamic
                            ;; module in the "bin" directory.
                            (flx-rs
                             :repo "jcs-elpa/flx-rs"
                             :fetcher github
                             :files (:defaults "bin"))
                            flycheck
                            flycheck-clj-kondo
                            flycheck-haskell
                            flycheck-package
                            flycheck-pos-tip
                            free-keys
                            fussy
                            git-link
                            go-mode
                            graphviz-dot-mode
                            groovy-mode
                            haskell-snippets
                            hcl-mode
                            highlight-indent-guides
                            highlight-indentation
                            highlight-parentheses
                            hindent
                            hl-todo
                            hlint-refactor
                            htmlize
                            hydra
                            imenu-list
                            impatient-mode
                            js2-mode
                            js2-refactor
                            json-mode
                            key-chord
                            link-hint
                            literate-calc-mode
                            loccur
                            lorem-ipsum
                            lsp-mssql
                            lsp-mode
                            lsp-pyright
                            lsp-treemacs
                            lsp-ui
                            lua-mode
                            macrostep
                            magit
                            markdown-mode
                            minions
                            modus-themes
                            monroe
                            move-text
                            multiple-cursors
                            mwim
                            nhexl-mode
                            obsidian
                            olivetti
                            org-download
                            (org-roam :branch "main")
                            orgtbl-aggregate
                            osx-dictionary
                            pandoc-mode
                            paredit
                            phi-search
                            prescient
                            projectile
                            python
                            pyvenv
                            rainbow-mode
                            reformatter
                            rg
                            rustic
                            scss-mode
                            shackle
                            shift-number
                            sly
                            smartparens
                            sql-indent
                            sqlup-mode
                            string-inflection
                            systemd
                            terraform-doc
                            terraform-mode
                            transpose-frame
                            treemacs
                            treemacs-projectile
                            treepy
                            treesit-auto
                            typescript-mode
                            undo-tree
                            unfill
                            vcl-mode
                            volatile-highlights
                            vterm
                            web-beautify
                            web-mode
                            ;; Seems like these folks have changed the
                            ;; default branch from "master" to "main",
                            ;; but maybe the MELPA recipe didn't get
                            ;; updated?
                            (webpaste :branch "main")
                            wgrep
                            which-key
                            winum
                            yaml-mode
                            yasnippet
                            yasnippet-snippets
                            zop-to-char
                            ))

(when my:tex-available-p
  ;; These are guarded so that straight doesn't try to pull in AUCTeX
  ;; against my will.
  (my:straight-use-packages '(
                              company-reftex
                              )))


;; Completion framework

(defvar my:completion-framework 'vertico)
(cl-assert (memq my:completion-framework '(ivy vertico)))

(cl-ecase my:completion-framework
  (ivy
   (my:straight-use-packages '(
                               amx
                               counsel
                               counsel-css
                               counsel-projectile
                               flx
                               ivy
                               ivy-avy
                               ivy-hydra
                               ivy-prescient
                               ivy-xref
                               ivy-yasnippet
                               lsp-ivy
                               swiper)
                             )
   (when my:tex-available-p
     (my:straight-use-packages '(ivy-bibtex))))

  (vertico
   (my:straight-use-packages '(
                               consult
                               consult-flycheck
                               consult-lsp
                               embark
                               embark-consult
                               marginalia
                               orderless
                               vertico
                               ))))


;;; Utility functions

(require 'dsedivec-utils)


;;;; Emacs built-ins

;; My nav-stack package uses circular lists, try not to stack overflow
;; when I accidentally end up printing one during debugging.  (Why is
;; this nil by default, anyway?)
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
  (clear-face-cache)

  ;; Per NEWS.28, this should get me Emoji support?  Please?
  (set-fontset-font t 'emoji '("Apple Color Emoji" . "iso10646-1") nil 'prepend)
  ;; Per people (rightfully?) bitching in
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=54970, it seems I
  ;; also need to do this for the "symbol script".  (You can see this
  ;; in `describe-char' output.)
  (set-fontset-font t 'symbol '("Apple Color Emoji" . "iso10646-1") nil 'prepend)
  ;; I added these two when I found the SQUARED LATIN CAPITAL LETTER M
  ;; (U+1F13C) (🄼) was no longer displaying, maybe after I upgraded to
  ;; Ventura.  Apple Symbols has it, but I *think* I prefer the SF Pro
  ;; version of it.
  (set-fontset-font t 'symbol '("Apple Symbols" . "iso10646-1") nil 'prepend)
  (set-fontset-font t 'symbol '("SF Pro" . "iso10646-1") nil 'prepend)

  ;; As of 9370a4763aac, `ns-popup-font-panel' is apparently gone.
  ;; The menu bar has its own `menu-set-font' that I'm going to crib
  ;; from.
  (unless (commandp 'ns-popup-font-panel)
    (autoload 'menu-set-font "menu-bar")
    (bind-key "s-t" 'menu-set-font))

  (my:load-recipes 'ns-paste-as-org))

;; macOS trashing: inspired first by
;; https://github.com/emacsorphanage/osx-trash, but then by
;; https://gist.github.com/dabrahams/14fedc316441c350b382528ea64bc09c
;; (from https://apple.stackexchange.com/a/162354).
;; `ns-do-applescript' seems fast enough to me.

(defun my:ns-move-files-to-trash (&rest paths)
  (let ((as-paths
         (mapconcat
          (lambda (path)
            (format "the POSIX file \"%s\"" (replace-regexp-in-string
                                             (rx (group (any ?\\ ?\")))
                                             "\\\\\\1"
                                             (expand-file-name path))))
          paths
          ", ")))
    (ns-do-applescript
     (format "tell application \"Finder\" to move {%s} to trash" as-paths))))

(when (eq system-type 'darwin)
  (let ((emacs-29 (>= emacs-major-version 29))
        (trash-defined (fboundp 'system-move-file-to-trash)))
    (if trash-defined
        (if emacs-29
            (message "Using Emacs 29 built-in trash rather than AppleScript")
          (warn (concat "`system-move-file-to-trash' shouldn't be defined,"
                        " check your init.el")))
      (when emacs-29
        (warn (concat "Using AppleScript trash support, but it should be"
                      " built-in for Emacs 29; maybe build newer Emacs?")))
      (defalias 'system-move-file-to-trash #'my:ns-move-files-to-trash))))

(setq delete-by-moving-to-trash t)


;;; el-patch

;; This package is way, way early because I'm about to use it to patch
;; `treepy-remove', and I needd my treepy-remove-fix recipe.  In
;; general, it needs to be early because I use this a decent number of
;; times throughout my init.el.

;; Without this, `el-patch-defvar' rarely does anything for me.
(setq el-patch-use-aggressive-defvar t)

;; Setting this early, since after I switched to straight, something
;; is hitting this all the time.  I strongly suspect it's el-patch.
(setq vc-follow-symlinks t)

(my:load-recipe 'el-patch-dont-kill-my-buffers)


;;; Mode line mods

;; (Couldn't require this at top, has to come after packages are
;; installed.)
(require 'dash)

;; Fix for https://github.com/volrath/treepy.el/issues/9, necessary
;; for treepy-remove to work right.
(my:load-recipes 'treepy-remove-fix)

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
           (`(:propertize ("" mode-line-percent-position) . ,_) t))
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


;;; Themes

(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(load-theme 'dsedivec t)

(defun my:set-theme-for-macos-system-theme (&optional toggle force)
  (interactive "P")
  (let* ((scpt (concat "tell application \"System Events\" to"
                       " get the dark mode of appearance preferences"
                       " as integer"))
         ;; `cl-equalp' does case insensitive string comparison.
         ;; "White" is the default background color on Emacs/macOS.
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
    (cl-assert (memq target-theme '(light dark)))
    (if (and (not force) (eq emacs-theme target-theme))
        (message "Emacs already configured for %s theme, no changes." target-theme)
      (message "Configuring Emacs for %s theme." target-theme)
      (modify-all-frames-parameters `((ns-appearance . ,target-theme)))
      (cl-ecase target-theme
        (light
         (disable-theme 'modus-vivendi)
         (load-theme 'dsedivec t))
        (dark
         (disable-theme 'dsedivec)
         (load-theme 'modus-vivendi t)))
      ;; Function `org-mode' sets up face org-hide based on the
      ;; current background color.  Changing the background color thus
      ;; requires restarting org-mode.  I think I can do this in just
      ;; one buffer and it'll change the org-hide face globally.
      (when-let ((org-buf (seq-some (lambda (buf)
                                      (with-current-buffer buf
                                        (when (derived-mode-p 'org-mode)
                                          buf)))
                                    (buffer-list))))
        (with-current-buffer org-buf
          (org-mode-restart))))))

;; Forcibly set the correct theme whenever starting Emacs.  Forcing is
;; important because desktop.el restores my theme, with all its
;; parameters, including its background color, so the code in
;; `my:set-theme-for-macos-system-theme' thinks the theme is set.  But
;; it's not, and you end up with the first frame looking half-right,
;; successive frames looking unthemed.
(add-hook 'after-init-hook (lambda () (my:set-theme-for-macos-system-theme nil t)))


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
  (exec-path-from-shell-initialize)

  ;; Need to pick up SSH_ASKPASS from shell on Darwin, which may be set
  ;; to the path to my own ssh-askpass clone that uses pinentry since we
  ;; have no good ssh-askpass on macOS AFAIK.
  (exec-path-from-shell-copy-envs '("SSH_ASKPASS")))



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
  ;; This auto-insert skeleton only loaded after 'latex because it
  ;; (ab)uses `LaTeX-arg-usepackage-read-packages-with-options'.
  (with-eval-after-load 'autoinsert
    (setf (alist-get "\\.[Ss][Tt][Yy]\\'" auto-insert-alist nil nil #'equal)
          '(nil
            "\\NeedsTeXFormat{LaTeX2e}[1994/06/01]\n"
            "\\ProvidesPackage{"
            (if buffer-file-name
                (file-name-base buffer-file-name)
              (read-string "Package name: "))
            "}\n"
            ;; AUCTeX doesn't help us indent here...
            ;; >
            ;; ...so we'll just insert our own indent.
            "  ["
            (format-time-string "%Y/%m/%d")
            " v001 "
            (read-string "This package's description: ")
            "]\n\n"
            (cl-loop
              for (packages . options)
               = (LaTeX-arg-usepackage-read-packages-with-options)
              while packages
              do (progn
                   (insert "\\RequirePackage")
                   (LaTeX-arg-usepackage-insert packages options)
                   (insert "\n")))
            & "\n"
            -
            "\n\n\\endinput\n")))

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


;;; all-the-icons-completion

(all-the-icons-completion-mode 1)

(when (eq my:completion-framework 'vertico)
  ;; README recommends setting this so that all-the-icons-completion
  ;; is "on when `marginalia-mode' is on and is off when it's off".
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))


;;; amx

(when (eq my:completion-framework 'ivy)
  (setq amx-history-length 500)

  ;; XXX bug fix?

  (defun my:amx-post-eval-force-update-improved (orig-fun fundef &rest args)
    "Schedule an amx update the next time Emacs is idle.

But this time, don't keep invalidating the hook every time
`ctrlf-local-mode' runs in the echo area due to eldoc, and ctrlf
uses a lambda, and for some reason that triggers some weird
`autoload-do-load' call, which ends up invalidating amx's cache
every time eldoc runs, and you get weird pauses that eat C-g
basically every time eldoc's idle hook runs.  Fuck me."
    (let ((result (apply orig-fun fundef args)))
      ;; Read the source for `autoload-do-load': there are various cases
      ;; where it returns FUNDEF, all of which look like no-ops.  If we
      ;; did a no-op, then don't trigger amx to rebuild its cache (which
      ;; is quite expensive).
      (unless (eq result fundef)
        (condition-case-unless-debug err
            (amx-post-eval-force-update)
          (error
           (warn "`my:amx-post-eval-force-update-improved' ignoring error: %S" err))))
      result))

  (with-eval-after-load 'amx
    (condition-case-unless-debug err
        (advice-remove 'autoload-do-load #'amx-post-eval-force-update)
      (error
       (warn "Ignoring error removing amx advice from `autoload-do-load': %S" err))
      (:success
       (advice-add 'autoload-do-load :around #'my:amx-post-eval-force-update-improved)))))


;;; anaconda-mode

(defun my:anaconda-mode-turn-on ()
  (anaconda-mode 1)
  (anaconda-eldoc-mode 1)
  (my:company-group-existing-backend 'company-capf '(company-anaconda)))

(defun my:use-anaconda-mode-if-no-lsp ()
  (add-hook 'my:use-lsp-no-lsp-hook #'my:anaconda-mode-turn-on nil t))

(add-hook 'python-base-mode-hook #'my:use-anaconda-mode-if-no-lsp)

(with-eval-after-load 'anaconda-mode
  (bind-keys :map anaconda-mode-map
             ("M-," . nil)))


;;; apheleia

(with-eval-after-load 'apheleia
  (let ((have-isort (executable-find "isort"))
        (have-darker (executable-find "darker"))
        (python-modes '(python-mode python-ts-mode)))
    (cond
      (have-darker
       ;; To my surprise, Darker works just fine on files outside any
       ;; Git repository.  I assume it functions as Black in the
       ;; absence of a Git repo.
       ;;
       ;; Need Darker >= 1.4.0 to remove stray EOL in --stdout mode.
       (setf (alist-get 'darker apheleia-formatters)
             (remq nil (list "darker"
                             (when have-isort "--isort")
                             "--quiet" "--stdout" 'file)))
       (dolist (mode python-modes)
         (setf (alist-get mode apheleia-mode-alist) 'darker))
       (message "apheleia set up to use darker for Python."))
      (have-isort
       (dolist (mode python-modes)
         (let* ((cell (assq mode apheleia-mode-alist))
                (current-checkers (cdr cell)))
           (if (or (eq current-checkers 'isort)
                   (and (listp current-checkers) (memq 'isort current-checkers)))
               (warn (concat "Apheleia has already set `isort' as a `%S'"
                             " formatter?  Fix your init.el.")
                     mode)
             (setcdr cell (cons 'isort (if (consp current-checkers)
                                           current-checkers
                                         (list current-checkers)))))))
       (message "apheleia set up to use isort for Python."))
      (t
       (message "apheleia setup: neither darker nor isort available")))))


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

;; XXX RECIPE
(with-eval-after-load 'auto-highlight-symbol
  (el-patch-defun ahs-change-range (&optional range nomsg)
    "Current plugin change to `RANGE' plugin. `RANGE' defaults to next runnable
plugin."
    (interactive)
    (el-patch-remove (ahs-clear (not nomsg)))

    (when (if range (ahs-valid-plugin-p range)
            (setq range (ahs-runnable-plugins t)))
      (ahs-change-range-internal range)
      (let ((ahs-suppress-log nomsg))
        (ahs-log 'plugin-changed (ahs-decorated-current-plugin-name))))

    (when (ahs-called-interactively-p 'interactive)
      (ahs-idle-function))
    (ahs-set-lighter))

  (el-patch-validate 'ahs-change-range 'defun t))


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
           ("M-g M-g" . avy-goto-line)
           ;; IntelliJ's limitations have broken me.
           ("M-g M-l" . avy-goto-line))


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


;;; bm

;; XXX RECIPE?
(el-patch-feature bm)

(with-eval-after-load 'bm
  (el-patch-defun bm-show-goto-bookmark ()
    "Goto the bookmark on current line in the `bm-show-buffer-name' buffer."
    (interactive)
    (let ((buffer-name (get-text-property (point) 'bm-buffer))
          (bookmark (get-text-property (point) 'bm-bookmark)))
      (if (null buffer-name)
          (when (> bm-verbosity-level  0)
            (message "No bookmark at this line."))
        (el-patch-remove
          (pop-to-buffer (get-buffer buffer-name))
          (bm-goto bookmark)
          (when bm-electric-show (bm-show-quit-window)))
        (el-patch-add
          (when bm-electric-show
            (bm-show-quit-window))
          (switch-to-buffer (get-buffer buffer-name))
          (bm-goto bookmark)))))

  (el-patch-validate 'bm-show-goto-bookmark 'defun t))

(bind-keys ("<f1>" . bm-toggle)
           ("<f7>" . bm-previous)
           ("<f8>" . bm-next)
           ("M-<f1>" . bm-show-all))

(setq bm-highlight-style 'bm-highlight-only-fringe)


;;; bookmark

;; My hints for using bookmarks:
;;
;; * C-x r m: set bookmark *at point*
;; * C-x r l or C-x p e: show bookmark list
;; * (In bookmark list) C-u a: edit annotation
;; * C-x p s: save bookmarks file (should also happen at exit)

(my:load-recipes 'bookmark-auto-save)


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

;; Since I use `common-lisp-indent-function' in Elisp, and Elisp often
;; has some deeply nested lists (and CL probably does too), turning
;; this up lets examples like this be formatted nicely:
;;
;;     (custom-set-faces
;;      '(default ((t (:inverse-video  nil
;;                     :box            nil
;;                     :strike-through nil)))))
;;
;; The default value of 3 means the keyword plist above would be
;; indented funny starting with the :box line.  (Try indenting that
;; with `lisp-indent-function' to see what I mean.
(setq lisp-indent-maximum-backtracking 10)


;;; clean-aindent-mode

(clean-aindent-mode 1)

(bind-keys :map clean-aindent-mode--keymap
           ([remap backward-kill-word] . nil))


;;; clj-refactor

(defun my:clj-refactor-mode-hook ()
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(my:add-hooks 'clj-refactor-mode-hook
  #'my:clj-refactor-mode-hook)

(my:add-hooks 'clojure-mode-hook
  #'clj-refactor-mode)


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
      company-show-quick-access t
      company-quick-access-modifier 'control
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

  ;; Pick up my changes to `company-quick-access-modifier'.
  (customize-set-variable 'company-quick-access-modifier company-quick-access-modifier)

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
                 'company-remove-duplicates-ignoring-annotations
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
             ("<up>" . nil)))

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


;;; company-shell

(setq company-shell-dont-fetch-meta t)


;;; conf-mode

;; conf-mode doesn't derive from anything.  Run prog-mode-hook by
;; hand.  What could go wrong?
(defun my:conf-mode-hook ()
  (run-hooks 'prog-mode-hook))

(add-hook 'conf-mode-hook #'my:conf-mode-hook)

(add-hook 'conf-mode-hook #'faux-indent-mode)

(defun my:conf-toml-mode-hook ()
  (setq indent-tabs-mode nil
        tab-width 4))

(add-hook 'conf-toml-mode-hook #'my:conf-toml-mode-hook)

(with-eval-after-load 'conf-mode
  (bind-keys :map conf-mode-map
             ("RET" . newline-and-indent)))


;;; counsel

(when (eq my:completion-framework 'ivy)
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
    (set-buffer (with-ivy-window (window-buffer)))))


;;; counsel-auto-grep

(with-eval-after-load 'counsel
  (bind-keys :map counsel-mode-map
             ("M-m /" . counsel-auto-grep-maybe-projectile)
             ("M-m s f" . counsel-auto-grep-ask-dir)))


;;; counsel-css

(when (eq my:completion-framework 'ivy)
  (add-hook 'css-mode-hook #'counsel-css-imenu-setup))


;;; counsel-projectile

(when (eq my:completion-framework 'ivy)
  (with-eval-after-load 'projectile
    (counsel-projectile-mode 1)))


;;; consult

;; XXX RECIPES

;; We have to follow this naming convention, see below (search for
;; just "current-last").
(defun consult--buffer-sort-current-last (buffers)
  "Puts the current buffer last, but otherwise doesn't sort at all.
Intention is that buffers are returned in MRU order."
  (let ((current (current-buffer)))
    (if (memq current buffers)
        (nconc (remq current buffers) (list current))
      buffers)))

(with-eval-after-load 'consult
  (el-patch-feature consult)

  ;; `consult-line' makes it so M-n brings up the line under point,
  ;; not the symbol.  Make M-n bring up the symbol under point first.
  (el-patch-defun consult-line (&optional initial start)
    "Search for a matching line.

Depending on the setting `consult-point-placement' the command
jumps to the beginning or the end of the first match on the line
or the line beginning.  The default candidate is the non-empty
line next to point.  This command obeys narrowing.  Optional
INITIAL input can be provided.  The search starting point is
changed if the START prefix argument is set.  The symbol at point
and the last `isearch-string' is added to the future history."
    (interactive (list nil (not (not current-prefix-arg))))
    (let* ((curr-line (line-number-at-pos (point) consult-line-numbers-widen))
           (top (not (eq start consult-line-start-from-top)))
           (candidates (consult--slow-operation "Collecting lines..."
                         (consult--line-candidates top curr-line))))
      (consult--read
       candidates
       :prompt (if top "Go to line from top: " "Go to line: ")
       :annotate (consult--line-prefix curr-line)
       :category 'consult-location
       :sort nil
       :require-match t
       ;; Always add last `isearch-string' to future history
       :add-history (list (thing-at-point 'symbol) isearch-string)
       :history '(:input consult--line-history)
       :lookup #'consult--line-match
       (el-patch-remove :default (car candidates))
       ;; Add `isearch-string' as initial input if starting from Isearch
       :initial (or initial
                    (and isearch-mode
                         (prog1 isearch-string (isearch-done))))
       :state (consult--location-state candidates))))

  (el-patch-validate 'consult-line 'defun t)

  ;; `consult-buffer' provides no way to stop sorting visible buffers
  ;; to the bottom of the list.  We have to define our own sort
  ;; function and then patch it in to the compiled lambdas in all
  ;; these variables.  I should probably contribute a patch to make
  ;; this a setting.

  (el-patch-defvar consult--source-project-buffer
      `(:name     "Project Buffer"
        :narrow   ?b
        :category buffer
        :face     consult-buffer
        :history  buffer-name-history
        :state    ,#'consult--buffer-state
        :enabled  ,(lambda () consult-project-function)
        :items
        ,(lambda ()
           (when-let (root (consult--project-root))
             (consult--buffer-query :sort (el-patch-swap 'visibility 'current-last)
                                    :directory root
                                    :as #'consult--buffer-pair))))
    "Project buffer candidate source for `consult-buffer'.")

  (el-patch-validate 'consult--source-project-buffer 'defvar t)

  (el-patch-defvar consult--source-hidden-buffer
      `(:name     "Hidden Buffer"
        :narrow   ?\s
        :hidden   t
        :category buffer
        :face     consult-buffer
        :history  buffer-name-history
        :action   ,#'consult--buffer-action
        :items
        ,(lambda () (consult--buffer-query :sort (el-patch-swap 'visibility 'current-last)
                                           :filter 'invert
                                           :as #'consult--buffer-pair)))
    "Hidden buffer candidate source for `consult-buffer'.")

  (el-patch-validate 'consult--source-hidden-buffer 'defvar t)

  (el-patch-defvar consult--source-modified-buffer
      `(:name     "Modified Buffer"
        :narrow   ?*
        :hidden   t
        :category buffer
        :face     consult-buffer
        :history  buffer-name-history
        :state    ,#'consult--buffer-state
        :items
        ,(lambda () (consult--buffer-query :sort (el-patch-swap 'visibility 'current-last)
                                           :as #'consult--buffer-pair
                                           :predicate
                                           (lambda (buf)
                                             (and (buffer-modified-p buf)
                                                  (buffer-file-name buf))))))
    "Modified buffer candidate source for `consult-buffer'.")

  (el-patch-validate 'consult--source-modified-buffer 'defvar t)

  (el-patch-defvar consult--source-buffer
      `(:name     "Buffer"
        :narrow   ?b
        :category buffer
        :face     consult-buffer
        :history  buffer-name-history
        :state    ,#'consult--buffer-state
        :default  t
        :items
        ,(lambda () (consult--buffer-query :sort (el-patch-swap 'visibility 'current-last)
                                           :as #'consult--buffer-pair)))
    "Buffer candidate source for `consult-buffer'.")

  (el-patch-validate 'consult--source-buffer 'defvar t))

(when (eq my:completion-framework 'vertico)
  (bind-keys ("s-s" . consult-line)
             ("M-m j i" . consult-imenu)
             ("M-m /" . consult-ripgrep)
             ("C-x b" . consult-buffer)
             ("C-x 4 b" . consult-buffer-other-window)
             ("C-x 5 b" . consult-buffer-other-frame)
             ("M-y" . consult-yank-pop))

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))



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


;;; ctrlf

(require 'ctrlf)

;; `ctrlf-mode-bindings' needs to be modified *before* you turn on
;; `ctrlf-mode'.
;; (cl-loop
;;   for (remap . new-binding) in
;;    '((isearch-forward . ctrlf-forward-fuzzy-regexp)
;;      (isearch-backward . ctrlf-backward-fuzzy-regexp))
;;   do (setf (alist-get `[remap ,remap] ctrlf-mode-bindings nil nil #'equal)
;;            new-binding))

(ctrlf-mode 1)

(setq ctrlf-default-search-style 'fuzzy-regexp
      ctrlf-alternate-search-style 'literal
      ctrlf-go-to-end-of-match nil)

;; XXX bug fix?
;;
;; The `eval-after-load' is now calling some
;; `cconv-make-interpreted-closure' function (I think that's the one),
;; which in turn ends up in `macroexpand-1' eventually, which calls
;; `autoload-do-load'--all told, *way* too fucking much is happening
;; in `ctrlf-local-mode', and motherfucker runs every single time
;; eldoc puts something in the echo area, for example (which may
;; itself be unintended).  Try and take it out, see if this fixes the
;; annoying AF pauses.

(el-patch-feature ctrlf)

(with-eval-after-load 'ctrlf
  (el-patch-define-minor-mode ctrlf-local-mode
      "Minor mode to use CTRLF in place of Isearch."
    :keymap ctrlf-mode-map
    (require 'map)
    ;; Weird indentation to make things indent the same in both Emacs
    ;; 28 and Emacs 29, because apparently something changed?
    (let (( default-ctrlf-mode-bindings
           (eval (car (get 'ctrlf-mode-bindings 'standard-value)))))
      (when (and ctrlf-local-mode
                 default-ctrlf-mode-bindings
                 (not (equal ctrlf-mode-bindings default-ctrlf-mode-bindings)))
        (when ctrlf--ctrlf-mode-bindings-deprecation-warning
          (message "Variable `ctrlf-mode-bindings' is deprecated. Please use \
`ctrlf-mode-map' to customize your keybindings instead.")
          (setq ctrlf--ctrlf-mode-bindings-deprecation-warning nil))
        ;; Hack to clear out keymap. Presumably there's a `clear-keymap'
        ;; function lying around somewhere...?
        (setcdr ctrlf-mode-map nil)
        (map-apply
         (lambda (key cmd)
           (when (stringp key)
             (setq key (kbd key)))
           (define-key ctrlf-mode-map key cmd))
         ctrlf-mode-bindings)))
    (el-patch-splice 2 0
      (with-eval-after-load 'ctrlf
        ;; TODO: This appears to have a bug where if CTRLF is enabled
        ;; globally, then disabled in a particular buffer, then the
        ;; advice will be removed globally. Instead, it should be
        ;; removed only when there are no buffers remaining with CTRLF
        ;; enabled.
        (if ctrlf-local-mode
            (advice-add #'minibuffer-message :around
                        #'ctrlf--minibuffer-message-condense)
          (advice-remove #'minibuffer-message
                         #'ctrlf--minibuffer-message-condense)))))

  (el-patch-validate 'ctrlf-local-mode 'define-minor-mode t))


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


;;; desktop

(desktop-save-mode 1)


;;; diff-hl

(global-diff-hl-mode 1)

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)


;;; dired

(setq dired-dwim-target t)

(with-eval-after-load 'dired
  (bind-keys :map dired-mode-map
             ("<mouse-1>" . dired-mouse-find-file)
             ("<mouse-2>" . dired-mouse-find-file)))

;; Combine all bordering-on-abusively-numerous dired menus into a
;; single menu item.  macOS users rejoice.
(underlings-move-menu-with-one-time-hook 'dired-mode
                                         ;; Quoted for indentation, sigh.
                                         '["Operate" "Mark" "Regexp"
                                           "Immediate" "Subdir"]
                                         "Dired")

(my:load-recipes 'dired-fix-48461-directory-renames)


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


;;; dired-x

;; Binds C-x C-j.  Probably does other stuff I care about.
(require 'dired-x)

;; Add more LaTeX extensions to ignore.  I'm not turning on
;; `dired-omit-mode' by default because I think changing the default
;; behavior of dired in such a way that it hides some files from me
;; would be far too error-prone.
(dolist (ext '(".fdb_latexmk" ".fls"))
  (if (member ext dired-latex-unclean-extensions)
      (warn (concat "%S is already in `dired-latex-unclean-extensions',"
                    " update init.el")
            ext)
    (add-to-list 'dired-latex-unclean-extensions ext)
    (add-to-list 'dired-omit-extensions ext)))


;;; display-line-numbers

(setq display-line-numbers-width (+ 88 5))

;; If I just turn this on, Emacs crashes, so I turn it on like this.
(add-hook 'after-init-hook #'global-display-line-numbers-mode t)


;;; dtrt-indent

;; Do this before dtrt-indent gets loaded, hopefully.
(setq dtrt-indent-active-mode-line-info nil)

(setq dtrt-indent-min-quality 70.0)

;; I get real annoyed with dtrt-indent doesn't engage in sh-mode
;; buffers, just because sh-mode uses SMIE.
;;(setq dtrt-indent-run-after-smie t)
;;
;; Wait, stop, don't use that setting.  It causes dtrt-indent to call
;; `smie-config-guess', which behaves just horribly in sh-mode, and I
;; bet it behaves horribly everywhere else too.
;;
;; Let's just patch out all this SMIE special-casing.

(el-patch-feature 'dtrt-indent)

(with-eval-after-load 'dtrt-indent
  (el-patch-define-minor-mode dtrt-indent-mode
      "Toggle dtrt-indent mode.
With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode.

When dtrt-indent mode is enabled, the proper indentation offset
and `indent-tabs-mode' will be guessed for newly opened files and
adjusted transparently."
    :lighter " dtrt-indent"
    :group 'dtrt-indent
    (if dtrt-indent-mode
        (el-patch-splice 3 0
          (if (and (featurep 'smie) (not (null smie-grammar)) (not (eq smie-grammar 'unset)))
              (progn
                (when (null smie-config--buffer-local) (smie-config-guess))
                (when dtrt-indent-run-after-smie
                  (dtrt-indent-try-set-offset)))
            (dtrt-indent-try-set-offset)))
      (dtrt-indent-undo)))

  (el-patch-validate 'dtrt-indent-mode 'define-minor-mode t))

(dtrt-indent-global-mode 1)

;; If dtrt-indent changed buffer settings like indent-tabs-mode,
;; whitespace-mode may/probably will need to be reset.
(define-advice dtrt-indent-try-set-offset
    (:after (&rest args) my:dtrt-indent-reset-whitespace-mode)
  (when (and (boundp 'whitespace-mode) whitespace-mode)
    (whitespace-mode -1)
    (whitespace-mode 1)))

;; `conf-mode' and its derivatives are all based on
;; `fundamental-mode', which dtrt-indent doesn't hook into.
(add-hook 'conf-mode-hook #'dtrt-indent-mode)


;;; dumb-jump

;; `dumb-jump-default-project' seems to default to "~", which I think
;; means it will search your entire home directory by default?  That
;; is... an interesting choice, one that I wish to avoid, personally.
(setq dumb-jump-default-project nil)

(with-eval-after-load 'dumb-jump
  (when (executable-find "rg")
    ;; Force dumb-jump to always use ripgrep instead of ag
    ;; (unsatisfactory gitignore handling) or git-grep (only searches
    ;; tracked files).
    (setq dumb-jump-force-searcher 'rg)
    ;; Ignore .gitignore when using ripgrep from dumb-jump.  You might
    ;; want this, for example, to have it search ~/.emacs.d/elpa,
    ;; which I have in a .gitignore.
    (let ((args (cons "-u"
                      (split-string-and-unquote (or dumb-jump-rg-search-args
                                                    "")))))
      (setq dumb-jump-rg-search-args
            (combine-and-quote-strings (seq-uniq args))))))

(my:load-recipes 'dumb-jump-only-if-no-tags
                 'dumb-jump-better-project-root-errors)


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

(my:load-recipes 'elec-pair-skip-inside-quotes)


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


;;; emacs-lsp-booster

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(advice-add (if (progn (require 'json) (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)
             ;; for check lsp-server-present?  see
             ;; lsp-resolve-final-command, it would add extra shell
             ;; wrapper
             (not (file-remote-p default-directory))
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)


;;; embark

(when (eq my:completion-framework 'vertico)
  (bind-keys ("C-." . embark-act)
             ("C-;" . embark-dwim)
             ("C-h b" . embark-bindings))

  (bind-keys :map minibuffer-local-map
             ("C-c C-o" . embark-export)))


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
    (set-face-attribute 'default nil :font "Fira Mono 9")
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


;;; fennel-mode

(with-eval-after-load 'fennel-mode
  (unbind-key "M-'" fennel-mode-map))


;;; files

(setq make-backup-files nil
      confirm-kill-emacs 'y-or-n-p
      require-final-newline t)

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
               (cons (rx bos (* (any space ?\n))
                         symbol-start
                         (or (regexp keywords-regexp)
                             (: (| "with" "WITH")
                                (+ (any space ?\n))
                                (? (| "recursive" "RECURSIVE")
                                   (+ (any space ?\n)))
                                (+ (not (any space ?\n)))
                                (+ (any space ?\n))
                                (| "as" "AS")
                                (+ (any space ?\n))
                                ?\(
                                )))
                     'sql-mode)))

(my:load-recipes 'files-delete-auto-save-after-revert-buffer
                 'files-save-some-buffers-default-no-query)


;;; fill

;; Make `adaptive-fill-mode' recognize numbered lists as well.

(unless (equal (my:get-standard-value 'adaptive-fill-regexp)
               "[-–!|#%;>*·•‣⁃◦ \t]*")
  (warn "`adaptive-fill-regexp' changed from 30.0.50 value, check your mod"))

(setq adaptive-fill-regexp
      (rx (or
           ;; Original regexp.
           (* (any "[-–!|#%;>*·•‣⁃◦ \t]*"))
           ;; Numbered lists.  I wonder why RMS took this out...  (See
           ;; comment in fill.el.)
           (: (1+ digit) ?.))))


;;; find-func

(bind-keys ("M-m j e f" . find-function)
           ("M-m j e v" . find-variable)
           ("M-m j e l" . find-library))

(which-key-add-key-based-replacements "M-m j e" "emacs")

(setq find-function-recenter-line nil)


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
    '(
      ;; 80
      ;; 88
      ;; 93 = 88 + 5 columns for line numbers
      ;; 93
      ;; 94 = 88 + 5 columns for line numbers + 1 columns for Lord RMS??
      94
      ;; 95
      nil)))

(with-eval-after-load 'frame-resize
  (add-to-list 'frame-resize-window-size-functions
               #'my:frame-resize-window-default t)

  (dolist (func '(
                  ediff-toggle-split
                  imenu-list-quit-window
                  kill-buffer-and-window
                  magit-mode-bury-buffer
                  my:delete-window-that-direction
                  org-roam-buffer
                  org-roam-buffer-display-dedicated
                  org-roam-buffer-toggle
                  transpose-frame
                  treemacs
                  treemacs-quit
                  winner-undo
                  ))
    (add-to-list 'auto-frame-resize-commands func)))

(defun my:auto-frame-resize-after-advice (&rest _)
  (frame-resize))

(advice-add 'display-buffer-in-side-window :after
            #'my:auto-frame-resize-after-advice)

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


;;; flx-rs

(with-eval-after-load 'fussy
  (flx-rs-load-dyn)
  (setq fussy-score-fn 'fussy-flx-rs-score))



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
      ;; However, I ran with 2000 for a while, and I think I can say
      ;; that, when you have even 1100 errors, Emacs slows down too
      ;; much.  Let's try 1000 instead.
      flycheck-checker-error-threshold 1000)

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

  ;; Preference order of some Python checkers.
  (let ((preferred-python-checkers '(python-ruff python-mypy python-pyright python-pylint)))
    (setq flycheck-checkers
          (nconc (copy-tree preferred-python-checkers)
                 (seq-remove (lambda (checker)
                               (memq checker preferred-python-checkers))
                             flycheck-checkers))))

  ;; pylint is expensive to run, only run it on save.
  (setf (flycheck-checker-get 'python-pylint 'predicate) #'flycheck-buffer-saved-p)

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

(defun my:flycheck-find-terraform-root-module (&optional _checker)
  (and buffer-file-name
       (locate-dominating-file buffer-file-name ".terraform")))

(with-eval-after-load 'terraform-mode
  (with-eval-after-load 'flycheck
    (flycheck-define-checker my:terraform-validate
      "A Terraform checker with `terraform validate'.

See URL `https://www.terraform.io/docs/commands/validate.html'."
      :working-directory my:flycheck-find-terraform-root-module
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

;; ;; Mypy really needs a working directory other than CWD.
;; ;; Encountered when I had a module called logging.py.  Cribbed from
;; ;; https://github.com/flycheck/flycheck/pull/1771.  Delete this when
;; ;; that's merged.
;; (defun my:flycheck-python-find-project-root (_checker)
;;   (let ((start (if buffer-file-name
;;                    (file-name-directory buffer-file-name)
;;                  default-directory)))
;;     (or (flycheck--locate-dominating-file-matching
;;          start (regexp-opt '("pyproject.toml" "setup.cfg" "mypy.ini"
;;                              "pyrightconfig.json")))
;;         (locate-dominating-file
;;          start (lambda (dir)
;;                  (not (file-exists-p (expand-file-name "__init__.py" dir))))))))
;; 
;; (with-eval-after-load 'flycheck
;;   (when (fboundp 'flycheck-python-find-project-root)
;;     (warn (concat "https://github.com/flycheck/flycheck/pull/1771"
;;                   " probably merged, update your init.el")))
;; 
;;   (setf (flycheck-checker-get 'python-mypy 'working-directory)
;;         #'my:flycheck-python-find-project-root))


;; ruff
(with-eval-after-load 'flycheck
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
    :command ("ruff"
              "check"
              "--output-format=text"
              (eval (when buffer-file-name
                      (concat "--stdin-filename=" buffer-file-name)))
              "-")
    :standard-input t
    :error-filter (lambda (errors)
                    (let ((errors (flycheck-sanitize-errors errors)))
                      (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message (one-or-more not-newline))
              line-end))
    :modes (python-mode python-ts-mode))

  ;; Chain Pyright after Ruff.
  (flycheck-add-next-checker 'python-ruff 'python-pyright)

  (my:add-to-list-before 'flycheck-checkers 'python-ruff 'python-pylint))


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


;;; git-link

(with-eval-after-load 'git-link
  (setq git-link-use-commit t))


;;; go-mode

(defun my:gofmt--gofumpt-set-up-environment (orig-func &rest args)
  (let ((process-environment process-environment))
    (unless (getenv "GOFUMPT_SPLIT_LONG_LINES")
      (setq process-environment (cons  "GOFUMPT_SPLIT_LONG_LINES=on"
                                       process-environment)))
    (apply orig-func args)))

(with-eval-after-load 'go-mode
  (when (executable-find "gofumpt")
    (setq gofmt-command "gofumpt")
    (advice-add 'gofmt :around #'my:gofmt--gofumpt-set-up-environment)
    (setq lsp-go-use-gofumpt t)))

(defun my:go-mode-hook ()
  (setq-local tab-width 4)
  (add-hook 'before-save-hook 'gofmt-before-save nil t)
  (when lsp-mode
    (add-hook 'before-save-hook 'lsp-organize-imports nil t)))

(my:add-hooks 'go-mode-hook
  #'lsp
  #'subword-mode
  #'my:go-mode-hook)


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

(my:load-recipes 'he-dabbrev-obey-dabbrev-skip-leading-regexp)


;;; hl-line

(global-hl-line-mode 1)


;;; hl-todo

(global-hl-todo-mode 1)

(with-eval-after-load 'hl-todo
  ;; `conf-mode' (and it's derivatives) derive from `fundamental-mode'
  ;; (to my surprise).
  (add-to-list 'hl-todo-include-modes 'conf-mode))

(if-let ((xxx-face (assoc "XXXX*" hl-todo-keyword-faces)))
    (setf (cdr xxx-face)
          '(:inherit hl-todo :foreground "yellow" :background "red"))
  (warn "Couldn't find XXXX* in `hl-todo-keyword-faces'"))


;;; ielm

(my:add-hooks 'ielm-mode-hook
  #'paredit-mode)

(underlings-move-menu-with-one-time-hook 'ielm-map
                                         ["Complete" "In/Out" "Signals"]
                                         "IELM"
                                         :hook-var 'ielm-mode-hook)


;;; imenu

;; I use `consult-imenu' with Vertico.
(unless (eq my:completion-framework 'vertico)
  (bind-key "M-m j i" 'imenu))

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


;;; isearch

(my:load-recipes 'isearch-exit-at-beginning-of-match)

(bind-keys :map isearch-mode-map
           ("C-'" . my:avy-isearch))


;;; iso-transl

(with-eval-after-load 'iso-transl
  (add-to-list 'which-key-replacement-alist
               '(("C-x 8" . "Prefix Command") . (nil . "unicode"))))


;;; ivy

(when (eq my:completion-framework 'ivy)
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
             ("S-SPC" . nil)
             ;; This will rotate through `ivy-preferred-re-builders',
             ;; which I set below.
             ("M-r" . ivy-rotate-preferred-builders))

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

  (my:load-recipes 'ivy-dont-hide-collection-errors
                   'ivy-fuzzy-regex-combo-matcher
                   'ivy-regex-plus-or-literal-regex
                   'ivy-rotate-preferred-builder-feedback
                   'ivy-special-switch-buffers)

  ;; Use `my:ivy--regex-regular-or-fuzzy' as our default regexp builder.
  (setf (alist-get t ivy-re-builders-alist) #'my:ivy--regex-regular-or-fuzzy)

  ;; First is my default, as above.  Second is `ivy--regex-ignore-order'
  ;; because that is the matcher I most frequently use instead of fuzzy.
  ;; Third is just "try and make a regexp if it looks like you have
  ;; groups, otherwise `ivy--regex-plus'".
  ;;
  ;; This is used by `ivy-rotate-preferred-builders', which I have bound
  ;; above.
  (setq ivy-preferred-re-builders
        '((my:ivy--regex-regular-or-fuzzy . "auto-fuzzy")
          (ivy--regex-ignore-order . "order")
          (my:ivy--regex-plus-or-literal-regex . "regexp")))

  ;; It's been a while since I wrote this, but IIRC, these commands
  ;; don't work well with my fuzzy matcher, or probably with any out of
  ;; order matcher such as `ivy--regex-ignore-order'.  And/or they were
  ;; horribly slow with something like `ivy--regex-fuzzy'.  Hence
  ;; forcing a different regexp builder here.
  (dolist (command '(counsel-rg swiper-isearch))
    (setf (alist-get command ivy-re-builders-alist)
          #'my:ivy--regex-plus-or-literal-regex)))


;;; ivy-bibtex

(when (eq my:completion-framework 'ivy)
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key))


;;; ivy-xref

(when (eq my:completion-framework 'ivy)
  (setq xref-show-definitions-function #'ivy-xref-show-defs
        xref-show-xrefs-function #'ivy-xref-show-xrefs
        ivy-xref-use-file-path t))


;;; js2-mode

;; Bigger files get parsed less often.  This should perhaps be halved
;; to parse even less eagerly---it's really slow on "big" files.
(setq js2-dynamic-idle-timer-adjust 8192)

;; https://elpa.gnu.org/packages/js2-mode.html
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; https://github.com/mooz/js2-mode/issues/529
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))

(defun my:maybe-turn-on-js2-minor-mode ()
  (unless (eq major-mode 'json-mode)
    (js2-minor-mode 1)))

(add-hook 'js-mode-hook #'my:maybe-turn-on-js2-minor-mode)

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


;;; lsp-mode

(with-eval-after-load 'lsp-mode
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 102400000
        read-process-output-max (* 1024 1024))

  (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)

  (bind-keys :map lsp-mode-map
             ("M-<f7>" . lsp-find-references)
             ("M-<left>" . lsp-ui-find-prev-reference)
             ("M-<right>" . lsp-ui-find-next-reference))

  (dolist (dir-name '(".venv" ".direnv" ".pytest_cache" ".mypy_cache"
                      "__pycache__"))
    (add-to-list 'lsp-file-watch-ignored-directories
                 (rx (char ?/ ?\\) (literal dir-name) eos)))

  (add-to-list 'lsp-file-watch-ignored-files
               (rx (char ?/ ?\\) (+ (not (any ?/ ?\\))) ".pyc" eos)))

;; I had once set `lsp-keep-workspace-alive' to nil.  Its default is
;; t.  I don't know why I ever wanted nil, it's a PITA to restart
;; Python LSPs.  If anything I should write something to _eventually_
;; kill off LSP servers with no buffers.
(setq lsp-keep-workspace-alive t
      lsp-modeline-diagnostics-scope :file
      lsp-pyls-plugins-pylint-enabled t)

;; `lsp-signature-render-documentation' is much too intrusive in the
;; echo area, renders potentially lots of documentation.  You can turn
;; this off but still leave on the signatures (in an eldoc-esque
;; fashion).
(setq lsp-signature-render-documentation nil)

(defun my:lsp-mode-hook ()
  (when (and lsp-enable-symbol-highlighting
             (bound-and-true-p auto-highlight-symbol-mode))
    (auto-highlight-symbol-mode -1)))

(add-hook 'lsp-mode-hook #'my:lsp-mode-hook)

;; (defun my:disable-elpy-before-lsp (orig-fun arg &rest _args)
;;   (let ((yas-minor-mode ))
;;     (when (and (bound-and-true-p elpy-mode)
;;                (cond
;;                  ((eq arg 'toggle)
;;                   (not lsp-mode))
;;                  ((and
;;                    (numberp arg)
;;                    (< arg 1))
;;                   nil)
;;                  (t t)))
;;       (elpy-mode -1)
;;       ;; Sigh, elpy doesn't remove itself from `xref-backend-functions'.
;;       (remove-hook 'xref-backend-functions 'elpy--xref-backend t))))
;; 
;; (advice-add #'lsp-mode :before #'my:disable-elpy-before-lsp)

(defun my:lsp-headerline-breadcrumb-mode-hook ()
  (which-function-mode -1))

(add-hook 'lsp-headerline-breadcrumb-mode-hook
          #'my:lsp-headerline-breadcrumb-mode-hook)

(my:load-recipes 'lsp-mode-extra-checkers
                 'lsp-mode-truncate-headerline-breadcrumbs
                 'lsp-mode-turn-on-with-local-variable
                 'lsp-mode-update-breadcrumbs-while-in-minibuffer)

(setf (alist-get 'python-mode my:lsp-flycheck-extra-checkers-alist)
      '(python-pylint python-mypy))

(setf (alist-get 'sh-mode my:lsp-flycheck-extra-checkers-alist) '(sh-shellcheck))

(setf (flycheck-checker-get 'python-pylint 'predicate) #'flycheck-buffer-saved-p)

(cl-defun my:lsp-install-format-on-save (&key (format t) (imports t))
  (when format
    (add-hook 'before-save-hook #'lsp-format-buffer 90 t))
  (when imports
    (add-hook 'before-save-hook #'lsp-organize-imports 90 t)))


;;; lsp-pyright

(setq lsp-pyright-multi-root nil)
(when (featurep 'lsp-pyright)
  (warn "Feature lsp-pyright present, set `lsp-pyright-multi-root' too late."))

(my:with-eval-after-all-load '(python lsp-mode)
  (require 'lsp-pyright)

  ;; Prevent other language servers from being used for Python.
  ;; (Pyright is the least bad, *probably*, for now.)
  (dolist (server '(pyls pylsp))
    (cl-pushnew server (alist-get 'python-mode lsp-disabled-clients)))

  ;; You get better import completion with this.  I still think some
  ;; file in the project has to be using a given import in order for
  ;; Pyright to offer you auto-import completions (along with this
  ;; setting).  See also:
  ;; https://github.com/fannheyward/coc-pyright/issues/90#issuecomment-813804148
  (setq lsp-pyright-diagnostic-mode "workspace"))


;;; lsp-ui

(setq lsp-ui-doc-enable nil)


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
      magit-bury-buffer-function 'magit-mode-quit-window
      magit-process-finish-apply-ansi-colors t)

;; `truncate-lines' does not work well for me when viewing diffs and
;; such.

(defun my:magit-mode-hook ()
  (setq truncate-lines nil))

(add-hook 'magit-mode-hook #'my:magit-mode-hook)

(which-key-add-major-mode-key-based-replacements 'magit-status-mode
    "j p" "unpushed"
    "j f" "unpulled")


;;; marginalia

(when (eq my:completion-framework 'vertico)
  (marginalia-mode 1)

  (bind-keys :map minibuffer-local-map
             ("M-A" . marginalia-cycle)))


;;; markdown-mode

;; If you get
;;
;;     which-func-ff-hook error: (wrong-type-argument consp nil)
;;
;; try eval'ing `markdown-imenu-create-nested-index'.  Maybe native
;; comp problem?  https://github.com/jrblevin/markdown-mode/issues/578

(with-eval-after-load 'markdown-mode
  (bind-keys :map markdown-mode-map
             ("M-m m c e" . my:markdown-mode-copy-as-html-email)
             ;; This interferes with my global binding to link-hint.
             ("C-c C-o" . nil))

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
                   'markdown-mode-shifttab-unindents-lists
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

;; XXX lisp/recipe

(defvar my:available-completion-styles
  (seq-filter (lambda (style) (require style nil t))
              '(fussy orderless)))

(defun my:cycle-completion-style ()
  (interactive)
  (unless my:available-completion-styles
    (user-error "No available completion styles"))
  (let ((next-style (pop my:available-completion-styles)))
    ;; Orderless recommended putting basic in the list always, so that
    ;; TRAMP completion would work.  I'm taking this as general advice
    ;; for any completion style (other than basic), which may be
    ;; incorrect.
    (setf completion-styles (list next-style 'basic)
          my:available-completion-styles (nconc my:available-completion-styles (list next-style)))
    (message "Using completion style %S" next-style)
    (when (and (minibufferp)
               (bound-and-true-p vertico--input))
      ;; This seems to force Vertico to recompute candidates.
      (setq vertico--input t))))

(bind-keys ("C-S-l" . my:cycle-completion-style))

(if my:available-completion-styles
    ;; Call it once to set one of them up.
    (my:cycle-completion-style)
  ;; Vanilla Emacs styles.
  (setq completion-styles '(basic partial-completion initials flex))
  ;; Presumably still needed with flex style...?
  (my:load-recipes 'minibuffer-flex-completion-ignore-long-candidates))

;;; minions

(setq minions-mode-line-lighter "🄼"
      minions-prominent-modes '(
                                black-format-on-save-mode
                                ;; darker-format-on-save-mode
                                docformatter-on-save-mode
                                flycheck-mode
                                isort-format-on-save-mode
                                multiple-cursors-mode
                                yapf-format-on-save-mode
                                ))

(minions-mode 1)


;;; modus-vivendi-theme

(when (featurep 'modus-vivendi-theme)
  (warn (concat "modus-vivendi-theme has been loaded too early,"
                " `modus-vivendi-theme-override-colors-alist' may not"
                " be respected")))

(setq modus-themes-paren-match '(intense)
      modus-themes-headings '((t . (regular))))


;;; move-text

(move-text-default-bindings)


;;; mule

;; Note that C-x RET is actually bound in mule-cmds.el, but that
;; doesn't `provide' anything.
(with-eval-after-load 'mule
  (add-to-list 'which-key-replacement-alist
               '(("C-x RET" . "Prefix Command") . (nil . "mule"))))


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


;;; mwheel

;; Make horizontal scrolling work like every other application.
;; Maybe.  Credit to parsnip!
(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)


;;; nav-stack

(nav-stack-mode 1)

(defun my:nav-stack-auto-push-predicate ()
  (not (or (my:pop-up-buffer-p))))

(setq nav-stack-auto-push-predicate #'my:nav-stack-auto-push-predicate)

(defun my:nav-stack-pop-predicate (win mark)
  (with-current-buffer (marker-buffer mark)
    ;; No empty *Backtrace* buffers (i.e. dead debugger buffers)
    (not (and (derived-mode-p 'debugger-mode)
              (zerop (buffer-size))))))

(setq nav-stack-pop-predicate #'my:nav-stack-pop-predicate)

(add-hook 'nav-stack-post-pop-hook #'my:highlight-line-after-movement)


;;; newcomment

(setq comment-empty-lines 'eol)


;;; obsidian

(obsidian-specify-path "~/Documents/Obsidian")

(with-eval-after-load 'obsidian
  (bind-keys :map obsidian-mode-map
             ("C-c C-o" . obsidian-follow-link-at-point)
             ("C-c C-l" . obsidian-insert-wikilink)
             ("C-c C-b" . obsidian-backlink-jump)))

(my:add-hooks 'obsidian-mode-hook
  #'idle-save-buffer-mode)

;; Activate detection of Obsidian vault
(global-obsidian-mode t)

(defvar my:obsidian-daily-prefix (file-name-concat obsidian-directory "Main"))

(defun my:obsidian-daily ()
  (interactive)
  (let ((date-file-name (format-time-string "%Y-%m-%d.md")))
    (find-file (expand-file-name date-file-name my:obsidian-daily-prefix))))


;;; olivetti

(setq-default olivetti-body-width 80)


;;; orderless

(with-eval-after-load 'orderless
  (setf orderless-matching-styles '(orderless-literal orderless-flex)
        (alist-get ?~ orderless-affix-dispatch-alist) 'orderless-regexp)
  ;; Overriding file is probably necessary for TRAMP completion
  ;; to work, per Orderless's README.
  (setq completion-category-overrides '((file (styles basic partial-completion)))))


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
 ;; 'auto (default) doesn't do what I want.  I have a recipe,
 ;; org-leave-blank-line-before-heading, that adds this 'preserve
 ;; logic for 'heading.
 org-blank-before-new-entry '((heading . preserve) (plain-list-item . nil))
 ;; I don't want or like org creating bookmarks, especially if you
 ;; want to use Bookmark+ (which I no longer do) because it will
 ;; highlight the bookmarks.
 org-bookmark-names-plist nil
 ;; I never use this bookmark, and when I was using Bookmark+, it
 ;; seemed to be visualizing the last thing I captured somehow, which
 ;; annoys me to no end.
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
 org-clock-ask-before-exiting nil
 org-clock-display-default-range 'untilnow
 org-clock-out-remove-zero-time-clocks t
 org-clock-report-include-clocking-task t
 org-default-priority ?D
 org-duration-format '(("h" . t) (special . 2))
 org-enforce-todo-dependencies t
 org-fold-catch-invisible-edits nil
 ;; As of 2022-07-14, 'text-properties (the default) is way too buggy
 ;; for my tastes.  (Disclaimer: it might be that my various mods to
 ;; org-mode are what are causing bugs, but basically the behavior of
 ;; org-mode folding changed, breaking how I want to use org-mode and
 ;; with no discernible benefit to me.)
 org-fold-core-style 'overlays
 org-hide-leading-stars t
 org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
 org-image-actual-width nil
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
 ;; I think I always want indentation preserved in my source
 ;; blocks.
 org-src-preserve-indentation t
 org-src-window-setup 'other-window
 ;; Maybe don't be executing Elisp in table formulas in some random
 ;; org file I downloaded.  (Thanks wasamasa.)
 org-table-allow-automatic-line-recalculation nil
 ;; 'expert stops C-c C-t from popping up a (jarring) temporary
 ;; window.
 org-tags-column -76
 org-use-fast-todo-selection 'expert
 org-use-speed-commands (lambda ()
                          (and (org-at-heading-p)
                               (looking-back "^\\**"
                                             (line-beginning-position)))))

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
  (require 'ox-md)

  (my:load-recipes
   'org-babel-read-table-in-dblock
   'org-columns-delete-property
   'org-daily-time-summary
   'org-fix-C-e
   'org-fix-faces-after-goto
   'org-insert-heading-ignore-invisibility
   'org-jump-over-priority-after-setting-it
   'org-leave-blank-line-before-heading
   'org-make-dabbrev-ignore-emphasis
   'org-make-nice-id-from-headline-text
   'org-property-drawer-fixes
   )

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

  (setf (alist-get "a" org-speed-commands nil nil #'equal)
        'org-archive-subtree-default)

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

;; XXX recipe---though maybe I should do that text property thing I
;; talked about
;;
;; Give us a way to allow certain languages to run without
;; confirmation via org-babel using a local variable.  Otherwise you
;; either have to turn off all confirmations for a file, or put up
;; with confirmation every time you evaluate.

(defvar-local my:org-no-confirm-babel-evaluate-languages nil
  "A list of languages that will be run by org-babel without confirmation.")

(defun my:org-babel-optional-confirm-evaluate (lang body)
  "Bypass confirmation for languages included in
`my:org-no-confirm-babel-evaluate-languages', confirm for
everything else."
  (not (member lang my:org-no-confirm-babel-evaluate-languages)))

(setq org-confirm-babel-evaluate #'my:org-babel-optional-confirm-evaluate)

;; Clock persistence between restarts.

(setq org-clock-persist t
      org-clock-persist-query-resume nil)

(org-clock-persistence-insinuate)

(with-eval-after-load 'org-clock
  (my:load-recipes 'org-compact-clock-in-mode-line
                   'org-clock-update-mode-line-on-clock-in-out
                   'org-switch-to-pending-on-clock-in
                   'org-clock-table-omit-link-urls))

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


;;; org-download

(setq org-download-image-org-width 400)

(with-eval-after-load 'org
  (require 'org-download))

(my:load-recipes 'org-download-macos-applescript)


;;; org-roam

(setq org-roam-directory "~/Documents/org-roam"
      org-roam-dailies-directory "daily/"
      org-roam-v2-ack t)

(my:add-hooks 'org-roam-find-file-hook
  #'idle-save-buffer-mode)

(defun my:org-roam-search ()
  (interactive)
  (cl-ecase my:completion-framework
    (ivy (counsel-auto-grep nil org-roam-directory))
    (vertico (consult-ripgrep org-roam-directory))))

(bind-keys ("M-m r r" . org-roam-buffer-toggle)
           ("M-m r f" . org-roam-node-find)
           ("M-m r i" . org-roam-node-insert)
           ("M-m r d d" . org-roam-dailies-find-directory)
           ("M-m r d f t" . org-roam-dailies-goto-today)
           ("M-m r d c t" . org-roam-dailies-capture-today)
           ("M-m r d f y" . org-roam-dailies-goto-yesterday)
           ("M-m r d c y" . org-roam-dailies-capture-yesterday)
           ("M-m r d f d" . org-roam-dailies-goto-date)
           ("M-m r d c d" . org-roam-dailies-capture-date))

(bind-keys ("M-m r /" . my:org-roam-search))

(with-eval-after-load 'org-roam
  (org-roam-db-autosync-mode)

  ;; https://www.orgroam.com/manual.html#Configuring-the-Org_002droam-buffer-display
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t))))))


;;; osx-dictionary

(bind-keys ("C-$" . osx-dictionary-search-pointer))


;;; outline

(with-eval-after-load 'outline
  (bind-keys :map outline-minor-mode-map
             ("C-c C-2" . outline-hydra/body)))


;;; paredit

(with-eval-after-load 'paredit
  (bind-keys :map paredit-mode-map
             ("M-m j s" . paredit-split-sexp))

  (my:load-recipes 'paredit-delsel
                   'paredit-kill-whole-line
                   'paredit-fix-ielm))


;;; paren

(add-hook 'prog-mode-hook #'show-paren-mode)


;;; prescient.el

;; prescient has support for vertico, but I don't know if I want to
;; use it yet, so I only enable this for Ivy--even in company-mode.

;; NOTE: Package docs say that this must be loaded after counsel.
;; Thankfully not a problem as long as "c" comes before "p" in the
;; alphabet.

(when (eq my:completion-framework 'ivy)
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
                    counsel-yank-pop
                    ivy-yasnippet)
      if (memq func ivy-prescient-sort-commands)
      do (warn "`%S' is already in `ivy-prescient-sort-commands', fix init.el"
               func)
      else
      collect func into funcs-to-add
      finally
       (setq ivy-prescient-sort-commands (nconc ivy-prescient-sort-commands
                                                funcs-to-add)))))


;;; projectile

(projectile-mode 1)

(bind-keys ("C-c C-p" . projectile-command-map)
           ;; Spacemacs bindings, particularly useful when comint
           ;; binds something to C-c C-p.
           ("M-m p" . projectile-command-map))

(underlings-move-menu-with-one-time-hook 'projectile-mode "Projectile" "Minors"
                                         :dest-map global-map :visible t)


;;; pulse

(my:load-recipes 'find-cursor)

(bind-keys ("C-<f8>" . my:find-cursor))


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
            #'my:python-mode-inhibit-electric-indent nil t)

  (when (bound-and-true-p adaptive-fill-mode)
    ;; Teach `adaptive-fill-mode' about "#:" comments for Sphinx.
    (rx-let ((any-indent-space (* (any " \t"))))
      (setq-local adaptive-fill-regexp
                  (rx (or (: any-indent-space "#:" any-indent-space)
                          (regexp adaptive-fill-regexp)))))))

(my:add-hooks 'python-base-mode-hook
  #'my:python-mode-hook
  #'my:warn-white-space-mode
  #'electric-pair-local-mode
  #'smart-tabs-mode
  #'subword-mode
  #'apheleia-mode)

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

(with-eval-after-load 'python
  (my:load-recipes
   'expand-region-python-fix-strings
   'python-add-import
   'python-better-imenu
   'python-colon-doesnt-add-indentation
   'python-edit-indirect-in-strings
   'python-enable-black-from-editorconfig
   'python-fix-dead-shell-font-lock-buffer
   'python-fix-indent-line-columns-vs-characters
   'python-indent-multi-line-strings
   'python-magic-quotes
   'python-multi-line-string-content-starts-on-own-line
   'python-reformat-region-or-buffer
   'python-shell-send-dwim
   'python-toggle-triple-quotes
   'python-up-list-in-strings-and-comments
   )

  (bind-keys :map python-base-mode-map
             ("M-m m q" . my:python-toggle-triple-quotes)
             ("C-c '" . my:python-edit-indirect-dwim)
             ("M-m m f" . my:python-reformat-region-or-buffer)
             ("M-m m F" . black-format-on-save-mode)
             ("C-c C-c" . my:python-shell-send-dwim)
             ("C-c C-b" . python-shell-send-buffer)
             ("M-m m i" . my:python-add-import)))

;; (defun darker-format-buffer ()
;;   (interactive)
;;   ())
;; 
;; (define-minor-mode darker-format-on-save-mode
;;     "Reformat file with darker on save."
;;   :lighter "Drk"
;;   (if darker-format-on-save-mode
;;       (add-hook 'after-save-hook #'darker-format-buffer nil t)
;;     (remove-hook 'after-save-hook #'darker-format-buffer t)))


;;; pyvenv

(exec-path-from-shell-copy-envs '("PYTHONPATH"  "WORKON_HOME"))

(setq pyvenv-mode-line-indicator nil)

(underlings-move-menu-with-one-time-hook 'pyvenv-mode "Virtual Envs" "Minors"
                                         :dest-map global-map :visible t)


;;; rainbow-mode

(with-eval-after-load 'rainbow-mode
  (add-to-list 'rainbow-html-colors-major-mode-list 'web-mode))


;;; recentf-mode

(setq recentf-max-saved-items 1000)

(recentf-mode 1)


;;; rect

(my:load-recipes 'emacs-yank-rectangle-to-new-lines)


;;; reformatter

(reformatter-define autopep8-format
    :program "autopep8"
    :args '("-aa" "-")
    :lighter "AP8")

(reformatter-define black-format
    :program "black"
    :args '("--quiet" "--fast" "-")
    :lighter "Bl")

(reformatter-define docformatter
    :program "docformatter"
    :args '("-")
    :lighter "Df")

(reformatter-define eslint-format
    ;; This is a script of my own.
    :program "eslint-reformat"
    :lighter " ESL")

(reformatter-define isort-format
    :program "isort"
    :args '("-")
    :lighter "Ist")

(reformatter-define rufo
    :program "rufo-emacs-wrapper"
    :lighter " Rufo")

(reformatter-define sqlformat
    :program "sqlformat"
    :args '("-r" "--indent_width" "4" "-a" "-s" "--wrap_after" "79"
            "-k" "upper" "-")
    :lighter " sqlfmt")

(reformatter-define yapf-format
    :program "yapf"
    :lighter "Yapf")


;;; replace

;; I get bothered when I can't do M-s o for `occur' in buffers using
;; `paredit-mode', which squats on M-s.
(bind-keys ("M-m s o" . occur))


;;; rg

(setq rg-ignore-case 'smart
      rg-keymap-prefix (kbd "M-m s r")
      rg-default-alias-fallback "everything"
      rg-hide-command nil)

(rg-enable-default-bindings)

(rg-define-search rg-everything
    "Search everything."
  :files "everything"
  :confirm prefix
  :menu ("Search" "e" "Everything"))

(with-eval-after-load 'rg
  (bind-keys :map rg-mode-map
             ("N" . next-error)
             ("P" . previous-error)))


;;; saveplace

(save-place-mode 1)


;;; scroll-bar-mode

(scroll-bar-mode -1)


;;; server

(server-start)


;;; sh-script

(defun my:sh-base-mode-hook ()
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
(my:add-hooks 'sh-base-mode-hook
  #'my:warn-white-space-mode
  #'smart-tabs-mode
  #'my:sh-base-mode-hook)

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

(bind-keys ("S-<escape>" . my:shackle-close-pop-up-windows))

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
;; Unbind the original binding, I don't want to be tempted by it in
;; `describe-bindings'.
(unbind-key "M-=")

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
  (with-eval-after-load 'lsp-mode
    ;; Register LSP server for Microsoft SQL Server.  (Configuration
    ;; in init-local.el.)
    (require 'lsp-mssql nil t))
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
                 "^[-[:alnum:]_]*[-'(][#>] ")
    (warn "sql.el changed PostgreSQL :prompt-cont-regexp, edit your init."))

  (sql-set-product-feature 'postgres :prompt-cont-regexp
                           "^[-[:alnum:]_]*[-'($\"][#>] ")

  ;; With readline on (or libedit on macOS?) things like
  ;; `sql-send-buffer' will sometimes apparently corrupt the data sent
  ;; to psql.
  (add-to-list 'sql-postgres-options "--no-readline")

  ;; PostgreSQL connections should ask for the port.
  (add-to-list 'sql-postgres-login-params 'port t)

  ;; Additional MSSQL keywords.
  (push (sql-font-lock-keywords-builder 'font-lock-type-face nil "bigint")
        sql-mode-ms-font-lock-keywords)
  (push (sql-font-lock-keywords-builder 'font-lock-builtin-face
                                        nil "sysdatetimeoffset")
        sql-mode-ms-font-lock-keywords))

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
  #'faux-indent-mode
  #'smart-tabs-mode
  #'my:sql-mode-hook)

(with-eval-after-load 'sql
  (my:load-recipes 'sql-mode-electric-apostrophe))

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

(defun my:sql-postgres-override-pspg-settings ()
  (when (eq sql-product 'postgres)
    ;; Using `sql-send-string' makes you get an extra prompt, which is
    ;; annoying.
    (sql-input-sender (get-buffer-process (current-buffer))
                      "\\pset border 1 \\pset linestyle ascii")))

(add-hook 'sql-login-hook #'my:sql-postgres-override-pspg-settings)


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
                        "table_name" "column_name" "strip"
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

(when (eq my:completion-framework 'ivy)
  (bind-keys ("s-s" . swiper-isearch)
             ("s-r" . swiper-isearch-backward))

  (setq swiper-goto-start-of-match t))


;;; terraform-mode

(defun my:terraform-mode-hook ()
  ;; `company-capf' here for LSP's sake.
  (setq-local company-backends '(company-capf
                                 (company-terraform company-dabbrev-code)
                                 company-files))
  ;; `company-tng-mode' disables LSP snippets by default.  You really
  ;; want these in Terraform.  Let's see if we can turn them on
  ;; locally...
  (setq-local lsp-enable-snippet t))

(my:add-hooks 'terraform-mode-hook
  #'terraform-format-on-save-mode
  #'my:terraform-mode-hook)

;; Configuration for terraform-ls, from
;; https://github.com/hashicorp/terraform-ls/blob/main/docs/USAGE.md.
(with-eval-after-load 'terraform-mode
  (require 'lsp-mode)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     '("terraform-ls" "serve"))
                    :major-modes '(terraform-mode)
                    :server-id 'terraform-ls))

  (setf (alist-get 'terraform-mode my:lsp-flycheck-extra-checkers-alist)
        '(terraform-tflint))

  ;; Disable Hashicorp's less feature-ful (their claim) LSP server
  ;; when the more feature-ful one is available.
  (when (executable-find "terraform-lsp")
    (cl-pushnew 'tfmls (alist-get 'terraform-mode lsp-disabled-clients))))


;;; tool-bar

(tool-bar-mode -1)


;;; treemacs

(setq ;; treemacs-python-executable "/opt/local/bin/python3"
 treemacs-width 50)

(defun my:treemacs-mode-hook ()
  (setq truncate-lines t))

(my:add-hooks 'treemacs-mode-hook
  #'my:treemacs-mode-hook)

;; (my:add-hooks 'treemacs-hook
;;   #'treemacs-follow-mode)

(bind-keys ("M-m u t" . treemacs))

(with-eval-after-load 'treemacs
  ;; Normal size says to use 22, or 44 if you have a high DPI display.
  ;; I have a high DPI display---but I don't think NS Emacs is able to
  ;; use it.  22 is too big, 44 is *way* too big!
  (treemacs-resize-icons 11))


;;; treesit

(setf (alist-get 'jsonnet treesit-language-source-alist)
      '("https://github.com/sourcegraph/tree-sitter-jsonnet"))


;;; treesit-auto

(require 'treesit-auto)

(setq treesit-auto-install 'prompt)

;; It turns out that `yaml-ts-mode' is built-in, and somehow awful?
;; It has no indentation settings as of 2023-11-25?
;; https://redd.it/17gtxmr
(setq treesit-auto-langs (seq-difference treesit-auto-langs
                                         '(yaml)))

(global-treesit-auto-mode 1)


;;; two-column

;; I keep activating this by accident, and I have never, ever wanted
;; to use this mode.  I have nothing but sympathy for people who must
;; write two-column plain text documents.
(if (eq (lookup-key global-map (kbd "<f2>")) '2C-command)
    (unbind-key "<f2>")
  (warn "<f2> is no longer `2C-command', update your init.el"))


;;; typescript-mode

(defun my:typescript-mode-hook ()
  (setq indent-tabs-mode nil))

(add-hook 'typescript-mode #'my:typescript-mode-hook)


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


;;; vc

;; Note that I set `vc-follow-symlinks' where I set up el-patch,
;; otherwise I think my init.el will trip the "follow symlink" prompt
;; numerous times, due to `el-patch-validate' calls.

(bind-keys ("M-m v g" . vc-annotate))

(my:load-recipes 'vc-use-icons-in-mode-line
                 'vc-truncate-long-branch-names)


;;; vcl-mode

(defun my:vcl-mode-hook ()
  (my:setq-local c-basic-offset 4))

(add-hook 'vcl-mode-hook #'my:vcl-mode-hook)


;;; vertico

;; Recipe to make / enter a directory, like Ivy did.  Installed later
;; with a key binding.
;; https://github.com/minad/vertico/wiki#make-vertico-and-vertico-directory-behave-more-like-ivyido
(defun my:vertico-magic-slash ()
  (interactive)
  (if (and (not (string-match-p "[/~:]$" (minibuffer-contents-no-properties)))
           (>= vertico--index 0)
           (eq 'file (vertico--metadata-get 'category))
           (file-directory-p (vertico--candidate)))
      (vertico-insert)
    (self-insert-command 1 ?/)))

(when (eq my:completion-framework 'vertico)
  (vertico-mode 1)
  (savehist-mode 1)

  (bind-keys :map vertico-map
             ;; Avy-like selection for vertico
             ("C-'" . vertico-quick-exit)
             ;; Backspace in a file prompt works like Ivy did.
             ("DEL" . vertico-directory-delete-char)
             ("/" . my:vertico-magic-slash))

  ;; This is also necessary to make file prompts work like in Ivy,
  ;; specifically my "magic slash".
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  ;; `vertico-repeat' is like `ivy-occur': repeat the last...
  ;; Vertico... thing, I guess.  It's probably not nearly 100%
  ;; equivalent.

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  (bind-keys ("<f6>" . vertico-repeat)))


;;; volatile-highlights

(volatile-highlights-mode 1)


;;; vterm

(setq vterm-max-scrollback 100000
      ;; vterm-kill-buffer-on-exit nil
      vterm-buffer-name-string "vterm: %s")

(defun my:vterm-quote-next (char)
  (interactive (list (let ((inhibit-quit t)) (read-char))))
  (let ((base-char (event-basic-type char))
        (modifiers (event-modifiers char)))
    (vterm-send-key (string base-char)
                    (memq 'shift modifiers)
                    (memq 'meta modifiers)
                    (memq 'control modifiers))))

(defun my:vterm-mode-hook ()
  (setq-local global-hl-line-mode nil))

(with-eval-after-load 'vterm
  (when (require 'eterm-256color nil t)
    ;; Private function of eterm-256color that checks that you have
    ;; the terminfo file for eterm-color installed in the right place.
    (eterm-256color-compile)
    (setq vterm-term-environment-variable "eterm-color"))
  (customize-set-variable 'vterm-keymap-exceptions
                          (cons "M-m" (seq-difference vterm-keymap-exceptions
                                                      '("C-l" "C-u"))))

  (bind-keys :map vterm-mode-map
             ("C-c C-c" . vterm-send-C-c)
             ("C-q" . my:vterm-quote-next)
             ("C-c C-t" . vterm-copy-mode))

  (add-hook 'vterm-mode-hook #'my:vterm-mode-hook))


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
        (my:buffer-local-value 'company-backends))
  ;; I have no idea how `web-mode-enable-auto-quoting' is supposed to
  ;; work: It wants to insert a space after it turns attr= into
  ;; attr="", *unless* the quotes it just inserted are followed by \n
  ;; or >.  The > makes sense if typing < inserts a >, a la electric
  ;; pairing.  Except that I don't see if/how/where web-mode does
  ;; that.  Sigh.  Just tell elec-pair to pair < with >.
  (add-to-list (make-local-variable 'electric-pair-pairs) '(?< . ?>)))

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

(with-eval-after-load 'webpaste
  (setq webpaste-provider-priority
        (cons "bpa.st" (remove "bpa.st" webpaste-provider-priority)))

  ;; I should upstream this.
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
                                        :foreground "unspecified"
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

(defun my:xref-find-definitions-or-references ()
  "Find definition of symbol at point, or references if at a definition."
  (interactive)
  (let* ((buffer (current-buffer))
         (pos (point))
         (res (call-interactively #'xref-find-definitions)))
    (if (and (equal (current-buffer) buffer)
             (= (point) pos))
        (call-interactively #'xref-find-references)
      res)))

;; Have to do this so xref doesn't force `xref-find-definitions' (and
;; maybe `xref-find-references' too) to prompt.
(with-eval-after-load 'xref
  (when (eq (car xref-prompt-for-identifier) 'not)
    (push 'my:xref-find-definitions-or-references
          (cdr xref-prompt-for-identifier))))

(keymap-global-set "<remap> <xref-find-definitions>"
                   #'my:xref-find-definitions-or-references)

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
           ("TAB" . nil))

(when (eq my:completion-framework 'ivy)
  (bind-keys ("M-m i s" . ivy-yasnippet)))

;; Also let hippie-expand expand snippets.
(with-eval-after-load 'hippie-exp
  (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand))

(with-eval-after-load 'which-key
  (add-to-list 'which-key-replacement-alist
               '(("C-c &" . "Prefix Command") . (nil . "yasnippet") )))


;;; zop-to-char

(bind-keys ("M-z" . zop-up-to-char))


;;; envrc

;; Per the suggestion of envrc's README, this comes very late.

(envrc-global-mode)


;;; Epilogue

;; Sometimes init.el doesn't get to load all the way to the end, but
;; errors get swallowed and don't appear in *Messages*?  WTF.
(message "init.el loaded successfully")
