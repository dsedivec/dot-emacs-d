;; -*- lexical-binding: t; -*-

;; Time was, `el-patch-validate' would leave buffers around for the
;; things (e.g. functions, variables, macros) you were validating.
;; This was annoying because I'd end up with a bunch of buffers I
;; didn't want after Emacs startup (where I use "a lot" of el-patch
;; and `el-patch-validate' pretty religiously immediately afterwards).
;;
;; Then the "templates" feature landed
;; (https://github.com/raxod502/el-patch/pull/52) which made
;; `el-patch-wrap-locator' kill the buffers that el-patch opened to
;; get the source of the thing it was patching.  Now I have the
;; opposite problem: if I patch something in an Elisp file that I
;; already have open, `el-patch-validate' ends up killing that buffer.
;; The code below attempts to make el-patch only kill buffers that it
;; opened itself.
;;
;; I really wish there was just a way to tell the stuff in find-func
;; not to use any existing buffers at all.  Life would be easier.
;;
;; Also, the humor, and potential risks, of patching el-patch and the
;; libraries it uses with el-patch itself is not lost on me.

(require 'el-patch)

;; First we're going to patch `find-function-search-for-symbol' to
;; `save-excursion' before it locates the definition of SYMBOL in the
;; buffer for LIBRARY.  Without this, point will be moved.

(el-patch-feature find-func)

(el-patch-defun find-function-search-for-symbol (symbol type library)
  "Search for SYMBOL's definition of type TYPE in LIBRARY.
Visit the library in a buffer, and return a cons cell (BUFFER . POSITION),
or just (BUFFER . nil) if the definition can't be found in the file.

If TYPE is nil, look for a function definition,
otherwise, TYPE specifies the kind of definition.
If SYMBOL has a property `definition-type',
the property value is used instead of TYPE.
TYPE is interpreted via `find-function-regexp-alist'.

The search is done in the source for library LIBRARY."
  (if (null library)
      (error "Don't know where `%s' is defined" symbol))
  ;; Some functions are defined as part of the construct
  ;; that defines something else.
  (while (and (symbolp symbol) (get symbol 'definition-name))
    (setq symbol (get symbol 'definition-name)))
  (setq type (or (get symbol 'definition-type)
                 type))
  (if (string-match "\\`src/\\(.*\\.\\(c\\|m\\)\\)\\'" library)
      (find-function-C-source symbol (match-string 1 library) type)
    (when (string-match "\\.el\\(c\\)\\'" library)
      (setq library (substring library 0 (match-beginning 1))))
    ;; Strip extension from .emacs.el to make sure symbol is searched in
    ;; .emacs too.
    (when (string-match "\\.emacs\\(.el\\)\\'" library)
      (setq library (substring library 0 (match-beginning 1))))
    (let* ((filename (find-library-name library))
	   (regexp-symbol (cdr (assq type find-function-regexp-alist))))
      (with-current-buffer (find-file-noselect filename)
	(let ((regexp (if (functionp regexp-symbol) regexp-symbol
                        (format (symbol-value regexp-symbol)
                                ;; Entry for ` (backquote) macro in loaddefs.el,
                                ;; (defalias (quote \`)..., has a \ but
                                ;; (symbol-name symbol) doesn't.  Add an
                                ;; optional \ to catch this.
                                (concat "\\\\?"
                                        (regexp-quote (symbol-name symbol))))))
	      (case-fold-search))
          (el-patch-wrap 1 0
            (save-excursion
              (save-restriction
                (widen)
                (with-syntax-table emacs-lisp-mode-syntax-table
                  (goto-char (point-min))
                  (if (if (functionp regexp)
                          (funcall regexp symbol)
                        (or (re-search-forward regexp nil t)
                            ;; `regexp' matches definitions using known forms
                            ;; like `defun', or `defvar'.  But some
                            ;; functions/variables are defined using special
                            ;; macros (or functions), so if `regexp' can't find
                            ;; the definition, we look for something of the
                            ;; form "(SOMETHING <symbol> ...)".  This fails to
                            ;; distinguish function definitions from variable
                            ;; declarations (or even uses thereof), but is a
                            ;; good pragmatic fallback.
                            (re-search-forward
                             (concat "^([^ ]+" find-function-space-re "['(]?"
                                     (regexp-quote (symbol-name symbol))
                                     "\\_>")
                             nil t)))
                      (progn
                        (beginning-of-line)
                        (cons (current-buffer) (point)))
                    ;; If the regexp search didn't find the location of
                    ;; the symbol (for example, because it is generated by
                    ;; a macro), try a slightly more expensive search that
                    ;; expands macros until it finds the symbol.
                    (cons (current-buffer)
                          (find-function--search-by-expanding-macros
                           (current-buffer) symbol type))))))))))))

(el-patch-validate 'find-function-search-for-symbol 'defun t)

;; Now we modify the `el-patch-wrap-locator' macro to only kill a
;; buffer if it didn't exist before el-patch called
;; `find-function-noselect' or whatever, meaning only kill buffers
;; that el-patch creates.

(el-patch-feature el-patch)

(el-patch-defmacro el-patch-wrap-locator (&rest body)
  "Wrap the operation of `find-function-noselect' or similar.
This disables local variables and messaging, saves the current
buffer and point, etc. BODY is executed within this context. It
is assumed that BODY finds the appropriate file in a buffer using
`get-file-buffer', and then returns a cons cell with the buffer
and point for the beginning of some Lisp form. The return value
is the Lisp form, read from the buffer at point."
  (declare (indent 0))
  `(let* (;; Since Emacs actually opens the source file in a (hidden)
          ;; buffer, it can try to apply local variables, which might
          ;; result in an annoying interactive prompt. Let's disable
          ;; that.
          (enable-local-variables nil)
          (enable-dir-local-variables nil)
          ;; This is supposed to be noninteractive so we also suppress
          ;; all the messages. This has the side effect of masking all
          ;; debugging messages (you can use `insert' instead, or
          ;; temporarily remove these bindings), but there are just so
          ;; many different messages that can happen for various
          ;; reasons and I haven't found any other standard way to
          ;; suppress them.
          (inhibit-message t)
          (message-log-max nil)
          (el-patch-add
            (existing-buffers (buffer-list)))
          ;; Now we actually execute BODY to move point to the right
          ;; file and location.
          (buffer-point (save-excursion
                          ;; This horrifying bit of hackery on
                          ;; `get-file-buffer' prevents
                          ;; `find-function-noselect' from returning
                          ;; an existing buffer, so that later on when
                          ;; we jump to the definition, we don't
                          ;; temporarily scroll the window if the
                          ;; definition happens to be in the *current*
                          ;; buffer.
                          ;;
                          ;; Note from your patcher: this doesn't
                          ;; work.  Hypothesis on why it doesn't work:
                          ;; because `find-file-noselect' goes on to
                          ;; call `find-buffer-visiting' to, "Find any
                          ;; buffer for a file that has same
                          ;; truename."  Let's just not bother with
                          ;; this hack, then.
                          (el-patch-splice 2 0
                            (cl-letf (((symbol-function #'get-file-buffer)
                                       #'ignore))
                              ;; Because we get an error if the function
                              ;; doesn't have a definition anywhere.
                              (ignore-errors
                                ,@body)))))
          (defun-buffer (car buffer-point))
          (defun-point (cdr buffer-point)))
     (prog1 (and defun-buffer
                 defun-point
                 (with-current-buffer defun-buffer
                   (save-excursion
                     (goto-char defun-point)
                     (read (current-buffer)))))
       (when (el-patch-wrap 1 1
               (and defun-buffer (not (memq defun-buffer existing-buffers))))
         (kill-buffer defun-buffer)))))

(el-patch-validate 'el-patch-wrap-locator 'defmacro t)

;; Now, per the el-patch README, "Patching defmacro, defsubst, and
;; defconst forms will not affect usages of them in already-defined
;; functions, due to macroexpansion and byte-compilation. You may need
;; to define no-op patches of client functions to get your changes to
;; show up."  Not pretty, but it works.  Humorously, I could consider
;; using el-patch templates instead, so I don't have to copy/paste
;; these definitions.

(el-patch-defun el-patch-locate-variable (definition)
  "Return the source code of DEFINITION.
DEFINITION is a list starting with `defvar' or similar."
  (el-patch-wrap-locator
    (find-variable-noselect (nth 1 definition))))

(el-patch-validate 'el-patch-locate-variable 'defun t)

(el-patch-defun el-patch-locate-function (definition)
  "Return the source code of DEFINITION.
DEFINITION is a list starting with `defun' or similar."
  (el-patch-wrap-locator
    (find-function-noselect (nth 1 definition) 'lisp-only)))

(el-patch-validate 'el-patch-locate-function 'defun t)
