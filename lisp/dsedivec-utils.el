;;; utils.el --- Utility functions for my init files  -*- lexical-binding: t; -*-

(require 'seq)
(require 'repeat)

(require 'treepy)

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
         (setf (cdr last-cell) (list new-element)))
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
         (derived-mode-p 'bm-show-mode
                         'bs-mode
                         'compilation-mode
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

(defmacro my:key-binding-with-modes-off (modes &optional command-keys-vector)
  "Return the original binding of some command keys with all MODES off.

Command keys vector defaults to the return value from
`this-command-keys-vector', which should be the keys used to run
the current command.

I use this to find out what a key would be bound to with one or
more minor modes turned off."
  `(let (,@(mapcar (lambda (mode) (list mode nil)) modes))
     (key-binding ,(or command-keys-vector '(this-command-keys-vector)))))

(defun my:insert-time-stamp ()
  (interactive)
  (insert (format-time-string "%FT%T%:::z")))

;; `benchmark-run' insists on an integer, but `benchmark-call' will
;; happily take a float (see its docstring).  This is
;; `benchmark-run' with the conditional modified to accept a float.  I
;; should upstream this.
;;
;; I also modify the return value to add the average time per run as
;; the first element of the returned list when REPETITIONS is a float.
;; That I should maybe not upstream.
(defmacro my:benchmark-run (&optional repetitions &rest forms)
  "Time execution of FORMS.
If REPETITIONS is an integer, run FORMS that many times,
accounting for the overhead of the resulting loop.  If
REPETITIONS is a floating point number, run for at least that
many seconds.  Otherwise run FORMS once.

See `benchmark-call' for more information."
  (declare (indent 1) (debug t))
  (unless (or (and (numberp repetitions) (not (cl-minusp repetitions)))
              (and repetitions (symbolp repetitions)))
    (setq forms (cons repetitions forms)
          repetitions 1))
  (let ((s-reps (gensym))
        (s-result (gensym)))
    `(let* ((,s-reps ,repetitions)
            (,s-result (benchmark-call (lambda () ,@forms) ,s-reps)))
       (if (floatp ,s-reps)
           (cons (/ (cadr ,s-result) (car ,s-result)) ,s-result)
         ,s-result))))


;; Utilities to edit the mode line using treepy zippers.

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

(provide 'dsedivec-utils)
