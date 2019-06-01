;; -*- lexical-binding: t; -*-

;; This introduces a command `my:python-add-import' that tries to
;; import whatever you tell it to at the end of the import list at the
;; top of your current file.  It makes nearly no efforts to make sure
;; your import is syntactically correct, nor that the import is not a
;; duplicate, nor does it make any attempts to coalesce multiple
;; imports from the same module into a single import statement.  You
;; could hook up isort to `my:python-add-import-hook' to do that sort
;; of work for you.

(defun my:python-search-import-statement ()
  (let ((case-fold-search nil)
        (start (point)))
    (catch 'done
      (while (re-search-forward "\\(?2:^\\|;\\)\\s-*\\(?1:from\\|import\\)\\_>"
                                nil t)
        (unless (or
                 ;; In a string or comment
                 (nth 8 (syntax-ppss))
                 ;; In parentheses (I can't see how you could have a
                 ;; syntactically-correct "import" statement inside
                 ;; parentheses)
                 (nth 9 (syntax-ppss))
                 ;; At the second line in "from x\\\nimport y"
                 (and (string= (match-string 1) "import")
                      (string= (match-string 2) "")
                      (eql (char-before (line-end-position 0)) ?\\)))
          (throw 'done t)))
      ;; Found nothing, go back to where we started and return nil.
      (goto-char start)
      nil)))

;; This function improves upon `python-nav-forward-statement' by being
;; able to find the end of a statement on the same line.  For example,
;; a line like "foo; bar" will be treated as if it has two statements.
;; (Because, as far as I'm aware, it does have two statements, in
;; terms of Python grammar.)
;;
;; This function moves to the last non-space character of the
;; statement, but before a semicolon, new line, or comment.  Arguably
;; we should move just *past* a semicolon or new line, to behave more
;; like something like `forward-sexp'.
(defun my:python-forward-statement ()
  ;; Get out of any comment or string we're inside.
  (let ((state (syntax-ppss)))
    (cond
      ((nth 3 state)
       (goto-char (nth 8 state))
       (forward-sexp))
      ((nth 8 state)
       ;; Must be in a comment.
       (python-util-forward-comment))))
  ;; Get out of any parentheses we're inside.
  (let ((state (syntax-ppss)))
    (when (nth 9 state)
      (goto-char (car (last (nth 9 state)))))
    (forward-sexp))
  (when (re-search-forward "[;\n#]" nil t)
    (goto-char (match-beginning 0))
    (let ((state (syntax-ppss)))
      (when (or (nth 8 state) (nth 9 state))
        ;; We moved back into a comment or string, so do all of the
        ;; above again.
        (my:python-forward-statement))))
  (skip-syntax-backward "-"))

(defun my:python-find-imports-in-all-buffers ()
  (let ((case-fold-search nil)
        imports)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'python-mode)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (while (my:python-search-import-statement)
                (let ((import-start (match-beginning 1)))
                  ;; This is like `ignore-errors-unless-debug', which
                  ;; doesn't exist.
                  (condition-case-unless-debug _
                      (progn
                        (my:python-forward-statement)
                        (cl-pushnew (buffer-substring-no-properties import-start
                                                                    (point))
                                    imports
                                    :test 'equal))
                    (error)))))))))
    imports))

(defun my:python-skip-spaces-strings-comments ()
  (while (cond
           ((> (skip-chars-forward "[[:space:]]\n;") 0))
           ((looking-at-p "[rfbuRFBU]*[\"']")
            (forward-sexp)
            t)
           ((= (or (char-after) 0) ?#)
            (python-util-forward-comment)
            t))))

(defvar my:python-add-import-history nil)

(defvar my:python-add-import-hook nil
  "Functions to be run after adding an import.")

(defun my:python-add-import (import)
  (interactive
   (list
    (let (defaults)
      (save-excursion
        (skip-chars-backward "[[:space:]].")
        (let* ((start (point))
               (name (progn
                       (skip-chars-backward "[:word:]_.")
                       (buffer-substring-no-properties (point) start))))
          (while (not (zerop (length name)))
            (push (concat "import " name) defaults)
            (setq name
                  (substring name 0
                             (or (string-match-p "\\(^\\|\\.\\)[[:word:]_]+$"
                                                 name)
                                 0))))))
      (completing-read "Import: "
                       (my:python-find-imports-in-all-buffers)
                       nil
                       nil
                       nil
                       'my:python-add-import-history
                       defaults))))
  (when (symbolp import)
    (setq import (symbol-name import)))
  (unless (and (stringp import) (> (length import) 0))
    (user-error "Invalid import to add: %S" import))
  (unless (string-match-p "^\\(?:from\\|import\\)\\s-" import)
    (if (string-match-p "\\_<import\\_>" import)
        (setq import (concat "from " import))
      (setq import (concat "import " import))))
  (save-excursion
    (goto-char (point-min))
    (if (my:python-search-import-statement)
        (let ((last-import (point)))
          (while (progn
                   (my:python-forward-statement)
                   (my:python-skip-spaces-strings-comments)
                   (looking-at-p "\\(?:import\\|from\\)\\_>"))
            (setq last-import (point)))
          (goto-char last-import)
          (my:python-forward-statement)
          (skip-syntax-forward "-")
          (cond
            ((= (char-after) ?#)
             (python-util-forward-comment)
             (forward-line 0)
             (cl-assert (let ((state (syntax-ppss)))
                          (not (or (nth 8 state) (nth 9 state)))))
             (insert import "\n"))
            ((= (char-after) ?\;)
             (insert "; " import))
            ((= (char-after) ?\n)
             (insert "\n" import))
            (t
             (error "Can't figure out how to insert your import (at %d)"
                    (point)))))
      (my:python-skip-spaces-strings-comments)
      (unless (bolp)
        (insert "\n")
        (delete-horizontal-space))
      (insert import "\n")))
  (run-hooks 'my:python-add-import-hook))
