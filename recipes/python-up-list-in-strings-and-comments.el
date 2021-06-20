;; -*- lexical-binding: t; -*-

;; Make up-list commands work in strings and comments
;;
;; We just skip forward over the comment before doing what you asked,
;; which seems like what happens in lisp modes (and so it is what I
;; expect).

(require 'el-patch)

(el-patch-feature python)

(with-eval-after-load 'python
  (el-patch-defun python-nav--up-list (&optional dir)
    "Internal implementation of `python-nav-up-list'.
DIR is always 1 or -1 and comes sanitized from
`python-nav-up-list' calls."
    (let ((context (python-syntax-context-type))
          (forward-p (> dir 0)))
      (el-patch-add
        (when (eq context 'comment)
          (python-util-forward-comment)
          (setq context (python-syntax-context-type))))
      (cond
        (el-patch-remove ((memq context '(string comment))))
        (el-patch-add
          ((eq context 'comment))
          ((eq context 'string)
           (let ((forward-sexp-function))
             (up-list dir t t))))
        ((eq context 'paren)
         (let ((forward-sexp-function))
           (up-list dir)))
        ((and forward-p (python-info-end-of-block-p))
         (let ((parent-end-pos
                (save-excursion
                  (let ((indentation (and
                                      (python-nav-beginning-of-block)
                                      (current-indentation))))
                    (while (and indentation
                                (> indentation 0)
                                (>= (current-indentation) indentation)
                                (python-nav-backward-block)))
                    (python-nav-end-of-block)))))
           (and (> (or parent-end-pos (point)) (point))
                (goto-char parent-end-pos))))
        (forward-p (python-nav-end-of-block))
        ((and (not forward-p)
              (> (current-indentation) 0)
              (python-info-beginning-of-block-p))
         (let ((prev-block-pos
                (save-excursion
                  (let ((indentation (current-indentation)))
                    (while (and (python-nav-backward-block)
                                (>= (current-indentation) indentation))))
                  (point))))
           (and (> (point) prev-block-pos)
                (goto-char prev-block-pos))))
        ((not forward-p) (python-nav-beginning-of-block)))))

  (el-patch-validate 'python-nav--up-list 'defun t))
