;; -*- lexical-binding: t; -*-

(defvar my:bs-file-name-abbrev-alist nil
  "List of (REGEXP . REPLACEMENT) for file names shown by bs.")

(defun my:bs-get-abbreviated-file-name (&rest args)
  "Call `bs--get-file-name' then abbreviate.
Abbreviations are taken from `my:bs-file-name-abbrev-alist'."
  (let ((name (apply #'bs--get-file-name args)))
    (if (stringp name)
        (apply #'propertize
               (or (seq-some (lambda (pair)
                               (when (string-match (car pair) name)
                                 (replace-match (cdr pair) nil nil name)))
                             my:bs-file-name-abbrev-alist)
                   (abbreviate-file-name name))
               (text-properties-at 0 name))
      name)))

(with-eval-after-load 'bs
  (let ((file-col (assoc "File" bs-attributes-list)))
    (if file-col
        (setf (car (last file-col)) 'my:bs-get-abbreviated-file-name)
      (warn "Couldn't find (and change) \"File\" in `bs-attributes-list'"))))
