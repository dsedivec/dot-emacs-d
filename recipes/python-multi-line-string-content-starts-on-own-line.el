;; -*- lexical-binding: t; -*-

;; Hopefully better string wrapping.  See the comments in the
;; `el-patch-swap' call below.

(require 'el-patch)

(el-patch-feature python)

(with-eval-after-load 'python
  (el-patch-defun python-fill-string (&optional justify)
    "String fill function for `python-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
    (let* ((str-start-pos
            (set-marker
             (make-marker)
             (or (python-syntax-context 'string)
                 (and (equal (string-to-syntax "|")
                             (syntax-after (point)))
                      (point)))))
           (num-quotes (python-syntax-count-quotes
                        (char-after str-start-pos) str-start-pos))
           (el-patch-remove (str-line-start-pos
                             (save-excursion
                               (goto-char str-start-pos)
                               (beginning-of-line)
                               (point-marker))))
           (str-end-pos
            (save-excursion
              (goto-char (+ str-start-pos num-quotes))
              (or (re-search-forward (rx (syntax string-delimiter)) nil t)
                  (goto-char (point-max)))
              (point-marker)))
           (multi-line-p
            ;; Docstring styles may vary for one-liners and multi-liners.
            (> (count-matches "\n" str-start-pos str-end-pos) 0))
           (delimiters-style
            (pcase python-fill-docstring-style
              ;; delimiters-style is a cons cell with the form
              ;; (START-NEWLINES .  END-NEWLINES). When any of the sexps
              ;; is NIL means to not add any newlines for start or end
              ;; of docstring.  See `python-fill-docstring-style' for a
              ;; graphic idea of each style.
              ('django (cons 1 1))
              ('onetwo (and multi-line-p (cons 1 2)))
              ('pep-257 (and multi-line-p (cons nil 2)))
              ('pep-257-nn (and multi-line-p (cons nil 1)))
              ('symmetric (and multi-line-p (cons 1 1)))))
           (fill-paragraph-function))
      (save-restriction
        (narrow-to-region (el-patch-swap
                            str-line-start-pos
                            (save-excursion
                              (goto-char (+ str-start-pos num-quotes))
                              ;; If quote is followed by nothing but
                              ;; continuation character and white
                              ;; space on its line, then narrow
                              ;; starting at the following line, so we
                              ;; don't tuck the start of string
                              ;; content onto the same line as its
                              ;; opening quote.  Example:
                              ;;
                              ;;     def f():
                              ;;         x = '''
                              ;;             wrap me!
                              ;;         '''
                              (forward-line
                               (if (looking-at-p "\\s-*\\\\?\\s-*$")
                                   1
                                 0))
                              ;; Always going to give line beginning
                              ;; position so that the wrapping is indented
                              ;; in line with the surrounding code.
                              (point)))
                          str-end-pos)
        (fill-paragraph justify))
      (save-excursion
        (when (and (python-info-docstring-p) python-fill-docstring-style)
          ;; Add the number of newlines indicated by the selected style
          ;; at the start of the docstring.
          (goto-char (+ str-start-pos num-quotes))
          (delete-region (point) (progn
                                   (skip-syntax-forward "> ")
                                   (point)))
          (and (car delimiters-style)
               (or (newline (car delimiters-style)) t)
               ;; Indent only if a newline is added.
               (indent-according-to-mode))
          ;; Add the number of newlines indicated by the selected style
          ;; at the end of the docstring.
          (goto-char (if (not (= str-end-pos (point-max)))
                         (- str-end-pos num-quotes)
                       str-end-pos))
          (delete-region (point) (progn
                                   (skip-syntax-backward "> ")
                                   (point)))
          (and (cdr delimiters-style)
               ;; Add newlines only if string ends.
               (not (= str-end-pos (point-max)))
               (or (newline (cdr delimiters-style)) t)
               ;; Again indent only if a newline is added.
               (indent-according-to-mode))))) t)

  (el-patch-validate 'python-fill-string 'defun t))
