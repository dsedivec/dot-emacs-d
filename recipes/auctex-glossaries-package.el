;; -*- lexical-binding: t; -*-
;;
;; This teaches AUCTeX about a few commands from the glossaries
;; package.

(require 'latex)

(defun my:LaTeX-arg-glossary-entry (optional &rest _args)
  (assert (not optional))
  (defvar exit-mark)
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
