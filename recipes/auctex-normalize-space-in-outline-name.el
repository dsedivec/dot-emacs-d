;; -*- lexical-binding: t; -*-

;; This fixes a problem where you've got wrapped text in
;; e.g. \section, and imenu-list (at least imenu-list, maybe regular
;; imenu as well) breaks and starts moving point to the following
;; heading anytime you click on one of its headings.  It does not like
;; newline in the middle of a \section{} command's argument.

(require 'subr-x)

(defun my:LaTeX-normalize-space-in-outline-name (name)
  (replace-regexp-in-string "[[:space:]\n]+" " " (string-trim name)))

(advice-add 'LaTeX-outline-name :filter-return
            #'my:LaTeX-normalize-space-in-outline-name)
