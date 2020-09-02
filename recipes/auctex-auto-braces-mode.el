;; -*- lexical-binding: t; -*-

;; `my:LaTeX-auto-add-braces-mode' is a convenience for when I define
;; some vocabulary within a document, such as
;;
;;     \newcommand*{\pmgr}[0]{program manager}
;;
;; and I want to be able to type \pmgr SPC and have it transformed
;; automatically to \pmgr{}.
;;
;; (If you know some better feature of LaTeX that I should be using
;; for this, please let me know.)

;; In the below list, "slash" is my own preference, but all the rest
;; are stolen from chktex's chktexrc "Silent" macro list.
(defvar my:LaTeX-auto-add-braces-excluded-macro-regexp
  (rx string-start
      (| "slash"
         "rm" "em" "bf" "it" "sl" "sf" "sc" "tt" "selectfont"
         "rmfamily" "sffamily" "ttfamily" "mdseries" "bfseries"
         "slshape" "scshape" "relax" "vskip" "pagebreak"
         "nopagebreak" "textrm" "textem" "textbf" "textit" "textsl"
         "textsf" "textsc" "texttt" "clearpage" "ddots" "dotfill"
         "flushbottom" "fussy" "indent" "linebreak" "onecolumn"
         "pagebreak" "pushtabs" "poptabs" "scriptsize" "sloppy"
         "twocolumn" "vdots" "today" "kill" "newline" "thicklines"
         "thinlines" "columnsep" "space" "item" "tiny" "footnotesize"
         "small" "normalsize" "normal" "large" "Large" "LARGE" "huge"
         "Huge" "printindex" "newpage" "listoffigures" "listoftables"
         "tableofcontents" "maketitle" "makeindex" "hline" "hrule"
         "vrule" "centering" "bigskip" "medskip" "smallskip"
         "noindent" "expandafter" "makeatletter" "makeatother"
         "columnseprule" "textwidth" "textheight" "hsize" "vsize"
         "if" "fi" "else" "csname" "endcsname" "z@" "p@" "@warning"
         "typeout" "dots" "ldots" "input" "endinput" "nextline"
         "leavevmode" "cdots" "appendix" "listfiles" "and" "quad"
         "hskip" "vfill" "vfil" "hfill" "hfil" "topmargin"
         "oddsidemargin" "frenchspacing" "nonfrenchspacing"
         "begingroup" "endgroup" "par" "vrefwarning" "upshape"
         "headheight" "headsep" "hoffset" "voffset" "cdot" "qquad"
         "left" "right" "qedhere" "xspace" "addlinespace" "cr" "fill"
         "frontmatter" "toprule" "midrule" "bottomrule")
      string-end)
  "Macros matching this regexp will not have {} added to them automatically.")

(defun my:LaTeX-auto-add-braces-to-macro ()
  "Possibly add {} to the command before point."
  (let ((ch (char-before)))
    (when (or (eq ch ?\n) (memq (char-syntax ch) '(?\. ?\s)))
      (save-excursion
        (backward-char)
        (let* ((end (point))
               (start (+ end (skip-syntax-backward "w"))))
          (when (and (< start end)
                     (eq (char-before) ?\\)
                     (not (string-match-p my:LaTeX-auto-add-braces-excluded-macro-regexp
                                          (buffer-substring start end))))
            (goto-char end)
            (insert "{}")))))))

(define-minor-mode my:LaTeX-auto-add-braces-mode
    "Automatically add braces after a macro with none."
  :lighter "{}"
  (if my:LaTeX-auto-add-braces-mode
      (add-hook 'post-self-insert-hook #'my:LaTeX-auto-add-braces-to-macro
                nil t)
    (remove-hook 'post-self-insert-hook #'my:LaTeX-auto-add-braces-to-macro)))
