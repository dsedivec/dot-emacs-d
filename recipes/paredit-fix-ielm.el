;; -*- lexical-binding: t; -*-
;;
;; Paredit 25 (2022-11-25) started binding RET and C-j to, apparently,
;; work around some unwanted interactions with `electric-indent-mode'.
;; As documented at https://paredit.org/cgit/paredit/plain/NEWS,
;; though, "The Electric Indent Mode workaround turns out to break
;; ielm and other interactive modes, because paredit now defines RET,
;; overriding the binding in interactive modes that submits an input."
;; Very true!  IELM is now broken, and thus I am also broken.  We must
;; repair this situation.  Attempt to call the original bindings of
;; whatever key invoked `paredit-RET'.  (I don't need to do the same
;; for `paredit-C-j' right now, but I could.)
;;
;; Alternatives:
;;
;; * Do this one on mode activation.  But what if modes change after
;;   that?  Dangerous, it seems to me.
;;
;; * Hack in explicit support for some modes, or set up some variable
;;   so they can communicate to paredit not to override RET and such.

(with-eval-after-load 'paredit
  (require 'dsedivec-utils)

  (el-patch-defun paredit-RET ()
    "Default key binding for RET in Paredit Mode.
Normally, inserts a newline, like traditional Emacs RET.
With Electric Indent Mode enabled, inserts a newline and indents
  the new line, as well as any subexpressions of it on subsequent
  lines; see `paredit-newline' for details and examples."
    (interactive)
    (el-patch-wrap 2 0
      (let ((orig-binding (my:key-binding-with-modes-off (paredit-mode))))
        (el-patch-wrap 2 1
          (if (memq orig-binding '(newline))
              (if (paredit-electric-indent-mode-p)
                  (let ((electric-indent-mode nil))
                    (paredit-newline))
                (newline))
            (call-interactively orig-binding))))))

  (el-patch-validate 'paredit-RET 'defun t))
