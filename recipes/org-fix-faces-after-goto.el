;; -*- lexical-binding: t; -*-

;; Currently using org-goto forgets my the org-todo-keyword-faces
;; value I have set in some buffers.  This is a hack for now to
;; restore the faces after using org-goto.
;;
;; Still needed as of 9.1.14-923-g4c92e9.
;;
;; I originally implemented this as advice, but adding
;; font-lock-fontify-buffer directly as an :after advice on org-goto
;; resulted in the wrong (interactive) form being used to determine
;; the args to org-goto.  Advising interactive functions can be hard,
;; see http://stackoverflow.com/a/14608394/2305480.  However, a [remap
;; org-goto] in the keymap wasn't respected by speed commands (press
;; "j" at left margin).  So now we're back to advice and I copied the
;; interactive form.  It's possible I should go back to [remap] and
;; then explicit remap "j" in org-speed-commands-user to my:org-goto.
(define-advice org-goto
    (:around (orig-fun &rest args) my:fix-font-lock-after-org-goto)
  (interactive "P")
  (prog1
      (cond ((called-interactively-p 'any)
             ;; I am assuming that any interactive args will
             ;; automatically be sent to org-goto by
             ;; call-interactively.
             (call-interactively orig-fun))
            (t
             (apply orig-fun args)))
    ;; Note that font-lock-ensure didn't work here. font-lock-flush
    ;; seems to be more of a "force re-fontification" kind of
    ;; function.
    (font-lock-flush)))
