;; -*- lexical-binding: t; -*-

;; Ivy already installs its own ivy-switch-buffer.
;; `ivy--recompute-index' has to treat it special so that, when you
;; start typing, the match selected becomes the match at the top,
;; which is generally what people expect.  (It's what I expect, and I
;; assume abo-abo agrees since he added this code to Ivy.)
;;
;; Ivy also installs `ivy-switch-buffer-other-window', but it fails to
;; give this special treatment in `ivy--recompute-index', so I do it
;; here with advice.
;;
;; Ivy does not supply something like `switch-to-buffer-other-frame',
;; so I cribbed code from Ivy to create
;; `my:ivy-switch-to-buffer-other-frame' here.  Then I give it the
;; same special index treatment I have described above.

(defvar my:ivy-additional-switch-buffer-commands
  '(ivy-switch-buffer-other-window my:ivy-switch-to-buffer-other-frame)
  "List of commands that should behave like `ivy-switch-buffer'.
Note that you can only put Ivy-aware commands here, as the advice
depends on the :caller key being available.")

;; Might as well have all buffer commands sort the same way, too.
(dolist (buffer-cmd my:ivy-additional-switch-buffer-commands)
  (setf (alist-get buffer-cmd ivy-sort-matches-functions-alist)
        #'ivy-sort-function-buffer))

;; Note that I tried to do this using `ivy-index-functions-alist'
;; instead of advice, but it doesn't seem to be possible.  (Which
;; explains `ivy--recompute-index' already having a special case for
;; `ivy-switch-buffer'.)
(define-advice ivy--recompute-index
    (:around (orig-fun name &rest args) my:ivy-fix-index-for-buffer-commands)
  (if (and (> (length name) 0)
           (memq (ivy-state-caller ivy-last)
                 my:ivy-additional-switch-buffer-commands))
      (setq ivy--index 0)
    (apply orig-fun name args)))

(defun my:ivy-switch-to-buffer-other-frame ()
  "Switch to another buffer in another frame."
  (interactive)
  (ivy-read "Switch to buffer in other frame: " 'internal-complete-buffer
            :matcher #'ivy--switch-buffer-matcher
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'switch-to-buffer-other-frame
            :keymap ivy-switch-buffer-map
            :caller 'my:ivy-switch-to-buffer-other-frame))

(define-key ivy-mode-map [remap switch-to-buffer-other-frame]
  #'my:ivy-switch-to-buffer-other-frame)
