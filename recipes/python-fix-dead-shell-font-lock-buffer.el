;; -*- lexical-binding: t; -*-

;; For some reason I keep finding `python-shell--font-lock-buffer'
;; getting set to a killed buffer, in which case you'll get "Selecting
;; deleted buffer" errors in the *weirdest* places, definitely in a
;; Python shell buffer, but also from, say, `find-file'.  I have no
;; idea why these buffers are getting killed.  (Carousel?  But it's
;; not telling me that it's killing some " *foo-font-lock*" buffer.)
;;
;; This advises the complete set of functions that call macro
;; `python-shell-font-lock-with-font-lock-buffer' as of this writing.
;; These are the fewest points I could see clear to advising, because
;; I'm not going to try and advise some macros.
;;
;; Alternative fix might be to add some hook(s) to make sure
;; `python-shell--font-lock-buffer' is live, and make sure they get
;; inserted before the relevant python-mode hooks.

(defvar python-shell--font-lock-buffer)
(declare-function python-shell-font-lock-turn-on "python")

(defun my:python-shell-ensure-font-lock-buffer-is-live (&rest _)
  (unless (or (null python-shell--font-lock-buffer)
              (buffer-live-p python-shell--font-lock-buffer))
    ;; This should set `python-shell--font-lock-buffer' to nil, which
    ;; will cause `python-shell-font-lock-with-font-lock-buffer' to
    ;; create a new buffer for python.el's font-locking needs.
    (python-shell-font-lock-turn-on)))

(dolist (func '(python-shell-font-lock-cleanup-buffer
                python-shell-font-lock-comint-output-filter-function
                python-shell-font-lock-post-command-hook))
  (advice-add func :before #'my:python-shell-ensure-font-lock-buffer-is-live))
