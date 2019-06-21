;; -*- lexical-binding: t; -*-

;; Adds command `my:company-complete-or-other-backend' which can be
;; bound to a key to mean, "accept the current candidate if there is
;; one, go to next backend if completion is in progress but no
;; selected candidate, or else start completion."

(defun my:company-complete-or-other-backend (&optional force-other)
  "Start completion, go to next backend, or complete selected candidate.

If FORCE-OTHER is non-nil, always run `company-other-backend'.

Note that, despite its name, `company-other-backend' will try the
first backend in `company-backends' if you're not in the middle
of a completion, so this command is a suitable replacement for a
`company-complete' key binding."
  (interactive "P")
  (if (and (not force-other)
           company-candidates
           (or company-selection-changed
               (string= (company-grab-symbol) (car company-candidates))))
      (company-complete-selection)
    (company-other-backend)))

(defun my:company-complete-or-other-is-company-cmd (orig-fun &rest args)
  "Advice to make `my:company-complete-or-other-backend'
  recognized by `company--company-command-p', so that company-tng
  believes it is a company command and doesn't try to end
  completion before executing
  `my:company-complete-or-other-backend'."
  ;; To test the need for this, disable this advice, enable
  ;; company-tng, start completion, invoke
  ;; `my:company-complete-or-other-backend' to go to the next backend,
  ;; hit TAB to go to the first candidate, then invoke
  ;; `my:company-complete-or-other-backend' again.  company-tng will
  ;; complete *and then invoke `my:company-complete-or-other-backend'
  ;; which will restart completion*.  We need to avoid company-tng
  ;; completing, since `my:company-complete-or-other-backend' will
  ;; call `company-complete-selection' itself thankyouverymuch.
  (or (eq this-command 'my:company-complete-or-other-backend)
      (apply orig-fun args)))

(advice-add 'company--company-command-p :around
            #'my:company-complete-or-other-is-company-cmd)

(defun my:company-prefix-args-dont-end-tng (orig-fun &rest args)
  "Advice to prevent prefix arg commands from ending company-tng completion.

This allows use of a prefix to commands such as
`my:company-complete-or-other-backend' while in the middle of a
completion while using company-tng."
  (or (memq this-command
            '(universal-argument digit-argument negative-argument))
      (apply orig-fun args)))

(advice-add 'company--company-command-p :around
            #'my:company-prefix-args-dont-end-tng)
