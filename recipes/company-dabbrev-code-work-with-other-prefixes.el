;; -*- lexical-binding: t; -*-

;; This is advice on `company-dabbrev-code' that lets other backends
;; pick the prefix, as long `company-dabbrev-code' is being used in a
;; grouped backend and it is not the first backend.  It's designed to
;; let you get more completions in the scenario where "smarter"
;; backends in the group might not be able to help you, but where
;; they've alreaady provided a prefix.
;;
;; The reason for this to exist is because company-mode will not
;; include completions from a backend within a group when it returns a
;; different prefix from the first backend to return a prefix.  (Note
;; to future self: this happens regardless of :with.  Backends after
;; :with are never given the opportunity to *pick* a prefix, but they
;; must return the *same* prefix as whatever was already chosen in
;; order to be able to contribute completions.)
;;
;; Here's an example Python buffer, with cursor at "|":
;;
;;     class A:
;;         pass
;;
;;     x = A()
;;     x.long_name = "foo"
;;     x.lon|
;;
;; If `company-backends' is set to
;;
;;     ((company-anaconda company-dabbrev-code))
;;
;; and you invoke `company-complete', `company-anaconda' will choose
;; the prefix "x.lon" *and will not offer any completions for that
;; prefix*. `company-dabbrev-code', on the other hand, will return
;; `lon' for the prefix.  Because `company-dabbrev-code' picked a
;; different prefix from `company-anaconda', any completions it might
;; be able to offer are ignored by company-mode.  The end result is
;; that you get no completions.
;;
;; With this advice, though, we'll notice that `company-dabbrev-code'
;; is being used in a group, and we'll instead return whatever
;; `company-prefix' was picked by a previous backend.  In the above
;; example, that means (company-dabbrev-code 'prefix) would return
;; company-anaconda's "x.lon" prefix.  Because the prefixes from
;; `company-anaconda' and `company-dabbrev-code' now match,
;; company-mode will proceed to poll both for candidates.  The end
;; result is that you get some completions from
;; `company-dabbrev-code', even though `company-anaconda' was
;; unhelpful.

(defvar my:company-dabbrev-code--grouped-completions-prefix nil
  "Completion prefix already selected.
Set, by advice, only within a call to
`company--multi-backend-adapter-candidates'.")

(define-advice company--multi-backend-adapter-candidates
    (:around
     (orig-fun backends prefix &rest args)
     my:set-company-dabbrev-code--grouped-completions-prefix)
  (let ((my:company-dabbrev-code--grouped-completions-prefix prefix))
    (apply orig-fun backends prefix args)))

(define-advice company-dabbrev-code
    (:around
     (orig-fun &optional command &rest args)
     my:always-add-candidates-in-grouped-completion)
  (if (not my:company-dabbrev-code--grouped-completions-prefix)
      ;; Don't behave any differently than usual when used in contexts
      ;; other than during a call to
      ;; `company--multi-backend-adapter-candidates'.  See our advice,
      ;; above.
      (apply orig-fun command args)
    (cl-case command
      (prefix
       (if (apply orig-fun command args)
           ;; Use whatever prefix was already chosen.  See previous
           ;; advice on `company--multi-backend-adapter-candidates'.
           my:company-dabbrev-code--grouped-completions-prefix
         ;; `company-dabbrev-code' doesn't want to play here at all,
         ;; so return nil for prefix, meaning we won't be consulted
         ;; for candidates next.
         nil))
      (candidates
       ;; Use the prefix that `company-dabbrev-code' would normally
       ;; use instead of whatever else was chosen.  Need to then graft
       ;; on the rest of the chosen prefix to match any completions
       ;; from `company-dabbrev-code' because
       ;; `company--insert-candidate' is going to expect to replace
       ;; that previously-chosen prefix if we insert one of these
       ;; candidates.  (Plus the candidates with different
       ;; (e.g. missing) prefix from `company-dabbrev-code' will look
       ;; dumb next to others in the list.)
       (let* ((real-prefix my:company-dabbrev-code--grouped-completions-prefix)
              (dabbrev-prefix (funcall orig-fun 'prefix)))
         (mapcar (lambda (cand)
                   ;; For speed, not copying text properties here.
                   ;; May be a bad idea?  Right now
                   ;; `company-dabbrev-code' doesn't use any text
                   ;; properties AFAIK, and AFAIK the prefix shouldn't
                   ;; have any properties either.
                   (concat real-prefix
                           (substring cand (length dabbrev-prefix))))
                 (apply orig-fun command dabbrev-prefix (cdr args)))))
      (t
       (apply orig-fun command args)))))
