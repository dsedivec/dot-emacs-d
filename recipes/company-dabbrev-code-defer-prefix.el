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
;; :with are simply never given the opportunity to pick a prefix.)
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

(defvar my:company-collecting-multi-backend-candidates nil)

(define-advice company--multi-backend-adapter-candidates
    (:around
     (orig-fun backends &rest args)
     my:set-flag-inside-multi-backend-candidates)
  (let ((my:company-collecting-multi-backend-candidates backends))
    (apply orig-fun backends args)))

(define-advice company-dabbrev-code (:around
                                     (orig-fun command &rest args)
                                     my:find-candidates-using-existing-prefix)
  (if (and (eq command 'prefix)
           my:company-collecting-multi-backend-candidates
           (not (eq (car my:company-collecting-multi-backend-candidates)
                    'company-dabbrev-code))
           company-prefix)
      company-prefix
    (apply orig-fun command args)))
