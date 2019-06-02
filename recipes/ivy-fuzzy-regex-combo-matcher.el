;; -*- lexical-binding: t; -*-

;; Custom RE builder: if search string contains space then it's
;; ivy--regex-ignore-order, otherwise it's ivy--regex-fuzzy.

(defcustom my:ivy-regular-re-builder #'ivy--regex-ignore-order
  "Non-fuzzy RE builder to use in `my:ivy--regex-regular-or-fuzzy'."
  :group 'ivy
  :type 'function)

(defcustom my:ivy-fuzzy-re-builder #'ivy--regex-fuzzy
  "Fuzzy RE builder to use in `my:ivy--regex-regular-or-fuzzy'."
  :group 'ivy
  :type 'function)

(defun my:ivy--regular-or-fuzzy-re-builder-for-input (&optional input)
  "Return fuzzy RE builder or regular RE builder based on INPUT.
`ivy-text' is used if STR is not supplied."
  ;; Fun fact: string-match-p is like 10x faster than seq-contains.
  (let ((has-space-or-regexp (string-match-p "[!^$\\\\ ]" (or input ivy-text))))
    (if has-space-or-regexp
        my:ivy-regular-re-builder
      my:ivy-fuzzy-re-builder)))

(defun my:ivy--regular-or-fuzzy-get-actual-re-builder (&optional str)
  "Return current RE builder, possibly considering STR.
If `my:ivy--regex-regular-or-fuzzy' is in use then STR will be
considered to see if it contains a space, and then this function
will return either the regular or fuzzy RE builders."
  (if (eq ivy--regex-function 'my:ivy--regex-regular-or-fuzzy)
      (my:ivy--regular-or-fuzzy-re-builder-for-input str)
    ivy--regex-function))

(defun my:ivy--regex-regular-or-fuzzy (str &rest args)
  "Use fuzzy RE builder if STR contains space, regular RE builder otherwise."
  ;; swiper--add-overlays will error out if this indicates subexps
  ;; where there are none.  ivy--regex-fuzzy and ivy--regex set this.
  ;; ivy--regex-ignore-order does not set it to a meaningful value,
  ;; nor to 0, so if you switch between these sorts of RE builders you
  ;; encounter this problem.  No harm in clearing it each time.
  (setq ivy--subexps 0)
  (let ((re-builder (my:ivy--regular-or-fuzzy-re-builder-for-input str)))
    (apply re-builder str args)))

(defun my:ivy--highlight-regular-or-fuzzy (&rest args)
  "Highlight for fuzzy or regular RE builder as appropriate."
  (let* ((re-builder (my:ivy--regular-or-fuzzy-re-builder-for-input))
         (highlighter (or (alist-get re-builder ivy-highlight-functions-alist)
                          #'ivy--highlight-default)))
    (when (and (eq re-builder 'ivy--regex-ignore-order)
               (eq highlighter 'ivy--highlight-ignore-order))
      ;; ivy--filter special-cases ivy--regex-ignore-order.  We need
      ;; to set ivy--old-re here for ivy--highlight-ignore-order to
      ;; work.  I stole the hint on how to invoke ivy--regex-function
      ;; from ivy--highlight-default.
      (setq ivy--old-re (ivy--regex-ignore-order ivy-text)))
    (apply highlighter args)))

(setf (alist-get 'my:ivy--regex-regular-or-fuzzy
                 ivy-highlight-functions-alist)
      #'my:ivy--highlight-regular-or-fuzzy)

;; Note that Swiper uses swiper--re-builder, which will not engage flx
;; sorting here--but flx sorting with Swiper and ivy--regex-fuzzy
;; sucks.  You do not want that, therefore this lack of flx in the
;; case of Swiper is good.
(define-advice ivy--sort
    (:around (orig-fun name candidates) my:ivy-regular-or-fuzzy-flx-sort-fix)
  "Engage flx sorting if fuzzy matching.
`ivy--sort' behaves specially when `ivy--regex-function' is
exactly `ivy--regex-fuzzy'."
  (let ((ivy--regex-function
         (my:ivy--regular-or-fuzzy-get-actual-re-builder name)))
    (funcall orig-fun name candidates)))

(define-advice ivy--recompute-index
    (:around (orig-fun name &rest args) my:ivy-regular-or-fuzzy-flx-index-fix)
  "Compute indexes better when fuzzy matching.
`ivy--recompute-index' special-cases `ivy--regex-fuzzy'.  Set
`ivy--regex-function' before calling it to get its special
behavior when using fuzzy searching."
  (let ((ivy--regex-function
         (my:ivy--regular-or-fuzzy-get-actual-re-builder name)))
    (apply orig-fun name args)))

;; `ivy--exhibit' implements special behavior for switching buffers,
;; such as with `ivy-switch-buffer': if the input has a leading space
;; then it will only return "hidden" buffers (those with leading
;; spaces in their names) as candidates.  Without a leading space it
;; will never return hidden buffers as candidates.
;;
;; Since I commonly use `my:ivy--regex-regular-or-fuzzy' with a
;; leading space in order to switch to `ivy--regex-ignore-order', this
;; caused a problem since I would frequently find myself only
;; presented with hidden buffers as candidates---not what I wanted.
;;
;; These two pieces of advice change Ivy's behavior when completing
;; buffers: when `my:ivy--regex-regular-or-fuzzy' is in use, two
;; leading spaces (or "\ " or "^ ") are required to start searching
;; hidden buffers.  A single leading space will be interpreted as a
;; request to switch to the other regexp builder, as usual.  This
;; makes sense, since switching to hidden buffers is expected to be
;; uncommon.
;;
;; The implementation here is gross.  It is necessary because of the
;; special case that handles `internal-complete-buffer' completions in
;; `ivy--exhibit' (which may itself be considered "gross" by some).
;; Ivy only requests the list of buffer candidates when the search
;; string changes from no leading space to having a leading space, or
;; from having a leading space to no leading space.  Therefore, to
;; reach our goals here, we trick `ivy--exhibit' into recomputing
;; completion candidates by clobbering `ivy--old-text' whenever the
;; search string changes between "two leading spaces" and "less than
;; two leading spaces".  Then we must further advise
;; `ivy--buffer-list' to change which set of candidates it will return
;; when it is called to produce the list of candidates, which itself
;; ugly.
;;
;; Alternatives to all this ugliness that I have not really explored:
;;
;; * Change how Ivy decides when to get a new list of buffer
;;   candidates, maybe via a user-configurable predicate function on
;;   the input.
;;
;; * Use a key binding for SPC in `ivy-minibuffer-map' that doesn't
;;   insert space when you're at the beginning of the input in the
;;   minibuffer, but instead it just sets a flag or toggles the regexp
;;   builder for you, preferrably with some sort of message telling
;;   you that the matcher has been changed.

(defconst my:ivy--intentional-leading-space-regexp "^\\(?:  \\|\\\\ \\|\\^ \\)")

(define-advice ivy--buffer-list (:around
                                 (orig-fun str &optional virtual predicate)
                                 my:fix-leading-space-with-my-re-builder)
  (when (and (eq ivy--regex-function #'my:ivy--regex-regular-or-fuzzy)
             (equal str " ")
             (not (string-match-p my:ivy--intentional-leading-space-regexp
                                  ivy-text)))
    ;; Switch this to a regular non-leading-space `ivy--buffer-list'
    ;; call.
    (setq str ""))
  (funcall orig-fun str virtual predicate))

(define-advice ivy--input
    (:filter-return (str) my:buffer-switch-combo-matcher-ignore-leading-space)
  (when (and (eq ivy--regex-function #'my:ivy--regex-regular-or-fuzzy)
             (eq (ivy-state-collection ivy-last) #'internal-complete-buffer)
             (not (equal
                   (string-match-p my:ivy--intentional-leading-space-regexp
                                   str)
                   (string-match-p my:ivy--intentional-leading-space-regexp
                                   ivy--old-text))))
    (setq ivy--old-text ""))
  str)

;; Use `my:ivy--regex-regular-or-fuzzy' as our default regexp builder.
(setf (alist-get t ivy-re-builders-alist) #'my:ivy--regex-regular-or-fuzzy)
