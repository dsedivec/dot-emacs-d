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
  (let ((has-space-or-regexp (string-match-p "[!^\\\\ ]" (or input ivy-text))))
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

;; Defined in counsel.el.
(defvar smex-initialized-p)

;; Note that Swiper uses swiper--re-builder, which will not engage flx
;; sorting here--but flx sorting with Swiper and ivy--regex-fuzzy
;; sucks.  You do not want that, therefore this lack of flx in the
;; case of Swiper is good.
(define-advice ivy--sort
    (:around (orig-fun name candidates) my:ivy-regular-or-fuzzy-flx-sort-fix)
  "Engage flx sorting if fuzzy matching.
`ivy--sort' behaves specially when `ivy--regex-function' is
exactly `ivy--regex-fuzzy'.  Also don't do flx sorting when
`counsel-M-x' is the caller and smex is in use, because smex's
ordering is better than the flx ordering."
  (let ((ivy--regex-function
         (my:ivy--regular-or-fuzzy-get-actual-re-builder name)))
    (if (and (eq (ivy-state-caller ivy-last) 'counsel-M-x)
             (featurep 'smex)
             smex-initialized-p
             (memq ivy--regex-function (list 'ivy--regex-fuzzy
                                             my:ivy-fuzzy-re-builder)))
        candidates
      (funcall orig-fun name candidates))))

(define-advice ivy--recompute-index
    (:around (orig-fun name &rest args) my:ivy-regular-or-fuzzy-flx-index-fix)
  "Compute indexes better when fuzzy matching.
`ivy--recompute-index' special-cases `ivy--regex-fuzzy'.  Set
`ivy--regex-function' before calling it to get its special
behavior when using fuzzy searching."
  (let ((ivy--regex-function
         (my:ivy--regular-or-fuzzy-get-actual-re-builder name)))
    (apply orig-fun name args)))

(setf (alist-get t ivy-re-builders-alist) #'my:ivy--regex-regular-or-fuzzy)
