;; -*- lexical-binding: t; -*-

;; Speed up bookmark+ moving between auto-named bookmarks.  And maybe
;; speed it up in other places as well, I don't know, I haven't
;; tested.

(defvar my:bookmark--in-bookmark-save nil
  "Set to true only within a call to `bookmark-save'.")

;; Maybe I should submit this upstream.
(define-advice customize-save-variable
    (:before-until (variable value &rest _args) my:speed-up-bookmark-save)
  "Within `bookmark-save', only save when VARIABLE != VALUE.
This speeds up bookmark+ which tries to save this variable within
`bookmark-save' when `bmkp-last-as-first-bookmark-file' is
non-nil."
  (and my:bookmark--in-bookmark-save
       (boundp variable)
       (equal (symbol-value variable) value)))

(define-advice bookmark-save
    (:around (orig-fun &rest args) my:speed-up-bookmark-save)
  "Hacks to speed up `bookmark-save'.
This is all very brittle, but makes saving bookmarks maybe ~14x
faster, in my testing."
  ;; Neuter `vc-mode' and `magit-auto-revert-mode' `find-file-hook's
  ;; for a file we are (presumably) only opening transiently, that
  ;; being our bookmarks file.  They are slow, relatively speaking.
  (cl-letf ((vc-handled-backends nil)
            ;; This is really a fairly offensive way to turn
            ;; `magit-auto-revert-mode-enable-in-buffers' into a noop.
            ;; I should do better.
            ((default-value 'auto-revert-mode-set-explicitly) t)
            (my:bookmark--in-bookmark-save t))
    (apply orig-fun args)))
