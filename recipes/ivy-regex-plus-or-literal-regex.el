;; -*- lexical-binding: t; -*-

(require 'ivy)

;; `ivy--split' is kind of insane IMHO.  See
;; https://github.com/abo-abo/swiper/commit/e598b044e2728f5e3b163db99c0f0c4d7c56c6e4
;; for example.  Try submitting "req\\(uest\\)?" to `ivy--regex-plus'
;; and you will see the madness.  This alternative re-builder is
;; `ivy--regex-plus' *unless* you include a "\\(" in your search
;; string, in which case it assumes you're typing a regexp and uses it
;; literally.

(defun my:ivy--regex-plus-or-literal-regex (str)
  "Convert STR with `ivy--regex-plus', or use it literally if it has \\(."
  ;; This `replace-regexp-in-string' call will convert
  ;;
  ;;       \\( → (
  ;;       \\x → x
  ;;         ( →     (that is, it deletes lone open parens)
  ;;     \\\\( → \\  (because that's a literal backslash plus open paren)
  ;;
  ;; We can then check the result for parens, which mean they were in
  ;; the input as "\\(", which indicates the presence of a regexp
  ;; group.
  (if (and (string-match-p "("
                           (replace-regexp-in-string (rx (|
                                                          (: "\\" (group any))
                                                          "("))
                                                     "\\1" str))
           (ivy--regex-p str))
      ;; User knows best, literal regexp.
      str
    (ivy--regex-plus str)))
