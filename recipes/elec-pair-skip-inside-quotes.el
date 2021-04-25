;; -*- lexical-binding: t; -*-

;; Golang, `type:"foo|"` point at |, typing " should skip not insert pair
;;
;; Also repros in Python but not in triple-quoted strings because
;; triple-quoted strings have (nth 3 (syntax-ppss)) → t (generic
;; string delimiter), which elec-pair lets skip a closing pair
;; character.
;;
;; This works in my very limited testing, but I feel certain it's
;; going to break something badly...

(require 'el-patch)

(el-patch-feature elec-pair)

(with-eval-after-load 'elec-pair
  (el-patch-defun electric-pair--inside-string-p (char)
    "Return non-nil if point is inside a string started by CHAR.

A comments text is parsed with `electric-pair-text-syntax-table'.
Also consider strings within comments, but not strings within
strings."
    ;; FIXME: could also consider strings within strings by examining
    ;; delimiters.
    (let ((ppss (electric-pair--syntax-ppss (point) '(comment))))
      (el-patch-splice 1 1 (memq (nth 3 ppss) (list t char)))))

  (el-patch-validate 'electric-pair--inside-string-p 'defun t))
