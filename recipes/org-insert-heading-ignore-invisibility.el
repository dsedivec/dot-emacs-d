;; -*- lexical-binding: t; -*-

;; I suspect 68d8f860cd introduced behavior where M-RET will insert a
;; new headline after the *subtree* rather than on the following line
;; if the character *before* point is invisible. I will be surprised
;; not a whit if this is someday identified as a bug.  In the
;; meantime, pass in invisible-ok as T unless it was explicitly
;; specified otherwise, which brings back the behavior I want: insert
;; a new headline!
;;
;; Test org file, with point at | when you hit M-RET, PROPERTIES
;; drawer is collapsed (could be LOGBOOK or whatever else, as long as
;; it's invisible):
;;
;;     * Foo
;;     :PROPERTIES:...|
;;     ** Child
;;
;; Without advice, you end up with a new heading below Child.  With
;; advice, you end up with a heading above Child.

(define-advice org-insert-heading
    (:filter-args (args) my:ignore-invisibility-when-inserting-headline)
  ;; defun org-insert-heading (&optional arg invisible-ok top)
  (if (>= (length args) 2)
      args
    (list (car args) t)))
