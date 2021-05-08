;; -*- lexical-binding: t; -*-

;; In addition to test case in comment below, also try '''''' at BOB
;; and try inserting ' at BOB with '' in front of point.  Also make
;; sure apostrophes don't pair in comments.
;;
;; References:
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=40231
;; commit 289d6b2265e

(defvar sql-mode-map)

(defun my:sql-mode-electric-apostrophe (arg)
  (interactive "P")
  (cond
    ((or (not electric-pair-mode)
         arg)
     (call-interactively #'self-insert-command))
    ((and (eq (char-after) ?')
          (eq (nth 3 (syntax-ppss)) ?')
          ;; I think we don't want this given apostrophe at:
          ;;
          ;;     'foo bar'|' baz'
          ;; (save-excursion
          ;;   (not (eq (nth 3 (syntax-ppss (1+ (point)))) ?')))
          )
     ;; We were already at a string, and the character after point is
     ;; an apostrophe.  Just move beyond it.  (This was the behavior
     ;; elec-pair before Emacs bug #40231.)
     (forward-char 1))
    ;; XXX I have no idea if any of this makes sense if/when
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=44653 gets fixed.
    ;; Check back then.  I think `self-insert-command' should be
    ;; sufficient, but ISTR I had to avoid using it in this case
    ;; because elec-pair was confounding me?  I don't quite remember.
    ((and (eq (char-before) ?')
          (not (nth 3 (progn
                        ;; (syntax-ppss-flush-cache (- (point) 2))
                        (syntax-ppss)))))
     ;; (self-insert-command 1)
     (insert "'")
     (save-excursion (electric-pair--insert ?'))
     )
    (t
     (self-insert-command 1))))

(with-eval-after-load 'sql
  (define-key sql-mode-map (kbd "'") #'my:sql-mode-electric-apostrophe))
