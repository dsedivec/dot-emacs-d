;; -*- lexical-binding: t; -*-

;; If you put ' in `electric-pair-pairs' it'll fix this, but then you
;; get doubled-up 's in strings and comments, so typing the
;; contraction "don't" becomes a PITA.  You might think to do
;; something like tweak the syntax of ' in
;; `electric-pair-text-syntax-table' to be " (strings) not
;; . (punctuation), but then the single quote still doesn't work right
;; in comments, because I think Python's syntax-table properties still
;; stop things from working right.  So maybe then you want to patch
;; python.el to stop propertizing these characters inside comments,
;; but then Emacs goes into a hard loop and you have to SIGUSR1 it a
;; few times just to bring it back from the dead.  In the end, it
;; turns out writing this function is the easiest solution, for me.
(defun my:python-magic-single-quote (times)
  "Skip over single quotes when used as string delimiters.

Attempts to obey electric-pair-mode settings."
  (interactive "p")
  (let (skip-to-pos)
    (if (and (= times 1)
             electric-pair-mode
             ;; If we were more complete we might call this function
             ;; after setting up single quote as the right kind of
             ;; syntax or putting it in `electric-pair-pairs' or
             ;; something.  But that's hard so we're not complete.
             ;; Sorry.  Just see that it's not nil.  Close enough.
             electric-pair-skip-self
             (let ((syntax (syntax-ppss)))
               (and
                ;; In a string
                (nth 3 syntax)
                ;; That string starts with a single quote
                (eq (char-after (nth 8 syntax)) ?\')
                ;; We're also staring at a single quote
                (eq (save-excursion
                      ;; Stolen from
                      ;; electric-pair-post-self-insert-function.
                      (when
                          (if (and (not (eq electric-pair-skip-whitespace
                                            'chomp))
                                   (functionp electric-pair-skip-whitespace))
                              (funcall electric-pair-skip-whitespace)
                            electric-pair-skip-whitespace)
                        (electric-pair--skip-whitespace))
                      (setq skip-to-pos (point))
                      (char-after))
                    ?\'))))
        (if (not (eq electric-pair-skip-whitespace 'chomp))
            (goto-char (1+ skip-to-pos))
          (delete-region (point) skip-to-pos)
          (forward-char 1))
      (self-insert-command times))))

(defun my:python-magic-double-quote (times)
  "Maybe open a triple-quoted string with double quotes.

Only works when electric-pair-mode is on."
  (interactive "p")
  (cond
    ((and (= times 1)
          electric-pair-mode
          ;; Not in a string nor a comment
          (not (nth 8 (syntax-ppss)))
          ;; "" is right before point
          (ignore-errors
            (string= (buffer-substring (- (point) 2) (point)) "\"\"")))
     (insert-char ?\" 4)
     (forward-char -3))
    (t
     (self-insert-command times))))

(defvar python-mode-map)

(with-eval-after-load 'python
  (bind-keys :map python-mode-map
             ("'" . my:python-magic-single-quote)
             ("\"" . my:python-magic-double-quote)))
