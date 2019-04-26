;; -*- lexical-binding: t; -*-

;; epergny on IRC kindly pointed out that an easier way here might be
;; to supply something like -M to ripgrep which limits its output (in
;; *bytes* in ripgrep's case).

(defvar my:counsel-git-grep-max-result-width 2.0
  "Maximum length of git-grep/ag/rg/etc. matches displayed in minibuffer.
If this is a real number, it is multiplied by the width of the
minibuffer.  Otherwise it is an integer number of columns.")

(defun my:counsel-git-grep-truncate-result (str)
  "Truncate results according to `my:counsel-git-grep-max-result-width'.
Prevents minibuffer from growing to ~95% of frame height when you
accidentally grep over files with really long lines and you have
`ivy-truncate-lines' turned off.

Lines containing tabs will be cut short because `string-width'
always considers tabs to take eight columns, presumably since it
doesn't know where on the screen you will start displaying the
string (which it would need to know in order to guess how many
columns a tab will actually occupy)."
  (if ivy-truncate-lines
      ;; We don't need to truncate, the lines will already be
      ;; truncated.
      str
    (with-selected-window (or (active-minibuffer-window) (selected-window))
      (let* ((max-len-factor my:counsel-git-grep-max-result-width)
             (max-result-len (max (if (floatp max-len-factor)
                                      (floor (* (window-body-width)
                                                max-len-factor))
                                    max-len-factor)
                                  ;; Kind of a minimum width: we want
                                  ;; to show you the path, line number
                                  ;; (hopefully not that long...),
                                  ;; plus at least a little bit of the
                                  ;; match.  This is more-or-less a
                                  ;; sanity check.
                                  (+ (or (seq-position str ?:) 0) 20))))
        (ivy--truncate-string str max-result-len)))))

(advice-add 'counsel-git-grep-transformer :filter-return
            #'my:counsel-git-grep-truncate-result)
