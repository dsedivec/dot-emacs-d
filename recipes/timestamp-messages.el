;; -*- lexical-binding: t; -*-

;; This attempts to put time stamps in front of everything written
;; with the `message' function.  It attempts to correctly handle
;; repeated messages, and the fact that `message' treats "..."
;; specially.

;; Not every message will have a time stamp put on it.  This library
;; works by advising `message', but Emacs sometimes writes to
;; *Messages* without going through `message'.

;; Some sort of before/after change hook might be appropriate here in
;; addition, or instead of, the advice used here.  That might be able
;; to timestamp more lines than the current approach does.

(require 'subr-x)

(defvar my:messages-time-stamp-format "[%F %T.%6N] ")

(defvar my:messages-time-stamp-regexp
  "\\(\\[2[0-9]\\{3\\}-\\(?:0[1-9]\\|1[0-2]\\)-\\(?:[012][0-9]\\|3[01]\\) [012][0-9]\\(?::[0-5][0-9]\\)\\{2\\}.[0-9]\\{6\\}\\] \\)"
  "Regexp that matches `my:messages-time-stamp-format' in a capture group.")

(define-advice message
    (:around (orig-fun format-string &rest args) my:timestamp-messages)
  "Add time stamps to lines in *Messages*."
  ;; Only bother if we're actually writing to *Messages*.
  (if (and message-log-max format-string)
      ;; (message) behaves specially in two cases:
      ;;
      ;; 1. If you log the same message multiple times
      ;; 2. If the message you're logging contains "..." and its
      ;;    prefix matches the prior message
      ;;
      ;; To let (message) work in these two cases we have to look to
      ;; see if the current message will match either of the above
      ;; messages, then remove our time stamp from the last line (if
      ;; there is one) before calling (message).  Once we're done
      ;; we'll put back the time stamp we removed, if necessary.  (It
      ;; would always be necessary, probably, if I didn't go lazy and
      ;; use string-match-p below, which finds the first match.  We
      ;; find the first "..." but Emacs internals find the last "...",
      ;; so our msg-prefix may end up not being the same prefix Emacs
      ;; looks for.  In any case, though, "maybe put back the time
      ;; stamp if necessary" seems like a more cautious, and thus
      ;; better general policy, my laziness aside.)
      ;;
      ;; Why let-bind `deactivate-mark'?  I don't really know why, but
      ;; somehow, this advice ends up causing `deactivate-mark' to get
      ;; set to t in a calling buffer, which breaks transient mark
      ;; mode in that you can never activate the region.  I tried
      ;; pretty hard to figure out how this was happening, since
      ;; `deactivate-mark' is automatically local when set, but I
      ;; failed.  I have not ruled out an Emacs bug.  let-binding
      ;; `deactivate-mark' is not great, since we are modifying the
      ;; messages buffer and its mark probably should get activated,
      ;; but this felt like a better solution in terms of performance
      ;; and invasiveness compared to my alternative of calling
      ;; (make-local-variable 'deactivate-mark) here instead.
      (let* (deactivate-mark
             (msg (apply #'format-message format-string args))
             (msg-last-line (if-let ((idx (save-match-data
                                            (and
                                             (string-match "\\(?:\n\\|.\\)*\n"
                                                           msg)
                                             (match-end 0)))))
                                (substring msg idx)
                              msg))
             (msg-prefix (if-let ((idx (string-match-p "\\.\\.\\."
                                                       msg-last-line)))
                             (substring msg-last-line 0 (+ idx 3))
                           msg-last-line))
             prev-stamp prev-last-line)
        (with-current-buffer (messages-buffer)
          (save-excursion
            ;; Go to BOL on the last message in *Messages*.
            (goto-char (point-max))
            (forward-line (if (bolp) -1 0))
            (save-match-data
              (when (looking-at (concat
                                 my:messages-time-stamp-regexp
                                 (regexp-quote msg-prefix)))
                ;; Remove time stamp from last line, maybe to be put
                ;; back later.
                (setq prev-stamp (match-string 1))
                (let ((inhibit-read-only t))
                  (replace-match "" nil nil nil 1))))
            (setq prev-last-line (point-marker))))
        (prog1
            (funcall orig-fun "%s" msg)
          (with-current-buffer (messages-buffer)
            (save-excursion
              (let ((inhibit-read-only t))
                (goto-char prev-last-line)
                ;; Try and find the new line that's been written by
                ;; (message).
                (forward-line 1)
                (when (eobp)
                  ;; (message) replaced the last line rather than adding
                  ;; a new one, back up and put a new time stamp on the
                  ;; presumably-edited last line in the buffer.
                  (forward-line -1)
                  (setq prev-stamp nil))
                (insert (format-time-string my:messages-time-stamp-format))
                (when prev-stamp
                  (goto-char prev-last-line)
                  (insert prev-stamp)))))))
    (apply orig-fun format-string args)))
