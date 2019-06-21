;; -*- lexical-binding: t; -*-

;; Periodically save persp-mode configuration.  Without this, saving
;; only happens (by default) on exit or when exiting persp-mode (which
;; surprises me).

(defun my:persp-mode-do-auto-save ()
  ;; Save messages from `persp-save-state-to-file' are noisy and can
  ;; only be quieted by `inhibit-message' AFAIK.  (See write-region in
  ;; fileio.c, "Wrote %s" message is generated there.)  However,
  ;; persp-mode also seems to be willing to `message' some *error*
  ;; messages, and if we inhibit those, you might never know that
  ;; saving is failing.  Therefore we try to alert the user to
  ;; messages that look like errors after calling
  ;; `persp-save-state-to-file'.
  (if persp-mode
      ;; Wrapping this in `save-match-data' since we are running
      ;; sporadically, and we use `re-search-forward', and even if we
      ;; didn't there's no telling what `persp-save-state-to-file'
      ;; might be doing.
      (save-match-data
        (let ((log-pos-before (with-current-buffer (messages-buffer)
                                ;; Using a marker in case, somehow,
                                ;; *Messages* gets truncated by
                                ;; `persp-save-state-to-file' (or by
                                ;; something else that slips in to run
                                ;; before we can scrape *Messages*).
                                (point-max-marker))))
          ;; Only want to inhibit messages for this one single call.
          (let ((inhibit-message t)
                ;; This gets rid of the "Saving"... messages, but not
                ;; the "Wrote"... messages.
                (save-silently t)
                ;; I have a lot of problems on macOS with my host name
                ;; changing and Emacs thinking some other user [with
                ;; my user name] on some other host [usually with the
                ;; same first component of the host name] has locked
                ;; the auto-save file.  Just turn off locking here.
                (create-lockfiles nil))
            (persp-save-state-to-file))
          (with-current-buffer (messages-buffer)
            (save-excursion
              (goto-char log-pos-before)
              (let ((case-fold-search t))
                ;; persp-mode is, at least, consistent in always
                ;; writing messages in the format:
                ;;
                ;;     [persp-mode] (Error|Warning):
                (when (re-search-forward "persp.*\\(error\\|warn\\)" nil t)
                  (warn (concat "persp-mode auto-save may have failed, check %s"
                                (buffer-name)))))))))
    (warn "Stopping `my:persp-mode-auto-save-timer' because persp-mode is off")
    (my:persp-mode-cancel-auto-save-timer)))

(defvar my:persp-mode-auto-save-interval 60)

(defvar my:persp-mode-auto-save-timer nil)

(defun my:persp-mode-start-auto-save-timer ()
  "Start (or restart) the persp-mode auto-save timer."
  ;; Interactive for convenience.
  (interactive)
  (my:persp-mode-cancel-auto-save-timer)
  (setq my:persp-mode-auto-save-timer
        (run-at-time t my:persp-mode-auto-save-interval
                     #'my:persp-mode-do-auto-save)))

(defun my:persp-mode-cancel-auto-save-timer ()
  (when my:persp-mode-auto-save-timer
    (cancel-timer my:persp-mode-auto-save-timer)
    (setq my:persp-mode-auto-save-timer nil)))

(defun my:persp-mode-maybe-start-auto-save-with-mode ()
  ;; Don't start auto-saving if persp-mode is going to auto-resume.
  ;; See next function.
  (when (and my:persp-mode-auto-save-interval
             (<= persp-auto-resume-time 0.0))
    (my:persp-mode-start-auto-save-timer)))

;; This is me going to great lengths to only turn on auto-saving after
;; persp-mode has restored perspectives.  If an error occurs while
;; loading, let the error occur, but warn the user that auto-saving
;; has not been turned on.  End goal here is not to, say, save out
;; empty perspectives before we can properly load state.
;;
;; We can't just add to `persp-after-load-state-functions' because
;; that only runs if data is successfully restored: we additionally
;; want to know when an error happened and then warn the user about
;; auto-saving.
(defun my:persp-mode-maybe-start-auto-save-after-load-state
    (orig-fun &rest args)
  (prog1
      (condition-case err
          (apply orig-fun args)
        (t
         (warn "persp-mode auto-save could not be started due to error")
         ;; Re-raise
         (signal (car err) (cdr err))))
    (when (and my:persp-mode-auto-save-interval
               (> persp-auto-resume-time 0.0))
      (my:persp-mode-start-auto-save-timer))))

(add-hook 'persp-mode-hook #'my:persp-mode-maybe-start-auto-save-with-mode)

(advice-add 'persp-load-state-from-file :around
            #'my:persp-mode-maybe-start-auto-save-after-load-state)

(add-hook 'persp-mode-deactivated-hook #'my:persp-mode-cancel-auto-save-timer)

(when persp-mode
  (warn (concat "persp-mode turned on already, run"
                " `my:persp-mode-start-auto-save-timer' yourself?")))
