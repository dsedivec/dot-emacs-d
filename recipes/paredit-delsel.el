;; -*- lexical-binding: t; -*-

;; Make <backspace>/C-d work like you'd expect in `paredit-mode'.

(require 'paredit)

(defun my:paredit-delete-selection ()
  "To be used as `delete-selection' property on paredit commands.
This is just `paredit-delete-region' but returns 'supersede so
that delsel doesn't execute the invoked paredit
command (e.g. `paredit-forward-delete')."
  ;; Must not raise error or else you'll get `delete-selection-mode's
  ;; `post-command-hook' function uninstalled, which will confuse the
  ;; shit out of you until you realize you need to toggle it off/on
  ;; again to get the hook reinstalled.
  (with-demoted-errors "Error: %S"
    (unless (use-region-p)
      ;; Really should never get here.
      (error "Region not set"))
    ;; We don't want to execute the typed command, we're superseding it
    ;; with `paredit-delete-region'.
    (setq this-command 'ignore)
    (paredit-delete-region (region-beginning) (region-end))))

(put 'paredit-forward-delete 'delete-selection #'my:paredit-delete-selection)
(put 'paredit-backward-delete 'delete-selection #'my:paredit-delete-selection)
