;; -*- lexical-binding: t; -*-

;; Defines new command `my:describe-map' that shows you all of the
;; bindings in a given keymap.

(defvar my:describe-map-temp-map)

(defun my:describe-map (keymap-name)
  (interactive (list (completing-read "Keymap: " obarray
                                      (lambda (sym)
                                        (or (and (boundp sym)
                                                 (keymapp (symbol-value sym)))
                                            (keymapp sym)))
                                      t)))
  (with-help-window (help-buffer)
    (princ (format-message "Bindings in `%s':\n\n" keymap-name))
    ;; `keymapp' identifies symbols as keymaps when they are unbound,
    ;; but have a keymap in their function slot, which I think may be
    ;; some old prefix keymap thing.  However, in the case of such a
    ;; symbol-function-as-keymap, `substitute-command-keys' doesn't
    ;; expand it.  To get around this, we let-bind the desired keymap
    ;; to `my:describe-map-temp-map', then have
    ;; `substitute-command-keys' print that.
    (let* ((keymap-name (if (stringp keymap-name)
                            (intern keymap-name)
                          keymap-name))
           (my:describe-map-temp-map
            (if (and (boundp keymap-name)
                     (keymapp (symbol-value keymap-name)))
                (symbol-value keymap-name)
              (symbol-function keymap-name))))
      (princ (substitute-command-keys "\\{my:describe-map-temp-map}")))))
