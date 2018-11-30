;; -*- lexical-binding: t; -*-

;; Edit GFM fenced code blocks in a separate buffer
;;
;; I find this a nice way to edit the contents of a fenced code block
;; in a proper major mode, nicer than any of the multiple major mode
;; solutions I've tried.

(defvar my:gfm-fcb-info nil
  "Buffer local that points back to where fenced code block content belongs.")
;; Needs to survive kill-all-local-variables.
(put 'my:gfm-fcb-info 'permanent-local t)

(defun my:gfm-fcb-save ()
  "Copy buffer contents back to the fenced code block from whence
it came."
  (interactive)
  (unless my:gfm-fcb-info
    (error "No fenced block info, is this a GFM fenced block edit buffer?"))
  (cl-destructuring-bind (start . end) my:gfm-fcb-info
    (cl-assert (and (markerp start) (markerp end)))
    (when (buffer-modified-p)
      (save-restriction
        (widen)
        (let ((fenced-block-buffer (current-buffer)))
          (with-current-buffer (marker-buffer start)
            (save-excursion
              (delete-region start end)
              (goto-char start)
              (insert-buffer-substring-no-properties fenced-block-buffer)
              (unless (bolp)
                (newline))))))
      (set-buffer-modified-p nil))))

(defun my:gfm-fcb-kill-query-hook ()
  (if (buffer-modified-p)
      (yes-or-no-p "Buffer has unsaved changes, kill without saving? ")
    t))

(defun my:gfm-fcb-done-editing ()
  "Binding for C-c ' in a buffer editing a fenced code block."
  (interactive)
  (my:gfm-fcb-save)
  (quit-restore-window nil 'kill))

;; We use a minor mode so that we can set our own key bindings.
;; (Protip: local-set-key actually means "set a keybinding in this
;; major-mode's map," which is clearly not what we want, especially
;; since we remap save-buffer.)

(defvar my:gfm-fcb-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c '") 'my:gfm-fcb-done-editing)
    (define-key map [remap save-buffer] 'my:gfm-fcb-save)
    map))

(define-minor-mode my:gfm-fcb-edit-mode
    "Mode for editing fenced code blocks in GitHub-flavored Markdown."
  :lighter " Fenced"
  :keymap my:gfm-fcb-edit-mode-map

  (add-hook 'kill-buffer-query-functions 'my:gfm-fcb-kill-query-hook nil t))

;; We need my:gfm-fcb-edit-mode to survive things like a major mode
;; change.  Setting permanent-local-hook on this
;; after-change-major-mode-hook function makes sure we can reactivate
;; the minor mode after a major mode switch.
(defun my:gfm-fcb-reenable-edit-mode ()
  (my:gfm-fcb-edit-mode 1))
(put 'my:gfm-fcb-reenable-edit-mode 'permanent-local-hook t)

(defvar my:gfm-fcb-language-mode-map nil
  "Maps fenced code block language identifiers to major modes.

Should be an alist of (\"IDENTIFIER\" . major-mode-function).")

(defun my:gfm-fcb-edit ()
  (interactive)
  (let (start end lang mode-func buffer-name buffer content)
    (save-excursion
      ;; End of line doesn't have the text property we want.
      (forward-line 0)
      (when (get-text-property (point) 'markdown-gfm-block-end)
        ;; We're at the end of some kind of block.  Move up one line
        ;; which should put us either in the middle of a code block or
        ;; at the line that starts the code block.
        (unless (zerop (forward-line -1))
          (error "Can't find start of fenced code block")))
      (when (get-text-property (point) 'markdown-gfm-code)
        (let ((start
               (previous-single-property-change (point)
                                                'markdown-gfm-block-begin)))
          (unless start
            (error "Can't find start of fenced code block"))
          (goto-char start)
          (forward-line 0)))
      (unless (looking-at "^```\\s-*\\(\\w+\\)?")
        (error "Can't find start of fenced code block"))
      (setq lang (match-string 1))
      (if lang
          (setq mode-func (or (assoc lang my:gfm-fcb-language-mode-map)
                              (intern (concat lang "-mode")))))
      (unless (zerop (forward-line 1))
        (error "Can't find end of fenced code block"))
      (setq start (point-marker))
      (set-marker-insertion-type start t)
      (when-let* ((code-middle-prop (get-text-property (point)
                                                       'markdown-gfm-code)))
        (goto-char (cadr code-middle-prop)))
      (unless (looking-at "^```\\s-*$")
        (error "Can't find end of fenced code block"))
      (setq end (point-marker)
            content (buffer-substring-no-properties start end)
            buffer-name (format "*GFM %s%s code*" (buffer-name)
                                (if lang (concat " " lang) ""))
            buffer (generate-new-buffer buffer-name))
      (switch-to-buffer-other-window buffer)
      (insert content)
      (set-buffer-modified-p nil)
      (if (fboundp mode-func)
          (funcall mode-func))
      (set (make-local-variable 'my:gfm-fcb-info) (cons start end))
      (my:gfm-fcb-edit-mode)
      ;; This will make sure my:gfm-fcb-edit-mode stays active even if
      ;; you change major modes.
      (add-hook 'after-change-major-mode-hook 'my:gfm-fcb-reenable-edit-mode
                nil t))))
