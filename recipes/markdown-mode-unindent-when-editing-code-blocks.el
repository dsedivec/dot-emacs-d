;; -*- lexical-binding: t; -*-

;; markdown-mode uses edit-indirect to edit code blocks, but it fails
;; to un-indent them if, for example, your fenced code block is part
;; of a list item.  This attempts to un-indent and then re-indent the
;; code while you're editing it.

(require 'edit-indirect)

(defun my:markdown-edit-code-block--remove-indentation ()
  (let* ((overlay edit-indirect--overlay)
         (indent (with-current-buffer (overlay-buffer overlay)
                   (goto-char (overlay-start overlay))
                   (when (and (zerop (forward-line -1))
                              (looking-at (rx (group (1+ space))
                                              (or (>= 3 ?`)
                                                  (>= 3 ?~)))))
                     (match-string-no-properties 1)))))
    ;; Buffer-local hooks on `edit-indirect-after-creation-hook' may
    ;; be run twice due to some vagaries of `run-hooks' and some
    ;; versions of edit-indirect.  After
    ;; https://github.com/Fanael/edit-indirect/issues/16 was fixed I
    ;; suspect this is no longer necessary, but since I had already
    ;; designed a fix for this problem, I've left it in here: we won't
    ;; run this code if our overlay property has already been set by a
    ;; previous run of the hook..
    (unless (overlay-get overlay 'my:markdown-fenced-code-block-indent)
      (overlay-put overlay 'my:markdown-fenced-code-block-indent indent)
      (when indent
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward (concat "^" indent) nil t)
            (replace-match "")
            (forward-line 1)))))))

(defun my:markdown-edit-code-block--put-back-indentation (start end)
  (when-let* ((indent (get-char-property start
                                         'my:markdown-fenced-code-block-indent))
              ;; We're going to be inserting text, so the end will
              ;; move, hence using a marker that will shift forward to
              ;; match each insert we do.
              (end-mark (copy-marker end)))
    (save-excursion
      (goto-char start)
      (forward-line 0)
      (cl-loop
        while (< (point) end-mark)
        unless (looking-at-p "\\s-*$") do (insert indent)
        always (zerop (forward-line 1))))))

(defun my:markdown-mode-setup-edit-code-block-indentation-hooks ()
  (add-hook 'edit-indirect-after-creation-hook
            #'my:markdown-edit-code-block--remove-indentation
            nil t)
  (add-hook 'edit-indirect-after-commit-functions
            #'my:markdown-edit-code-block--put-back-indentation
            nil t))

(add-hook 'markdown-mode-hook
          #'my:markdown-mode-setup-edit-code-block-indentation-hooks)
