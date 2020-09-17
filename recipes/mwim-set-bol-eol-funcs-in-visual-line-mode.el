;; -*- lexical-binding: t; -*-

(defun my:visual-line-mode-set-mwim-functions ()
  (pcase-dolist (`(,var . ,func)
                 '((mwim-beginning-of-line-function . beginning-of-visual-line)
                   (mwim-end-of-line-function . end-of-visual-line)))
    (if visual-line-mode
        (set (make-local-variable var) func)
      (when (and (local-variable-p var)
                 (eq (buffer-local-value var (current-buffer)) func))
        (kill-local-variable var)))))

(my:add-hooks 'visual-line-mode-hook #'my:visual-line-mode-set-mwim-functions)
