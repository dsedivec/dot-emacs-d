;; -*- lexical-binding: t; -*-

;; Replace some vc-mode mode line names like "Git" and "Hg" with cute
;; icons.  Requires third-party package all-the-icons.

(defvar my:vc-mode-line-icons-alist
  `((Git . ,(all-the-icons-alltheicon "git" :v-adjust 0 :height 0.8))
    (Hg . ,(all-the-icons-fileicon "hg" :v-adjust -0.1 :height 0.8)))
  "Icons for various vc backends.")

(define-advice vc-mode-line
    (:around (orig-fun &rest args) my:vc-mode-line-icons)
  (let ((backend (apply orig-fun args)))
    (when-let ((icon (and vc-mode
                          (alist-get backend my:vc-mode-line-icons-alist))))
      (setq vc-mode
            (replace-regexp-in-string
             (concat "^\\(\\s-*\\)\\(" (symbol-name backend) "\\)")
             (lambda (match)
               ;; Have to do a big dance here to copy all the
               ;; properties from the text we're replacing, but then
               ;; add/override any from the icon, especially the font
               ;; family for icon fonts.
               (let ((match-props (text-properties-at 0 (match-string 2 match)))
                     (icon-props (text-properties-at 0 icon))
                     (icon-len (length icon)))
                 (set-text-properties 0 icon-len match-props icon)
                 (add-text-properties 0 icon-len icon-props icon)
                 (concat (match-string 1 match) icon)))
             vc-mode))
      ;; Superfluous?
      (force-mode-line-update))
    backend))
