(deftheme dsedivec
    "My customizations on top of the default Emacs theme.")

(custom-theme-set-faces
 'dsedivec
 '(avy-background-face ((t (:background "white" :foreground "gray70"))))
 '(avy-lead-face
   ((t (:background "#be2745" :foreground "white" :weight normal))))
 '(avy-lead-face-0
   ((t (:background "#4f57f9" :foreground "white" :weight normal))))
 '(avy-lead-face-1
   ((t (:background "gray" :foreground "white" :weight normal))))
 '(avy-lead-face-2
   ((t (:background "darkseagreen2" :foreground "black" :weight normal))))
 ;; XXX not using bm these days
 '(bm-face ((t (:background "DarkOrange1" :distant-foreground "white"))))
 '(bmkp-light-non-autonamed
   ((t (:background "DarkSeaGreen1" :distant-foreground "grey20"))))
 '(highlight ((t (:background "darkseagreen2" :distant-foreground "black"))))
 '(isearch ((t (:background "magenta3" :foreground "white"))))
 '(mode-line ((t (:background "#228" :foreground "white" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :inverse-video t :box (:line-width -1 :color "grey75") :weight light))))
 '(org-done
   ((t (:background "ForestGreen" :foreground "#e4fee4" :weight bold))))
 ;; '(org-mode-line-clock ((t nil)))
 '(org-todo
   ((t (:background "#ac1879" :foreground "#FFFFDBD8F387" :weight bold))))
 '(rst-level-1 ((t (:background "grey80"))))
 '(rst-level-2 ((t (:background "grey85"))))
 '(rst-level-3 ((t (:background "grey90"))))
 '(rst-level-4 ((t (:background "grey95"))))
 '(rst-level-5 ((t nil)))
 '(rst-level-6 ((t nil)))
 '(secondary-selection ((t (:background "pale turquoise" :distant-foreground "black"))))
 '(sh-heredoc ((t (:foreground "#DF5169350000"))))
 '(web-mode-html-tag-face ((t (:foreground "dark violet"))))
 '(web-mode-symbol-face ((t (:foreground "#D64880F70000")))))

(provide-theme 'dsedivec)
