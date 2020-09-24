;; -*- lexical-binding: t; -*-

;; I'm pretty sure that `modus-vivendi-theme-override-colors-alist' is
;; not used (whether partially or at all) when the theme is byte
;; compiled, so this function lets me go out of my way to delete any
;; byte-compiled version of the theme, such as what you get if you
;; install it with `package-install'.
;;
;; (Note that modus-vivendi is included in
;; forthcoming-as-of-this-writing Emacs 28, and it is not byte
;; compiled there, which seems telling.)

(require 'custom)

(defvar my:never-compiled-themes nil)

(defun my:delete-compiled-theme (theme-name)
  (let* ((theme-name (if (symbolp theme-name)
                         (symbol-name theme-name)
                       theme-name))
         ;; This is taken from `load-theme'.
         (theme-file (locate-file (concat theme-name "-theme.el")
                                  (custom-theme--load-path)
                                  '("" "c")))
         ;; You might end up with "foo-theme.elcc" here, but we end up
         ;; never using this variable if theme-file ends in "elc", so
         ;; it doesn't matter.
         (theme-elc (concat theme-file "c")))
    (cond
      ((string-match-p (rx ".elc" eos) theme-file)
       (warn (concat "%s will probably be loaded from"
                     " compiled file %S which has no matching source file")
             theme-name theme-file))
      ((not (file-exists-p theme-elc))
       ;; Nothing to do
       nil)
      ((not (string-match-p (rx bos
                                (literal (file-name-as-directory
                                          (expand-file-name package-user-dir))))
                            theme-file))
       (warn (concat "%s will probably be loaded from"
                     " compiled file %S, which is not under `package-user-dir'"
                     " and so we don't delete the elc file")
             theme-name theme-file))
      (t
       ;; elc file exists, it's under `package-user-dir', let's delete
       ;; it.
       (delete-file theme-elc)
       (message "init.el deleted %S" theme-elc)))))

(defun my:maybe-delete-compiled-theme (theme-name &rest _)
  (when (and (custom-theme-name-valid-p theme-name)
             (memq theme-name my:never-compiled-themes))
    (my:delete-compiled-theme theme-name)))

(advice-add 'load-theme :before #'my:maybe-delete-compiled-theme)
