;; -*- lexical-binding: t; -*-

;; AUCTeX needs "style hooks" generated for style files.  You could
;; just run `TeX-auto-generate-global' over your TeX installation and
;; generate them for you, but I never remember to do that.  This will
;; call `TeX-auto-generate' when loading a style file for which an
;; automatic .el file has never been created.
;;
;; Important functions to look at if you want to understand this:
;;
;; `TeX-run-style-hooks'
;; `TeX-auto-generate'
;; `TeX-load-style' (which is what I advise below)
;;
;; Variable `TeX-style-hook-list' holds the style hooks for loaded
;; styles.
;;
;; The regexps searched for live around `TeX-auto-regexp-list'.
;; Functions such as `LaTeX-common-initialization' might alter the
;; list to include, e.g., LaTeX-specific regexps.  The members of this
;; list then do things like setting up buffer-local variables
;; (including hash tables, oh goodie) containing all the information
;; about commands and labels and such that are parsed from the current
;; buffer.

;; I want variable `TeX-auto-default' but I"m not quite sure of the
;; right way to make sure AUCTeX gets loaded.  I am hoping this will
;; work for users who install AUCTeX via package.el, or using a
;; distro-provided AUCTeX.  But really, just load this file inside
;; "(with-eval-after-load 'tex ...)" or the like.
;;
;; This better at least make the byte compiler happy.
;;
;; Note that Emacs does not, as of this writing, ship a tex.el.
(require 'tex)

(defvar my:TeX-auto-generate-on-demand-dir TeX-auto-default)

(defun my:TeX-find-style-file (style)
  (let (temp-buffer)
    (unwind-protect
         (progn
           (setq temp-buffer (generate-new-buffer "kpsewhich"))
           (ignore-errors
             (call-process "kpsewhich" nil (list temp-buffer nil) nil
                           (concat style ".sty")))
           (with-current-buffer temp-buffer
             (goto-char (point-min))
             (end-of-line)
             (when (> (point) (point-min))
               (buffer-substring (point-min) (point)))))
      (when temp-buffer
        (kill-buffer temp-buffer)))))

(defun my:TeX-find-styles-and-auto-parse (orig-fun style &rest args)
  ;; I have witnessed STYLE having a trailing space.  Should maybe
  ;; file upstream bug report.
  (setq style (string-trim style))
  ;; If STYLE was already searched for, or if STYLE looks like it
  ;; might contain path information, defer entirely to
  ;; `TeX-load-style'.  Note that, for paths, `TeX-load-style' will do
  ;; a recursive call to itself after stripping out path information
  ;; from STYLE, so you'll end up back here with a nice non-path STYLE
  ;; value.
  (if (or (assoc style TeX-style-hook-list)
          (string-match-p "[./\\]" style))
      (apply orig-fun style args)
    ;; STYLE is a bare style name.  Let TeX-load-style try to load
    ;; the style information.
    (apply orig-fun style args)
    ;; Did it find something?
    (unless (cdr (assoc style TeX-style-hook-list))
      ;; No, it didn't, or at least it didn't produce any hook
      ;; information.  Let's try and auto-parse the style file.  But
      ;; first we have to try and *find* the style file.
      (when-let ((style-file (my:TeX-find-style-file style)))
        (unless (file-directory-p my:TeX-auto-generate-on-demand-dir)
          (message "Creating directory %s" my:TeX-auto-generate-on-demand-dir)
          (condition-case err
              (make-directory my:TeX-auto-generate-on-demand-dir t)
            (t
             (user-error (concat "Can not create directory %S,"
                                 " check value of variable"
                                 " `my:TeX-auto-generate-on-demand-dir': %S")
                         my:TeX-auto-generate-on-demand-dir
                         err))))
        ;; Found a style file.
        (message "Auto-parsing %S on demand" style-file)
        ;; Generate the "foo.el" file for style "foo".
        (TeX-auto-generate style-file my:TeX-auto-generate-on-demand-dir)
        ;; Now load that generated "foo.el" file.  This call to
        ;; `TeX-load-style-file' is what `TeX-load-style' does
        ;; normally.
        (TeX-load-style-file
         (expand-file-name style my:TeX-auto-generate-on-demand-dir))))))

(advice-add 'TeX-load-style :around #'my:TeX-find-styles-and-auto-parse)
