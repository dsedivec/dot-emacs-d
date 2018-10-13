;; -*- lexical-binding: t; -*-

;; HOLD MY BEER
;;
;; AUCTeX doesn't recursively parse style files, from everything I've
;; read.  That's unsatisfactory.  Let's try and implement recursive
;; style file parsing.  What could go wrong?
;;
;; This relies on kpsewhich to find style files.  You may need to set
;; up e.g. TEXINPUTS appropriately.

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

(define-advice TeX-load-style
    (:around (orig-fun style &rest args) my:find-styles-and-auto-parse)
  ;; I have witnessed STYLE having a trailing space.  Should maybe
  ;; file upstream bug report.
  (setq style (string-trim style))
  (if (or (assoc style TeX-style-hook-list)
          ;; Better not have a path, I don't do paths.
          (string-match-p "[./\\]" style))
      ;; If STYLE was already searched for, or if STYLE looks like
      ;; it might contain path information, defer to TeX-load-style.
      ;; Note that TeX-load-style will do a recursive call to itself
      ;; after stripping out path information from STYLE, if
      ;; applicable.
      (apply orig-fun style args)
    ;; STYLE is a bare style name.  Let TeX-load-style try to load
    ;; the style information.
    (apply orig-fun style args)
    ;; Did it find something?
    (unless (cdr (assoc style TeX-style-hook-list))
      ;; No, it didn't, or at least it didn't produce any hook
      ;; information.  Let's try a little harder.  First let's try
      ;; and find the style file.
      (message "Trying hard to load information for style %S" style)
      (when-let ((style-file (my:TeX-find-style-file style)))
        ;; Found a style file.
        (let ((TeX-parse-self t)
              style-buf)
          (unwind-protect
               (progn
                 (with-current-buffer
                     (setq style-buf
                           (find-file-noselect style-file t nil nil))
                   ;; This will try to write the auto/*.el file.
                   ;; I'm not sure if this is necessary in a buffer
                   ;; that we just created: AUCTeX may already parse
                   ;; it at open.  For an already-open buffer,
                   ;; however, and especially when the user doesn't
                   ;; have `TeX-parse-self' turned on, I suspect
                   ;; this is highly necessary.  And maybe also a
                   ;; little rude, if we just did something naughty
                   ;; to your already-open buffer, but let's ignore
                   ;; that for now.
                   (TeX-auto-write)))
            (when style-buf
              (kill-buffer style-buf))))))))
