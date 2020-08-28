;; -*- lexical-binding: t; -*-

;; Save/restore frame configuration along with persp-mode.  Adding
;; data to perspectives inspired by
;; https://gist.github.com/gilbertw1/8d963083efea41f28bfdc85ed3c93eb4.
;; We mustn't leave any frame parameters in the frameset that
;; persp-mode thinks will print unreadably, otherwise it will not save
;; our frameset when it saves perspectives.
;;
;;
;; THEMES
;;
;; I change which Emacs theme is in use based on macOS dark mode.
;; This means that, if I exit Emacs while macOS is in dark mode, the
;; initial frame will be dark---but subsequent frames will be my
;; default theme (which is to say, not really any theme but the
;; default Emacs theme with my overlays, see
;; themes/dsedivec-theme.el).  To get around this, I'm applying the
;; technique suggested at https://superuser.com/a/1155381 and
;; https://gist.github.com/vividsnow/0609b55bd684d325e7cb: if
;; `my:persp-mode-save-restore-themes' is non-nil, we'll save the
;; active theme(s) and enable any missing ones at startup.  Actually,
;; since my process for going in/out of dark mode requires
;; disabling/enabling the light theme and enabling/disabling the light
;; theme, we'll also disable themes that weren't enabled.  In other
;; words, `custom-enabled-themes' will hopefully have the same value
;; when restoring is complete as it did when the persp-mode save was
;; created.
;;
;;
;; CHILD FRAMES
;;
;; frameset.el will merrily save/restore child frames, such as those
;; produced by posframe.el (and its users such as company-posframe or
;; ivy-posframe).  I had this happen even when ivy-posframe's frame
;; wasn't visible when I exited.  When I restored I then had a bizarre
;; floating child frame following me around.  Therefore I have
;; `frameset-save' exclude child frames.  Note, however, that this
;; might be a bad idea: there are perhaps some child frames that
;; should be saved?  Right now, though, I think my only use of child
;; frames are from posframe, and I have no circumstances where I want
;; a posframe child frame saved/restored.

(require 'cl-lib)
(require 'subr-x)
(require 'custom)

(require 'persp-mode)

(defvar my:persp-mode-save-restore-themes t
  "If non-nil, save active themes and restore them at load time.")

(defun my:persp-mode-serializable-p (obj)
  (and (or (memq (type-of obj) '(bool-vector
                                 char-table
                                 cons
                                 float
                                 integer
                                 string
                                 symbol
                                 vector)))
       ;; Can't pass an improper list to `seq-every-p', but
       ;; `sequencep' returns T for improper lists.
       (if (consp obj)
           (and (my:persp-mode-serializable-p (car obj))
                (my:persp-mode-serializable-p (cdr obj)))
         (or (not (sequencep obj))
             (seq-every-p #'my:persp-mode-serializable-p obj)))))

(defun my:persp-mode-unserializable-frame-parameters (&optional frame)
  (mapcar #'car (seq-remove (lambda (pair)
                              (my:persp-mode-serializable-p (cdr pair)))
                            (frame-parameters frame))))

(defun my:persp-mode-all-unserializable-frame-parameters ()
  (seq-reduce #'cl-union
              (mapcar #'my:persp-mode-unserializable-frame-parameters
                      (frame-list))
              nil))

(defun my:persp-mode-filter-persp-frame-property (current _filtered
                                                  _parameters saving)
  "frameset filter to properly save frames' 'persp' properties."
  (cons (car current)
        (if saving
            (let ((perspective (cdr current)))
              (when (and perspective (persp-p perspective))
                (persp-name perspective)))

          (when (cdr current)
            ;; Not documented, but if a perspective with the name
            ;; already exists, this returns it rather than creating a
            ;; new perspective.
            (persp-add-new (cdr current))))))

(defun my:persp-mode-get-frameset ()
  (let ((frameset-filter-alist
         (append '((persp . my:persp-mode-filter-persp-frame-property))
                 (mapcar (lambda (prop) (cons prop :never))
                         (my:persp-mode-all-unserializable-frame-parameters))
                 frameset-filter-alist)))
    (frameset-save nil
                   :predicate (lambda (frame)
                                (null (frame-parameter frame 'parent-frame))))))

(defun my:persp-mode-save-frame-configuration (&rest _)
  (set-persp-parameter 'my:frame-configuration (my:persp-mode-get-frameset)
                       nil)
  (when (and my:persp-mode-save-restore-themes (boundp 'custom-enabled-themes))
    (set-persp-parameter 'my:enabled-themes custom-enabled-themes nil)))

(defun my:persp-mode-restore-frame-configuration (&rest _)
  (condition-case-unless-debug err
      (progn
        (when-let ((frameset (persp-parameter 'my:frame-configuration nil)))
          (delete-persp-parameter 'my:frame-configuration nil)
          (let ((frameset-filter-alist
                 (append '((persp . my:persp-mode-filter-persp-frame-property))
                         frameset-filter-alist)))
            (frameset-restore frameset :reuse-frames t :cleanup-frames t))))
    (t
     (warn "Failed to restore frames: %S" err)))
  (condition-case-unless-debug err
      (when-let ((themes (and my:persp-mode-save-restore-themes
                              (persp-parameter 'my:enabled-themes nil))))
        ;; First disable themes we shouldn't have.
        (dolist (theme custom-enabled-themes)
          (unless (or (memq theme themes)
                      ;; The "user" and "changed" themes are special,
                      ;; see the docstring on `custom-known-themes'.
                      ;; I don't think we should ever see these in
                      ;; `custom-enabled-themes', but just in case I
                      ;; do *not* want to try disabling them.
                      (memq theme '(user changed)))
            (disable-theme theme)))
        ;; Now enable themes we should have.  (It's possible we should
        ;; really disable all themes, then re-enable them in the right
        ;; order, since I think the order of loading does matter, but
        ;; for now I don't need to worry about this, personally.)
        (dolist (theme (reverse themes))
          (unless (custom-theme-enabled-p theme)
            ;; I've had "weirdness" when calling `load-theme' on a
            ;; theme that was already loaded.  In particular,
            ;; `load-theme' followed by `disable-theme' for a current
            ;; theme seemed to break.  For that reason, call
            ;; `load-theme' only if the theme is not known.
            (if (custom-theme-p theme)
                (enable-theme theme)
              (load-theme theme)))))
    (t
     (warn "Failed to restore themes: %S" err))))

(with-eval-after-load 'persp-mode
  (add-hook 'persp-before-save-state-to-file-functions
            #'my:persp-mode-save-frame-configuration)
  (add-hook 'persp-after-load-state-functions
            #'my:persp-mode-restore-frame-configuration
            t))
