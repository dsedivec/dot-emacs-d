;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(require 'persp-mode)

;; Save/restore frame configuration along with persp-mode.  Adding
;; data to perspectives inspired by
;; https://gist.github.com/gilbertw1/8d963083efea41f28bfdc85ed3c93eb4.
;; We mustn't leave any frame parameters in the frameset that
;; persp-mode thinks will print unreadably, otherwise it will not save
;; our frameset when it saves perspectives.
;;
;; CHILD FRAMES
;;
;; frameset.el will merrily save/restore child frames, such as those
;; produced by posframe.el (and its users such as company-posframe or
;; ivy-posframe).  I had this happen even when ivy-posframe's frame
;; wasn't visible when I exited.  When I restored I then had a bizarre
;; floating child frame following me around.
;;
;; I don't really know what to do about this right now.  The easy
;; answer is "don't save/restore child frames", which can be done via
;; the PREDICATE argument to `frameset-save'.  I have not yet
;; implemented that, though, since I don't use posframe now, and
;; because I am not at all sure that child frames *shouldn't* be
;; saved/restored, generally speaking.  Which is to say, I don't know
;; the right way to solve this right now, and it doesn't affect me, so
;; I'm ignoring it.

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
            (when (persp-p (cdr current))
              (persp-name (cdr current)))

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
    (frameset-save nil)))

(defun my:persp-mode-save-frame-configuration (&rest _)
  (set-persp-parameter 'my:frame-configuration (my:persp-mode-get-frameset)
                       nil))

(defun my:persp-mode-restore-frame-configuration (&rest _)
  (condition-case err
      (when-let ((frameset (persp-parameter 'my:frame-configuration nil)))
        (delete-persp-parameter 'my:frame-configuration nil)
        (let ((frameset-filter-alist
               (append '((persp . my:persp-mode-filter-persp-frame-property))
                       frameset-filter-alist)))
          (frameset-restore frameset :reuse-frames t :cleanup-frames t)))
    (t
     (warn "failed to restore frames: %S" err))))

(with-eval-after-load 'persp-mode
  (add-hook 'persp-before-save-state-to-file-functions
            #'my:persp-mode-save-frame-configuration)
  (add-hook 'persp-after-load-state-functions
            #'my:persp-mode-restore-frame-configuration
            t))
