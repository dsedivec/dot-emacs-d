;; -*- lexical-binding: t; -*-

;; I want dumb-jump to be the penultimate alternative before asking
;; for a TAGS file.  If I have a TAGS file, use that instead.  If any
;; "smarter" backend gets loaded, use that instead of dumb-jump.

(require 'cl-lib)

(defun my:dumb-jump-activate-unless-TAGS-exists ()
  (unless (and buffer-file-name
               (locate-dominating-file buffer-file-name "TAGS"))
    (if (local-variable-p 'xref-backend-functions)
        (progn
          (setq xref-backend-functions (copy-sequence xref-backend-functions))
          (let* ((insert-ptr (cl-member-if (lambda (el)
                                             (memq el '(t etags--xref-backend)))
                                           xref-backend-functions)))
            (if insert-ptr
                ;; Insert it before the (probably) default etags
                ;; backend.
                (setf (cdr insert-ptr) (cons (car insert-ptr) (cdr insert-ptr))
                      (car insert-ptr) #'dumb-jump-xref-activate)
              ;; No insertion point found, just put dumb-jump at the
              ;; end.
              (nconc xref-backend-functions '(dumb-jump-xref-activate)))))
      (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t))))

(add-hook 'find-file-hook #'my:dumb-jump-activate-unless-TAGS-exists)
