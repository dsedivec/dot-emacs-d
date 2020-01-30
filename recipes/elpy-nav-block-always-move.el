;; -*- lexical-binding: t; -*-

;; Elpy navigate-by-block bindings should always try to move me
;; somewhere.!

(defun my:elpy-nav-blocks-always-do-something (orig-fun)
  (let ((col (current-column)))
    (when (> col (current-indentation))
      (back-to-indentation))
    (prog1
        (funcall orig-fun)
      (move-to-column col))))

(advice-add 'elpy-nav-forward-block :around
            #'my:elpy-nav-blocks-always-do-something)

(advice-add 'elpy-nav-backward-block :around
            #'my:elpy-nav-blocks-always-do-something)
