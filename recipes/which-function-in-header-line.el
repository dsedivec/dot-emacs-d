;; -*- lexical-binding: t; -*-

;; Move `which-function-mode' out of the mode line and into the header
;; line.
;;
;; We move it out of the mode line globally, but into the header line
;; only via a hook in `which-function-mode-hook'.  If you put it in
;; `header-line-format' globally, you'll have an empty, useless header
;; line in buffers where you're not using `which-function-mode'.

(defvar my:which-function-mode-line-spec nil)

(with-eval-after-load 'which-func
  (setq my:which-function-mode-line-spec
        (my:treepy-edit-mode-line-var (mode-line-misc-info zip)
          (eq (car-safe (treepy-node zip)) 'which-function-mode)
          (treepy-remove zip)))
  (unless my:which-function-mode-line-spec
    (warn "Could not find `which-function-mode' in `mode-line-misc-info'")))

(defun my:which-function-move-into-header ()
  (when my:which-function-mode-line-spec
    (make-local-variable 'header-line-format)
    (my:treepy-edit-mode-line-var
        (header-line-format zip)
      (equal (treepy-node zip) my:which-function-mode-line-spec)
      (treepy-remove zip))
    (push my:which-function-mode-line-spec header-line-format)))

(add-hook 'which-function-mode-hook #'my:which-function-move-into-header)
