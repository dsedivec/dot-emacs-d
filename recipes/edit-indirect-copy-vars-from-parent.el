;; -*- lexical-binding: t; -*-

;; Copy variables from the parent buffer to the edit-indirect buffer.
;;
;; Primary use for this was needing to edit SQL in strings in Python,
;; and I wanted to override my usual tab indentation in SQL to
;; indenting with spaces when coming from a Python file with spaces.

(defvar my:edit-indirect-vars-to-clone
  '(indent-tabs-mode fill-column sql-product)
  "List of variables cloned from parent buffer into edit-indirect buffers.")

(defvar my:edit-indirect-cloned-parent-vars nil)

(make-variable-buffer-local 'my:edit-indirect-cloned-parent-vars)

(put 'my:edit-indirect-cloned-parent-vars 'permanent-local t)

(defun my:edit-indirect-restore-parent-cloned-vars ()
  (pcase-dolist (`(,var . ,value) my:edit-indirect-cloned-parent-vars)
    ;; Unfortunately, sql-mode needs special handling of the product,
    ;; apparently.
    (pcase var
      ((and 'sql-product (guard (derived-mode-p '(sql-mode))))
       (sql-set-product value))
      (_ (set (make-local-variable var) value)))))

(put 'my:edit-indirect-restore-parent-cloned-vars 'permanent-local-hook t)

(defun my:edit-indirect-set-up-cloned-vars (parent-buffer)
  (setq my:edit-indirect-cloned-parent-vars
        (mapcar (lambda (var)
                  (cons var (buffer-local-value var parent-buffer)))
                my:edit-indirect-vars-to-clone))
  (add-hook 'after-change-major-mode-hook
            #'my:edit-indirect-restore-parent-cloned-vars nil t))
