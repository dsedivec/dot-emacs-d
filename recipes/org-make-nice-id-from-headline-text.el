;; -*- lexical-binding: t; -*-
;;
;; Rather than using a UUID when generating an ID for a headline, this
;; tries to make an ID from the headline text, which can make for
;; nicer/more descriptive IDs.  Only lightly tested!

(require 'org)
(require 'org-id)

(defvar my:org-new-id-from-headline 40)

(defun my:org-new-id-from-headline (&optional prefix pom)
  (org-with-point-at pom
    (when-let ((heading (org-get-heading t t t t)))
      (let* ((heading-as-id (replace-regexp-in-string "[^-a-z0-9]+" "_"
                                                      (downcase heading)))
             (heading-as-id
              (substring-no-properties heading-as-id
                                       0
                                       (min my:org-new-id-from-headline
                                            (length heading-as-id))))
             (prefix (unless (eq prefix 'none)
                       (or prefix org-id-prefix)))
             (prefix (concat (or prefix "") (if prefix ":" ""))))
        (unless (string-match-p "\\`_?\\'" heading-as-id)
          (cl-loop
             ;; Note this purposely will only suffix the heading with
             ;; numbers starting with _2 (not _1).
             for n from 1 below 100
             for id = (concat prefix heading-as-id)
             then (concat prefix heading-as-id "_" (number-to-string n))
             unless (org-id-find id) return id))))))

(defun my:org-id-get-create-id-from-headline (orig-fun &rest args)
  (cl-letf* ((orig-org-id-new (symbol-function 'org-id-new))
             ((symbol-function 'org-id-new)
              (lambda (&optional prefix)
                (or (my:org-new-id-from-headline prefix)
                    (funcall orig-org-id-new prefix)))))
    (apply orig-fun args)))

(advice-add 'org-id-get :around #'my:org-id-get-create-id-from-headline)
