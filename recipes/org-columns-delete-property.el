;; -*- lexical-binding: t; -*-

;; Delete property in columns mode

(defvar org-agenda-overriding-columns-format)
(defvar org-columns-inhibit-recalculation)
(defvar org-columns-current-fmt)
(defvar org-columns-map)
(declare-function org-columns--call "org-colview")
(declare-function org-columns-redo "org-colview")
(declare-function org-columns-update "org-colview")

(defun my:org-columns-delete-property (&rest key)
  "Delete property value for the current column in columns mode."
  (interactive)
  ;; Most of this function is copied from org-columns-edit-value.
  (let* ((col (current-column))
         (bol (line-beginning-position))
         (pom (or (get-text-property bol 'org-hd-marker) (point)))
         (key (or key (get-char-property (point) 'org-columns-key)))
         (action (lambda () (org-entry-delete pom key))))
    (cond
      ((member key org-special-properties)
       (user-error "Cannot delete special property %S" key))
      ((eq major-mode 'org-agenda-mode)
       (org-columns--call action)
       ;; The following let preserves the current format, and makes
       ;; sure that in only a single file things need to be updated.
       (let* ((org-agenda-overriding-columns-format org-columns-current-fmt)
              (buffer (marker-buffer pom))
              (org-agenda-contributing-files
               (list (with-current-buffer buffer
                       (buffer-file-name (buffer-base-buffer))))))
         (org-agenda-columns)))
      (t
       (let ((inhibit-read-only t))
         (with-silent-modifications
           (remove-text-properties (max (point-min) (1- bol))
                                   (line-end-position)
                                   '(read-only t)))
         (org-columns--call action))
       (let ((org-columns-inhibit-recalculation))
         (org-columns-redo))
       (org-columns-update key)
       (org-move-to-column col)))))

(with-eval-after-load 'org-colview
  (bind-keys :map org-columns-map
             ("DEL" . my:org-columns-delete-property)
             ;; I get this in columns mode in an agenda buffer.  Mac port
             ;; uses <delete>, NS port uses <deletechar>.
             ("<delete>" . my:org-columns-delete-property)
             ("<deletechar>" . my:org-columns-delete-property)))
