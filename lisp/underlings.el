;;; underlings.el --- Control your menu bar          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: extensions, lisp
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO: Need to make paths be (["foo" "bar"] "baz") not [("foo" bar")
;; "baz"] as they are now.

;;; Code:

(require 'subr-x)

(defun underlings--get-menu-item-info (menu-item)
  (pcase menu-item
    (`(,(and (pred symbolp) item-name)
       ,(and (pred stringp) item-string)
       . ,rest)
     (list item-name item-string (if (stringp (car-safe rest))
                                     (cdr rest)
                                   rest)))
    (`(,(and (pred symbolp) item-name)
       menu-item
       ,(and (pred stringp) item-string)
       . ,rest)
     (list item-name item-string (car-safe rest)))
    (`(menu-bar . ,binding)
     (list 'menu-bar nil binding))))

(defun underlings--menu-item-menu-p (menu-item)
  (keymapp (third (underlings--get-menu-item-info menu-item))))

(defun underlings--find-menu-items-in-keymap (map search-path)
  "Return a list of menu items from MAP which match SEARCH-PATH.

Each element of SEARCH-PATH is a symbol or a string.  It will be
matched against the \"item key\" of a menu item, or the menu
item's title.  Users of this function (other than this function
itself) will probably want to make `menu-bar' the first element
of SEARCH-PATH.

Each item in the returned list is (REAL-PATH . ITEM), where
\(car (last REAL-PATH)) is the ITEM's key in the keymap.  ITEM is
the complete item, not just its binding.  ITEM is not a copy: it
is the object stored in the keymap where it was found.  (This is
done on purpose, so that changes to ITEM from outside Underlings
will be reflected in `underlings-mode-map'."
  (cl-assert (keymapp map))
  (cl-assert (consp map))
  (cl-assert (eq (car map) 'keymap))
  (cl-assert (consp search-path))
  (cl-assert (seq-every-p (lambda (el) (or (symbolp el) (stringp el)))
                          search-path))
  (let ((search-name (car search-path))
        matches)
    ;; Skip `keymap' at head
    (setq map (cdr map))
    (while map
      (cond
        ((keymapp (car map))
         ;; Nested/composed keymap
         (setq matches
               (nconc matches
                      (underlings--find-menu-items-in-keymap (car map)
                                                             search-path))))
        ((keymapp map)
         ;; Parent keymap
         (setq matches (nconc matches (underlings--find-menu-items-in-keymap
                                       map
                                       search-path))
               ;; Done.
               map nil))
        (t
         ;; Normal binding, might be a relevant menu item
         (let ((elem (car map)))
           (cl-destructuring-bind (&optional item-name item-string binding)
               (underlings--get-menu-item-info elem)
             (when (and (not (memq binding '(nil undefined)))
                        (or (string= item-name search-name)
                            (string= item-string search-name)))
               (if (not (cdr search-path))
                   (push (cons (list (car elem)) (cdr elem)) matches)
                 ;; This would descend into prefix maps, but I don't
                 ;; think prefix maps are valid for menu items?  Are
                 ;; they...?
                 ;;
                 ;; (when (symbolp binding)
                 ;;   (setq binding (symbol-function binding)))
                 (when (keymapp binding)
                   (dolist (match (underlings--find-menu-items-in-keymap
                                   binding
                                   (cdr search-path)))
                     (setcar match (cons item-name (car match)))
                     (push match matches)))))))))
      (setq map (cdr map)))
    matches))

(defun underlings--undefine-menu-item (map path)
  (cl-loop
     with items = (underlings--find-menu-items-in-keymap map path)
     for (real-path . _item) in items
     do (define-key map (vconcat real-path) 'undefined)
     finally return items))

(defun underlings--add-props-to-menu-item (item &rest extra-props)
  (pcase item
    (`(,(and (pred stringp) item-string) ,(and (pred stringp) help) . ,binding)
     (append (list 'menu-item item-string binding)
             extra-props (list :help help)))
    (`(,(and (pred stringp) item-string) . ,binding)
     (cl-list* 'menu-item item-string binding extra-props))
    (`(menu-item ,(and (pred stringp) item-string) ,binding . ,props)
     (append (list 'menu-item item-string binding) extra-props props))
    (`(menu-item ,(and (pred stringp) item-string))
     (cl-list* 'menu-item item-string extra-props))
    (_ (error "Can't interpret item %S" item))))

(defun underlings--ensure-menu-at-dest (dest-map dest dest-path)
  (cl-assert (eq (aref dest-path 0) 'menu-bar))
  (cl-assert (= (length dest) (1- (length dest-path))))
  (let ((existing-path dest-path)
        leaf-keymap)
    (unless (keymapp (lookup-key dest-map [menu-bar]))
      (define-key dest-map [menu-bar] (make-sparse-keymap)))
    (while (memq (setq leaf-keymap (lookup-key dest-map existing-path))
                 '(nil undefined))
      (setq existing-path (seq-subseq existing-path 0 -1)))
    (cl-assert (> (length existing-path) 0))
    (cl-loop
       for n from (length existing-path) below (length dest-path)
       for comp in (seq-drop dest (1- (length existing-path)))
       do (let* ((comp-string (if (stringp comp) comp (symbol-name comp)))
                 (comp-name (if (string-match-p "^[^[:upper:]]*$" comp-string)
                                (capitalize comp-string)
                              comp-string)))
            (define-key-after dest-map (seq-subseq dest-path 0 (1+ n))
              (list 'menu-item comp-name
                    (setq leaf-keymap (make-sparse-keymap comp-name))))))
    leaf-keymap))

(defun underlings--resolve-keymap (mode-or-map &optional no-error)
  (cl-assert (or (keymapp mode-or-map) (symbolp mode-or-map)))
  (if (keymapp mode-or-map)
      mode-or-map
    (cl-loop
       for sym in (list mode-or-map (intern (format "%s-map" mode-or-map)))
       for val = (and (boundp sym) (symbol-value sym))
       when (keymapp val)
       return val
       finally
         (if no-error
             nil
           (error "Cannot find a keymap using %S" mode-or-map)))))

(defun underlings--delete-existing-items-and-nconc (keymap item-name
                                                    &optional mappings)
  (cl-assert (keymapp keymap))
  (let* ((last keymap)
         (ptr (cdr last)))
    (while ptr
      (let ((elem (car ptr)))
        (if (consp elem)
            (if (not (cond
                       ((eq (car elem) 'keymap)
                        (underlings--delete-existing-items-and-nconc elem
                                                                     item-name)
                        (equal elem '(keymap)))
                       (t
                        (eq (car elem) item-name))))
                ;; Advance
                (setq last ptr
                      ptr (cdr ptr))
              ;; Delete
              (setf ptr (cdr ptr)
                    (cdr last) ptr))
          (if (eq elem 'keymap)
              ;; Don't want to modify parent keymap, bail
              (setq ptr nil)
            ;; Advance
            (setq last ptr
                  ptr (cdr ptr))))))
    (setcdr last (nconc mappings (cdr last)))))

;;;###autoload
(cl-defun underlings-move-menu (mode-or-map menu-names dest
                                &key dest-map visible)
  (let* ((src-map (underlings--resolve-keymap mode-or-map))
         (menu-names (if (vectorp menu-names) menu-names (vector menu-names)))
         (dest (if (listp dest) dest (list dest)))
         (dest-path (vconcat (list 'menu-bar) (mapcar #'intern dest)))
         (dest-map (if dest-map
                       (underlings--resolve-keymap dest-map)
                     src-map))
         (leaf-map (underlings--ensure-menu-at-dest dest-map dest dest-path)))
    (cl-loop
       with ungrouped =
         (seq-mapcat (lambda (menu-name)
                       (let ((path (if (consp menu-name)
                                       (cons 'menu-bar menu-name)
                                     (list 'menu-bar menu-name))))
                         (underlings--undefine-menu-item src-map path)))
                     menu-names)
       for (real-path . pairs) in (seq-group-by #'car ungrouped)
       for first-menu-item = (cdar pairs)
       do
         (let ((visible (if (eq visible t) mode-or-map visible)))
           (if (or (<= (length pairs) 1)
                   ;; First item must itself be a menu item if
                   ;; successive menu items are going to even have a
                   ;; chance to matter.  (Behavior of Emacs determined
                   ;; experimentally.)
                   ;;
                   ;; Consider:
                   ;;
                   ;;     (keymap
                   ;;      (menu-bar .
                   ;;                (keymap
                   ;;                 (dest "Dest" .
                   ;;                       (keymap
                   ;;                        (keymap
                   ;;                         (child "Child1" . some-cmd))
                   ;;                        (keymap
                   ;;                         (child "Child2" .
                   ;;                                (keymap
                   ;;                                 (stuff "Stuff" .
                   ;;                                        stuff-cmd)))))))))
                   ;;
                   ;; You will only see Dest → Child1 in the menu bar.
                   ;; Dest → Child2 does not appear at all.  If Child1
                   ;; was itself a menu (i.e. bound to a keymap), both
                   ;; Child1 and Child2 would appear.
                   (not (underlings--menu-item-menu-p first-menu-item)))
               (define-key-after dest-map (vconcat dest-path (last real-path))
                 (if visible
                     (underlings--add-props-to-menu-item
                      first-menu-item
                      :visible visible)
                   first-menu-item))
             (let ((item-name (car (last real-path))))
               ;; *Delete* anything in `leaf-map' that is using our
               ;; item name, before we splice in one keymap per item we
               ;; found for this item name.  This makes us [closer to?]
               ;; idempotent, and prevents other existing items from
               ;; blocking the items we want to add right here.
               (underlings--delete-existing-items-and-nconc
                leaf-map
                item-name
                (mapcar (lambda (pair)
                          (cons 'keymap
                                (cons item-name
                                      (if visible
                                          (underlings--add-props-to-menu-item
                                           (cdr pair)
                                           :visible visible)
                                        (cdr pair)))))
                        pairs))))))
    (force-mode-line-update t)))

(defun underlings--set-function (func-name func docstring)
  (declare (indent 1))
  (setf (symbol-function func-name) func)
  (put func-name 'function-documentation docstring)
  func-name)

(defun underlings--make-mover-func-name (mode-or-map menu-names
                                         &optional prefix)
  (let ((name (format "%s-menu-%s"
                      (if (symbolp mode-or-map)
                          mode-or-map
                        "unknown")
                      menu-names)))
    (setq name (replace-regexp-in-string "[^[:alnum:]]+" "-" name))
    (setq name (replace-regexp-in-string "[^[:alnum:]]+$" "" name))
    ;; Purposely using `intern' not `make-symbol' so our hook function
    ;; names are recognized as the same by `add-hook' et al.
    (intern (concat "underlings--" (or prefix "move") "-" (downcase name)))))

;;;###autoload
(cl-defun underlings-define-menu-mover (mode-or-map menu-names dest
                                        &key func-name dest-map visible)
  (underlings--set-function
      (or func-name (underlings--make-mover-func-name mode-or-map menu-names))
    (lambda ()
      (underlings-move-menu mode-or-map menu-names dest
                            :dest-map dest-map
                            :visible visible))
    (concat (format "In %s, move the %S menu(s) into %s.\n\n"
                    (if (symbolp mode-or-map)
                        mode-or-map
                      "<some keymap>")
                    menu-names
                    (if dest (prin1-to-string dest) "the menu bar"))
            "Generated for you by Underlings.")))

(defun underlings--get-hook-var (mode-or-map)
  (unless (symbolp mode-or-map)
    (error "Must provide either mode name (not a keymap), or else HOOK-VAR"))
  (intern (format "%s-hook" mode-or-map)))

;;;###autoload
(cl-defun underlings-define-one-time-menu-mover-hook
    (mode-or-map menu-names dest
     &key hook-var local func-name dest-map visible)
  (unless func-name
    (setq func-name (underlings--make-mover-func-name mode-or-map menu-names
                                                      "move-once")))
  (unless hook-var
    (setq hook-var (underlings--get-hook-var mode-or-map)))
  (underlings--set-function func-name
    (lambda ()
      (underlings-move-menu mode-or-map menu-names dest
                            :dest-map dest-map
                            :visible visible)
      (remove-hook hook-var func-name local))
    (concat
     (format "In %S, move the %S menu(s) into %s and remove this hook.\n\n"
             (if (symbolp mode-or-map)
                 mode-or-map
               "<some keymap>")
             menu-names
             (if dest (prin1-to-string dest) "the menu bar"))
     (format "I will remove myself from%s hook `%s'.\n\n"
             (if local " the local value of" "")
             hook-var)
     "Generated for you by Underlings.")))

;;;###autoload
(cl-defun underlings-move-menu-with-one-time-hook
    (mode-or-map menu-names dest
     &key no-immediate hook-var local func-name dest-map visible)
  (let* ((hook-var (or hook-var (underlings--get-hook-var mode-or-map)))
         (func-name
          (underlings-define-one-time-menu-mover-hook mode-or-map
                                                      menu-names
                                                      dest
                                                      :hook-var hook-var
                                                      :local local
                                                      :func-name func-name
                                                      :dest-map dest-map
                                                      :visible visible)))
    ;; 100 should mean "put me at the very end of the list".
    (add-hook hook-var func-name 100 local)
    ;; Call the mover function right now if it looks like the keymap
    ;; is available.
    (when (and (not no-immediate)
               (keymapp (underlings--resolve-keymap mode-or-map t)))
      (underlings-move-menu mode-or-map menu-names dest
                            :dest-map dest-map
                            :visible visible))
    func-name))

;;;###autoload
(cl-defun underlings-move-menu-after-load (file mode-or-map menu-names dest
                                           &key dest-map visible)
  (eval-after-load file
    (lambda ()
      (underlings-move-menu mode-or-map menu-names dest
                            :dest-map dest-map
                            :visible visible))))

(provide 'underlings)
;;; underlings.el ends here
