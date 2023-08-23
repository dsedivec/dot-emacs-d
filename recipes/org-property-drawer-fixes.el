;; -*- lexical-binding: t; -*-

;;; Utility functions

(defun my:org-back-to-heading-for-property-drawer ()
  ;; Copied from org-get-property-block and org-insert-property-drawer.
  (if (or (not (featurep 'org-inlinetask)) (org-inlinetask-in-task-p))
      (org-back-to-heading t)
    (org-with-limited-levels (org-back-to-heading t))))


;;; 1. Hide property drawers after they're inserted

;; Hide property drawers after they're inserted.  If your whole
;; subtree was hidden, keep it hidden.  Otherwise, at least hide the
;; damn drawer contents.

(defun my:org-hide-property-drawer-after-creation (orig-fun &rest args)
  (if (org-get-property-block)
      ;; Drawer already exists, orig-fun should do nothing, so we
      ;; will make sure we do nothing.
      (apply orig-fun args)
    (let (headline-level hide-whole-headline)
      (save-excursion
        (condition-case nil
            (progn
              ;; If this fails we'll swallow the error and go to
              ;; call orig-fun, which we expect to fail with the
              ;; same error when it tries this same thing.
              (my:org-back-to-heading-for-property-drawer)
              (setq headline-level (org-current-level)
                    hide-whole-headline (or (not (zerop (forward-line 1)))
                                            (outline-invisible-p)
                                            (and (outline-on-heading-p)
                                                 (eq (org-current-level)
                                                     headline-level)))))
          (error nil)))
      (prog1
          (apply orig-fun args)
        (cond (hide-whole-headline
               (my:org-back-to-heading-for-property-drawer)
               (outline-hide-subtree))
              (t
               (re-search-forward org-property-drawer-re)
               (org-fold-hide-drawer-toggle t)))))))

(advice-add 'org-insert-property-drawer :around
            #'my:org-hide-property-drawer-after-creation)


;;; 2. Fix invisibility when the property drawer is automatically deleted

;; Funny story: if the property drawer is the last thing under a
;; hidden headline, and you just deleted the last property,
;; org-entry-delete deletes the empty drawer, but it deletes the
;; *visible* newline after :END: rather than the invisible newline
;; at the end of the headline it belongs to.  This results in your
;; display getting fucked up, e.g. this:
;;
;;     * Test 1...
;;     * Test 2
;;
;; turns into this:
;;
;;     * Test 1...* Test 2
;;
;; So if you just deleted the property drawer and now your
;; headline/subtree ends with an invisible newline, we're going to
;; make that visible for you.

(declare-function org-inlinetask-in-task-p "org-inlinetask")

(define-advice org-entry-delete (:around
                                 (orig-fun pom &rest args)
                                 my:fix-newline-after-deleting-property-drawer)
  (let ((headline (org-with-point-at pom
                    (my:org-back-to-heading-for-property-drawer)
                    (point-marker)))
        (removed (apply orig-fun pom args))
        moved-overlay-point)
    (when removed
      (save-excursion
        (goto-char headline)
        (outline-end-of-heading)
        ;; Move past any invisibility.
        (while (and (< (point) (point-max))
                    (outline-invisible-p))
          (goto-char (next-single-char-property-change (point) 'invisible)))
        ;; We're either out of invisibility or at end of buffer.  If the
        ;; preceding invisible text ends with newline, and we're not
        ;; looking at a visible newline, let's make that preceding
        ;; invisible newline visible.
        (when (and (or (eobp) (not (eq (char-after (point)) ?\n)))
                   (eq (char-before (point)) ?\n))
          ;; Iterate over all the overlays at the immediately preceding
          ;; position.  One or more of them are causing invisibility.
          ;; (Hopefully just one, really.)
          (dolist (overlay (overlays-at (1- (point))))
            ;; Only modify invisibility overlays that end right here.
            (when (and (overlay-get overlay 'invisible)
                       (eq (overlay-end overlay) (point)))
              ;; Shrink the overlay to make its trailing newline visible.
              (move-overlay overlay (overlay-start overlay) (1- (point)))
              ;; Record where we are when we moved that overlay; see
              ;; below.  (This does the same thing every time it's
              ;; run--we're not moving point.  I'm just lazy.)
              (setq moved-overlay-point (point))))))
      (when (and (null pom) (equal (point) moved-overlay-point))
        ;; The property drawer was the last thing in the invisible
        ;; headline body.  Removing it and shifting the invisibility
        ;; overlay left point at the *next* heading.  Move back to the
        ;; heading we started on, hopefully just one character back.
        ;;
        ;; The difference is between having point at (point indicated
        ;; with | character)
        ;;
        ;;     * Test 1|...
        ;;     * Test 2
        ;;
        ;; versus
        ;;
        ;;     * Test 1...|
        ;;     * Test 2
        ;;
        ;; when you delete the property drawer.
        ;;
        ;; We don't muck around with this when pom is non-nil because
        ;; I have NFC what is going on in that case.  I haven't seen
        ;; it non-nil tonight.  Maybe it's non-nil when I delete a
        ;; property from an agenda buffer?  (Agenda buffer in column
        ;; mode?)
        (forward-char -1)))
    removed))
