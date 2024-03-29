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


;;; 3. Newlines after drawer

;; org-mode started leaving extra newline at end of drawer, and it's
;; annoying to me.  This removes it.  See comment below.

(el-patch-feature 'org)

(with-eval-after-load 'org
  (el-patch-defun org-log-beginning (&optional create)
    "Return expected start of log notes in current entry.
When optional argument CREATE is non-nil, the function creates
a drawer to store notes, if necessary.  Returned position ignores
narrowing."
    (org-with-wide-buffer
     (let ((drawer (org-log-into-drawer)))
       (cond
         (drawer
          (org-end-of-meta-data)
          (let ((regexp (concat "^[ \t]*:" (regexp-quote drawer) ":[ \t]*$"))
                (end (if (org-at-heading-p) (point)
                       (save-excursion (outline-next-heading) (point))))
                (case-fold-search t))
            (catch 'exit
              ;; Try to find existing drawer.
              (while (re-search-forward regexp end t)
                (let ((element (org-element-at-point)))
                  (when (org-element-type-p element 'drawer)
                    (let ((cend  (org-element-contents-end element)))
                      (when (and (not org-log-states-order-reversed) cend)
                        (goto-char cend)))
                    (throw 'exit nil))))
              ;; No drawer found.  Create one, if permitted.
              (when create
                ;; Unless current heading is the last heading in buffer
                ;; and does not have a newline, `org-end-of-meta-data'
                ;; should move us somewhere below the heading.
                ;; Avoid situation when we insert drawer right before
                ;; first "*".  Otherwise, if the previous heading is
                ;; folded, we are inserting after visible newline at
                ;; the end of the fold, thus breaking the fold
                ;; continuity.
                (unless (eobp)
                  (when (org-at-heading-p) (backward-char)))
                (org-fold-core-ignore-modifications
                  (unless (bolp) (insert-and-inherit "\n"))
                  (let ((beg (point)))
                    ;; Changes here to prevent extra newline added
                    ;; after :END:.  Maybe ca. commit f63ff07441.
                    (insert-and-inherit ":" drawer (el-patch-swap ":\n:END:\n"
                                                                  ":\n:END:"))
                    (el-patch-add
                      (if (eolp)
                          (forward-line 1)
                        (insert-and-inherit "\n")))
                    (org-indent-region beg (point))
                    (org-fold-region (line-end-position -1) (1- (point)) t (if (eq org-fold-core-style 'text-properties) 'drawer 'outline)))))
              (end-of-line -1))))
         (t
          (org-end-of-meta-data org-log-state-notes-insert-after-drawers)
          (let ((endpos (point)))
            (skip-chars-forward " \t\n")
            (forward-line 0)
            (unless org-log-states-order-reversed
              (org-skip-over-state-notes)
              (skip-chars-backward " \t\n")
              (forward-line 1))
            ;; When current headline is at the end of buffer and does not
            ;; end with trailing newline the above can move to the
            ;; beginning of the headline.
            (when (< (point) endpos) (goto-char endpos))))))
     (if (bolp) (point) (line-beginning-position 2))))

  (el-patch-validate 'org-log-beginning 'defun t))
