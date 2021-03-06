;;; frame-resize.el --- Resize frame to fit desired windows widths  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Version: 1.0
;; Keywords: convenience, frames, windows

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

;; The idea is that you populate `frame-resize-window-size-functions'
;; with some functions that, when passed a window, will return your
;; desired (width . height) for that window in columns and lines,
;; respectively.  Then call `frame-resize' whenever you want in order
;; to resize the frame in an attempt to give all windows your desired
;; size.

;; In addition, there is global minor mode `auto-frame-resize-mode',
;; which will call `frame-resize' automatically after any command in
;; `auto-frame-resize-commands'.

;;; Code:

(require 'subr-x)

(defgroup frame-resize nil
  "Settings for frame-resize, which resizes frames according to their windows."
  :group 'frames)

(defcustom frame-resize-window-size-functions nil
  "List of functions called to determine the desired size of a window.
`frame-resize' will call functions in this list with a window.  A
function may return nil if it does not have desired dimensions
for the window, or else a list of (width height), where width is
given in columns and height in lines (see `frame-char-width' and
`frame-char-height', respectively).  The first dimensions
returned by any function in this list will be used for a given
window and subsequent functions will be ignored for that
window."
  :group 'frame-resize
  :type '(repeat function))

(defcustom frame-resize-ignore-frame-functions
  '(frame-resize-frame-fullscreen-p
    frame-resize-terminal-frame-p)
  "List of functions that may prevent `frame-resize` from resizing a frame.
Before resizing a frame, `frame-resize' calls each function in
this list with the frame to be resized.  If any function returns
non-nil, following functions are not called and `frame-resize'
terminates immediately (and normally, not with an error)."
  :group 'frame-resize
  :type '(repeat function))

(defcustom auto-frame-resize-commands
  '(split-window-right
    delete-window
    delete-other-windows
    imenu-list-smart-toggle)
  "List of commands that will trigger `frame-resize'."
  :group 'frame-resize
  :type '(repeat function))

(defvar frame-resize-debug nil
  "Output debugging messages when non-nil.")

(defun frame-resize--debug-message (msg &rest args)
  (when frame-resize-debug
    (let ((inhibit-message t))
      (apply #'message (concat "frame-resize: " msg) args))))

(defun frame-resize-frame-fullscreen-p (frame)
  "Return non-nil if FRAME is full screen or maximized.
Returns nil if FRAME is merely fullwidth or fullheight."
  (memq (frame-parameter frame 'fullscreen) '(fullboth maximized)))

(defun frame-resize-terminal-frame-p (frame)
  "Return non-nil if FRAME is a terminal frame."
  (eq (framep frame) t))

(defun frame-resize--get-frame-max-text-size-pixels (&optional frame)
  "Return (max-width . max-height) for the text area of FRAME, in pixels.
Calculation for \"text width\" taken from the Elisp manual,
section on frame geometry.  \"Text width\" is what is set by
`set-frame-size' and friends."
  (let* ((frame (or frame (selected-frame)))
         (workarea (frame-monitor-workarea frame))
         (workarea-width (nth 2 workarea))
         (workarea-height (nth 3 workarea)))
    (cons (- workarea-width
             (- (frame-outer-width) (frame-inner-width))
             (frame-scroll-bar-width frame)
             (frame-fringe-width frame))
          (- workarea-height
             (- (frame-outer-height frame) (frame-inner-height frame))
             (frame-scroll-bar-height frame)))))

(defun frame-resize--find-frame-and-window-sizes (tree)
  "Return the frame size delta needed given the windows in TREE.
Returns a list (width-delta height-delta . managed-windows).
managed-windows is a list of (window width height) triples,
specifying the given window's target size according to some
function in `frame-resize-window-size-functions'.  Width is
measured in columns and height is measured in lines.  Only
windows for which a size was returned by some function in
`frame-resize-window-size-functions' will be found in the list of
managed-windows."
  (if (windowp tree)
      (if-let ((target-size (run-hook-with-args-until-success
                             'frame-resize-window-size-functions tree)))
          (let* ((target-wd (car target-size))
                 (target-ht (cadr target-size))
                 (wd-delta (if target-wd
                               (- target-wd (window-width tree))
                             0))
                 (ht-delta (if target-ht
                               (- target-ht (window-height tree))
                             0)))
            (list wd-delta ht-delta (list tree target-wd target-ht)))
        '(0 0))
    (let* ((vertical-combo (car tree))
           (wd-delta nil)
           (ht-delta nil)
           managed-windows)
      (dolist (window (cddr tree))
        (pcase-let
            ((`(,child-wd-delta ,child-ht-delta . ,child-managed-windows)
               (frame-resize--find-frame-and-window-sizes window)))
          (when child-managed-windows
            (setq managed-windows (nconc managed-windows
                                         child-managed-windows))
            (when child-wd-delta
              (setq wd-delta
                    (cond
                      ((null wd-delta) child-wd-delta)
                      ;; We err on the side of larger frames.  This
                      ;; could become configurable in the future?  Not
                      ;; sure if that makes sense.  (Consider where
                      ;; wd-delta and child-wd-delta have different
                      ;; signs, for a more extreme case.)
                      (vertical-combo (max wd-delta child-wd-delta))
                      (t (+ wd-delta child-wd-delta)))))
            (when child-ht-delta
              (setq ht-delta
                    (cond
                      ((null ht-delta) child-ht-delta)
                      ((not vertical-combo) (max ht-delta child-ht-delta))
                      (t (+ ht-delta child-ht-delta))))))))
      (if managed-windows
          (nconc (list wd-delta ht-delta) managed-windows)
        '(0 0)))))

(defun frame-resize--clamp-window-resize-delta (window delta
                                                &optional horizontal)
  "Clamp DELTA to a valid value for WINDOW.
If HORIZONTAL is non-nil then DELTA is clamped to a valid width
delta for WINDOW.  Otherwise, DELTA is clamped to a valid height
delta for WINDOW.

This function should return 0 when a window cannot be resized."
  (if (< delta 0)
      (max delta (- (window-min-delta window horizontal)))
    (min delta (window-max-delta window horizontal))))

;;;###autoload
(defun frame-resize (&optional frame)
  "Resize and move FRAME according to `frame-resize-window-size-functions'.
The frame may be made larger or smaller in either height or
width, and `frame-resize' attempts to keep it in roughly the same
position within the viewport of the monitor that FRAME
dominates (see the documentation for
`display-monitor-attributes-list')."
  (interactive)
  (unless frame
    (setq frame (selected-frame)))
  (unless (run-hook-with-args-until-success
           'frame-resize-ignore-frame-functions frame)
    (pcase-let*
        ((tree (car (window-tree frame)))
         (`(,wd-delta ,ht-delta . ,managed-windows)
          (frame-resize--find-frame-and-window-sizes tree))
         (wd-delta-pixels (* wd-delta (frame-char-width frame)))
         (ht-delta-pixels (* ht-delta (frame-char-height frame)))
         (`(,max-wd-pixels . ,max-ht-pixels)
          (frame-resize--get-frame-max-text-size-pixels frame))
         (new-wd-pixels (min (+ (frame-text-width frame) wd-delta-pixels)
                             max-wd-pixels))
         (new-ht-pixels (min (+ (frame-text-height frame) ht-delta-pixels)
                             max-ht-pixels))
         (`(,pos-left ,pos-top ,pos-right ,pos-bottom)
          (frame-edges frame 'outer-edges))
         (`(,min-left ,min-top ,workarea-width ,workarea-height)
          (frame-monitor-workarea frame))
         (max-right (+ min-left workarea-width))
         (max-bottom (+ min-top workarea-height))
         ;; right-overflow/bottom-overflow hold the number of pixels
         ;; that would be off-screen if we keep the center where it is
         ;; after resize.  See their use in determining
         ;; new-left/new-top, below.
         (right-overflow
          (max 0 (- (+ pos-right (/ wd-delta-pixels 2)) max-right)))
         (new-left (cond
                     ((= pos-left min-left)
                      ;; Frame is currently at the left of the screen,
                      ;; try and keep it there.
                      pos-left)
                     ((= pos-right max-right)
                      ;; Frame is currently at the right of the
                      ;; screen, try and keep it there.
                      (- pos-left wd-delta-pixels))
                     (t
                      ;; Try to keep the center of the frame in the
                      ;; same horizontal position after resizing.  We
                      ;; may not be able to do that if the new width
                      ;; would put the frame outside of the bounds of
                      ;; the screen, hence the use of right-overflow.
                      (max min-left (- pos-left
                                       (/ wd-delta-pixels 2)
                                       right-overflow)))))
         (bottom-overflow
          (max 0 (- (+ pos-bottom (/ ht-delta-pixels 2)) max-bottom)))
         (new-top (cond
                    ((= pos-top min-top)
                     ;; Frame is currently at the top of the screen,
                     ;; keep it stuck to the top.
                     pos-top)
                    ((= pos-bottom max-bottom)
                     ;; Frame is currently at the bottom of the
                     ;; screen, keep it stuck to the bottom.
                     (- pos-top ht-delta-pixels))
                    (t
                     ;; Try to keep the center of the frame in the
                     ;; same vertical position after resizing.  We may
                     ;; not be able to do that if the new height would
                     ;; put the frame outside of the bounds of the
                     ;; screen, hence the use of bottom-overflow.
                     (max min-top (- pos-top
                                     (/ ht-delta-pixels 2)
                                     bottom-overflow))))))
      (frame-resize--debug-message (concat "currently top left %d, %d,"
                                           " bottom right %d %d")
                                   pos-top pos-left pos-bottom pos-right)
      (frame-resize--debug-message (concat "max top left %d, %d,"
                                           " max bottom right %d, %d")
                                   min-top min-left max-bottom max-right)
      (frame-resize--debug-message (concat "Δw=%s px, Δh=%s px,"
                                           " max w=%s px, max h=%s px")
                                   wd-delta-pixels ht-delta-pixels
                                   max-wd-pixels max-ht-pixels)
      (frame-resize--debug-message "new w=%s px, new h=%s px"
                                   new-wd-pixels new-ht-pixels)
      (frame-resize--debug-message (concat "→ over=%s px, ← new=%s px,"
                                           " ↓ over=%s px, ↑ new=%s px")
                                   right-overflow new-left
                                   bottom-overflow new-top)
      ;; Resize and reposition the frame.
      (set-frame-size frame new-wd-pixels new-ht-pixels t)
      (set-frame-position frame new-left new-top)
      ;; Resize windows in our newly-resized frame.
      (let (un-preserve-size-calls)
        (unwind-protect
             (pcase-dolist (`(,window ,win-wd ,win-ht) managed-windows)
               ;; Note that clamping the window size here prevents
               ;; errors from `window-resize'.  May be that we should
               ;; tell the user about this.  Also possible that we
               ;; should check this *before* we compute the frame's
               ;; new size.  However, I'm not sure if
               ;; `window-min-delta'/`window-max-delta' would work
               ;; right *before* the frame is resized.
               (let* ((win-wd-delta
                       (if win-wd
                           (frame-resize--clamp-window-resize-delta
                            window (- win-wd (window-width window)) t)
                         0))
                      (win-ht-delta
                       (if win-ht
                           (frame-resize--clamp-window-resize-delta
                            window (- win-ht (window-height window)))
                         0)))
                 ;; When we resize a window we `window-preserve-size'
                 ;; its new dimension(s) (if necessary) and then undo
                 ;; it later.  In my mind, at least, this prevents us
                 ;; from resizing window N, then resizing window N+1,
                 ;; but now Emacs has resized window N in a way we
                 ;; don't want.
                 (frame-resize--debug-message "%S wants %sx%s, Δ %sx%s"
                                              window
                                              win-wd win-ht
                                              win-wd-delta win-ht-delta)
                 (when (not (zerop win-wd-delta))
                   (window-resize window win-wd-delta t)
                   (unless (window-preserved-size window t)
                     (window-preserve-size window t t)
                     (push (list window t nil) un-preserve-size-calls)))
                 (when (not (zerop win-ht-delta))
                   (window-resize window win-ht-delta nil)
                   (unless (window-preserved-size window nil)
                     (window-preserve-size window nil t)
                     (push (list window nil nil) un-preserve-size-calls))))))
        ;; Now undo all the `window-preserve-size' calls we did above.
        (dolist (args un-preserve-size-calls)
          ;; We *really* want to try and undo everything we did.
          ;; Don't let an error from a single call kill us.
          (with-demoted-errors
              "`frame-resize' error undoing size preservation: %S"
            (apply #'window-preserve-size args)))))))

;; I considered using `window-configuration-change-hook' instead of
;; `post-command-hook'.  I decided against it for two reasons:
;;
;; First, I was worried that changing the frame/window sizes within
;; `window-configuration-change-hook' was probably a bad idea.  The
;; docs lightly warn that this could be a bad idea.  I could see some
;; sort of loop happening.
;;
;; Second, I was worried it would fire too often.  Our
;; `post-command-hook' is firing *constantly*, of course, but it's
;; very terse.  I fear `window-configuration-change-hook' could fire
;; very often itself, such as anytime the minibuffer gets invoked, but
;; then it would haul off and call `frame-resize' which is probably
;; considerably more expensive than our post-command hook function.

(defun auto-frame-resize--maybe-resize-frame ()
  "Call `frame-resize' if `this-command' is in `auto-frame-resize-commands'."
  (with-demoted-errors
      "Error in `auto-frame-resize--maybe-resize-frame' %S"
    (when (memq this-command auto-frame-resize-commands)
      (frame-resize))))

;;;###autoload
(define-minor-mode auto-frame-resize-mode
    "Automatically resize frames after certain commands."
  :group 'frame-resize
  :global t
  (funcall (if auto-frame-resize-mode #'add-hook #'remove-hook)
           'post-command-hook #'auto-frame-resize--maybe-resize-frame))

(provide 'frame-resize)
;;; frame-resize.el ends here
