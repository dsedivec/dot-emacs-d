;;; hideshow-tab.el --- Make TAB toggle visibility in hs-minor-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: convenience
;; Version: 0.1

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

;; This code was inspired by org-mode and wanting to hit TAB on a
;; hidden "..." to show it.  I believe
;; https://github.com/shanecelis/hideshow-org does something similar,
;; but I wrote this solution because that mode was last touched in
;; 2012, and because I wanted to write some code.
;;
;; When `hideshow-tab' is called:
;;
;; * If point is on a hide/show hidden overlay, it calls
;;   `hs-toggle-hiding' to (presumably) show it.
;;
;; * If it's being called from something other than a key bound to
;;   `hideshow-tab', such as from `execute-extended-command' (M-x),
;;   then do `hs-toggle-hiding'.  (But why are you doing this?)
;;
;; * Otherwise, it tries to do what command would normally have been
;;   called for whatever `this-command-keys-vector' reports for this
;;   command.  For example, if you bind TAB to `hideshow-tab' then
;;   there's a good chance it will call `indent-for-tab-command'.
;;
;; * If calling the normal command for your key didn't do anything, as
;;   determined by `hideshow-tab-detect-action-functions', then it
;;   calls `hs-toggle-hiding'.

;;; Code:

(require 'hideshow)

(defgroup hideshow-tab nil
  "Magic visibility cycling in `hs-minor-mode'."
  :prefix "hideshow-tab-"
  :group 'hideshow)

(defcustom hideshow-tab-detect-action-functions
  '(hideshow-tab-detect-buffer-modified-tick
    hideshow-tab-detect-point-moved)
  "List of functions called to determine if the normal binding did anything.
This is important because if your normal binding for your
`hideshow-tab' key does something, then `hideshow-tab' will not
try to hide/show something; otherwise it will.

Each function in this list will be called before the normal
command binding with nil as its only argument.  The function must
return some kind of non-nil state.  After calling the normal
command binding, this function may be called again with the state
it previously returned.  In this latter case, the function should
return non-nil if the normal command took some kind of action in
the buffer, such as modifying it, or else nil if not.  In this
second set of calls, these functions are called unless/until one
returns non-nil, in which case `hideshow-tab' will not attempt to
toggle visibility."
  :type '(repeat
          (choice
           (function-item hideshow-tab-detect-buffer-modified-tick)
           (function-item hideshow-tab-detect-point-moved)
           (function-item hideshow-tab-detect-indentation-chars-changed)
           function)))

(defun hideshow-tab-detect-buffer-modified-tick (state)
  "Return non-nil if `buffer-modified-tick' is changed by the command."
  (if state
      (/= state (buffer-modified-tick))
    (buffer-modified-tick)))

(defun hideshow-tab-detect-point-moved (state)
  "Return non-nil if point is moved by the command."
  (if state
      (not (equal state (point)))
    (point)))

(defun hideshow-tab-detect-indentation-chars-changed (state)
  (let ((indent (save-excursion
                  (back-to-indentation)
                  (buffer-substring-no-properties (line-beginning-position)
                                                  (point)))))
    (if state
        (not (equal state indent))
      (point))))

;;;###autoload
(defun hideshow-tab ()
  "Call the normal command for this key, or else `hs-toggle-hiding'.
If point is on a hideshow overlay then always `hs-toggle-hiding'.
If the key(s) used to call this command (`this-command-keys') are
not bound to this command, then we call
`hs-toggle-hiding'.  (This last condition happens if you call
this command via \\[execute-extended-command] for example.)

Otherwise, we call whatever command, if any, that the command
keys would have been bound to with `hs-minor-mode' off.
Afterwards, if any of the functions in
`hideshow-tab-detect-action-functions' (which see) report that
the keys' normal command has taken an action of some
sort (ex. modifying the buffer), then we do nothing else.

Otherwise and finally, if the normal command didn't do
anything (ex. line was already indented), then we call
`hs-toggle-hiding' at point."
  (interactive)
  (let ((command-keys (this-command-keys-vector)))
    (if (or (hs-overlay-at (point))
            (not (eq (key-binding command-keys) this-command)))
        (hs-toggle-hiding)
      (let* ((normal-binding (let ((hs-minor-mode nil))
                               (key-binding (this-command-keys-vector))))
             (detectors (when normal-binding
                          (mapcar (lambda (func)
                                    (cons func (funcall func nil)))
                                  hideshow-tab-detect-action-functions))))
        (unless (and normal-binding
                     (progn
                       (call-interactively normal-binding)
                       (seq-some (lambda (func-and-data)
                                   (funcall (car func-and-data)
                                            (cdr func-and-data)))
                                 detectors)))
          (hs-toggle-hiding))))))

;; BONUS FUN
;;
;; `hs-toggle-hiding' uses `event-end' to try and figure out where you
;; clicked.  However, `event-end' seems to return `posn-at-point' when
;; I trigger `hs-toggle-hiding' with a keyboard event, and
;; `posn-at-point' seems to return the point at the end of an
;; invisible overlay if point is in such an overlay---you know, like
;; the kind of overlays hideshow creates.  If you have el-patch
;; installed, we'll use it here to work around this behavior, so that
;; hitting TAB *in* a hideshow hidden block (rather than *in front* of
;; it) actually works.
;;
;; See also Emacs bug #4094, which was not resolved.

(when (require 'el-patch nil t)
  (el-patch-feature hideshow)

  (el-patch-defun hs-toggle-hiding (&optional e)
    "Toggle hiding/showing of a block.
See `hs-hide-block' and `hs-show-block'.
Argument E should be the event that triggered this action."
    (interactive)
    (hs-life-goes-on
     (el-patch-wrap 2 0
       (when e
         (posn-set-point (event-end e))))
     (if (hs-already-hidden-p)
         (hs-show-block)
       (hs-hide-block))))

  (el-patch-validate 'hs-toggle-hiding 'defun t))

(provide 'hideshow-tab)
;;; hideshow-tab.el ends here
