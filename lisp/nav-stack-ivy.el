;;; nav-stack-ivy.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords:
;; Version: 0.1
;; Package-Requires: ((ivy "0") (hydra "0"))

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

;;

;;; Code:

(require 'ivy)
(require 'hydra)
(require 'nav-stack)

(defun nav-stack-ivy--go-to-candidate (candidate-str)
  (pcase-let ((`(,index . ,location)
               (get-text-property 0 'indexed-location candidate-str)))
    (unless (equal (nav-stack-location-marker location) (point-marker))
      (nav-stack-debug "ivy trying to find index=%S location=%S" index location)
      (setq index (nav-stack-find-location-index location index))
      (nav-stack-debug "ivy will go to index=%S location=%S" index location)
      (if index
          (nav-stack-go-to-location-at-index index)
        (nav-stack-go-to-location location))
      (unless nav-stack-quiet
        (message "Moved to location %s"
                 (nav-stack-get-stack-position-message))))))

(defun nav-stack-ivy--format-location (location)
  (let* ((marker (nav-stack-location-marker location))
         (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char marker)
        (let* ((line-num (line-number-at-pos)))
          (format "%s:%s: %s"
                  (propertize (buffer-name)
                              'face 'ivy-grep-info)
                  (propertize (number-to-string line-num)
                              'face 'ivy-grep-line-number)
                  (buffer-substring (line-beginning-position)
                                    (line-end-position))))))))

;;;###autoload
(defun nav-stack-ivy (&optional initial-input frame-or-window extra-predicates)
  (interactive)
  (let* ((lines-seen (make-hash-table :test 'equal))
         (cand-head (list nil))
         (cand-ptr cand-head)
         (cand-idx 0)
         (pointer (nav-stack-get-pointer frame-or-window))
         (best-pointer-delta most-positive-fixnum)
         (preselect 0))
    (dolist (indexed-location
              (nav-stack-get-navigable-locations frame-or-window
                                                 extra-predicates))
      (let ((cand (nav-stack-ivy--format-location (cdr indexed-location))))
        (unless (gethash cand lines-seen)
          (puthash cand t lines-seen)
          (setq cand (propertize cand 'indexed-location indexed-location))
          (setq cand-ptr (setcdr cand-ptr (list cand)))
          (let ((pointer-delta (abs (- pointer (car indexed-location)))))
            (when (< pointer-delta best-pointer-delta)
              (setq preselect cand-idx
                    best-pointer-delta pointer-delta)))
          (cl-incf cand-idx))))
    (ivy-read "Nav-Stack: "
              (cdr cand-head)
              :require-match t
              :preselect preselect
              :initial-input initial-input
              :action #'nav-stack-ivy--go-to-candidate
              :caller 'nav-stack-ivy)))

;;;###autoload
(defun nav-stack-ivy-other-buffer (&optional initial-input frame-or-window)
  (interactive)
  (nav-stack-ivy initial-input frame-or-window
                 '(nav-stack-other-buffer-predicate)))

(defvar nav-stack-hydra--universal-mode 'any)

(defun nav-stack-hydra--get-universal-mode-predicates ()
  (cl-ecase nav-stack-hydra--universal-mode
    (any nil)
    (same '(nav-stack-same-buffer-predicate))
    (other '(nav-stack-other-buffer-predicate))))

(defun nav-stack-hydra-universal-mode-cycle ()
  (interactive)
  (setq nav-stack-hydra--universal-mode
        (cl-case nav-stack-hydra--universal-mode
          (same 'other)
          (other 'any)
          (t 'same))))

;;; XXX This should be a defcustom but then I'd need a defgroup and my
;;; namespace prefix usage isn't proper in this file, so I'm lazy and
;;; it's a defvar for now.  Please fix me.
(defvar nav-stack-hydra-num-surrounding-lines-shown 3)

(defun nav-stack-hydra--get-move-lines (&optional frame-or-window)
  (let* ((extra-predicates (nav-stack-hydra--get-universal-mode-predicates))
         (num-lines nav-stack-hydra-num-surrounding-lines-shown)
         (backwards
          (mapcar (lambda (index-loc) (cl-list* "" index-loc))
                  (nav-stack-get-next-move-locations nil num-lines
                                                     frame-or-window
                                                     extra-predicates)))
         (forwards
          (mapcar (lambda (index-loc) (cl-list* "" index-loc))
                  (nav-stack-get-next-move-locations t num-lines
                                                     frame-or-window
                                                     extra-predicates)))
         (shared-middle
          (when (equal (nav-stack-location-marker (cdar backwards))
                       (nav-stack-location-marker (cdar forwards)))
            (pop backwards))))
    (cond (shared-middle
           (setf (car (first forwards)) "p/n")
           (when (second forwards)
             (setf (car (second forwards)) "n")))
          (forwards
           (setf (car (first forwards)) "n")))
    (when backwards
      (setf (car (first backwards)) "p"))
    (cl-loop
       with result = ""
       for (prefix index . location) in (nconc (nreverse backwards) forwards)
       do (setq result
                (format "%s%-4d %-3s %s\n"
                        result
                        index
                        prefix
                        (nav-stack-ivy--format-location location)))
       ;; Hydra doesn't escape its own %'s, *probably* a bug I should
       ;; report upstream.
       finally return (if (zerop (length result))
                          "stack is empty"
                        (replace-regexp-in-string "%" "%%" result)))))

(defun nav-stack-hydra--decorate-universal-mode (mode)
  (if (eq nav-stack-hydra--universal-mode mode)
      (propertize (symbol-name mode) 'face '(:inverse-video t))
    (symbol-name mode)))

;;;###autoload (autoload 'nav-stack-hydra/body "nav-stack-hydra" nil t)
(defhydra nav-stack-hydra
    (:hint none
     :body-pre (setq nav-stack-hydra--universal-mode 'any))
  "
^^prev/next
  _,_ / _._  any buffer                  _p_/_n_  back/forward _u_niversal mode
  _b_ / _B_  same buffer                 mode: _ma_: ?ma?  _ms_: ?ms?  _mo_: ?mo?
  _o_ / _O_  other buffer                _i_vy     push _L_og     _C_lear all     _q_uit
  _a_ / _e_  beginning/end of stack      %s(nav-stack-get-stack-position-message) (_s_et, _l_ist)
================================================================================
%s(nav-stack-hydra--get-move-lines)"
  ("," nav-stack-go-back)
  ("." nav-stack-go-forward)
  ("b" nav-stack-go-back-same-buffer)
  ("B" nav-stack-go-forward-same-buffer)
  ("o" nav-stack-go-back-other-buffer)
  ("O" nav-stack-go-forward-other-buffer)
  ("ma" (setq nav-stack-hydra--universal-mode 'any)
        (nav-stack-hydra--decorate-universal-mode 'any))
  ("ms" (setq nav-stack-hydra--universal-mode 'same)
        (nav-stack-hydra--decorate-universal-mode 'same))
  ("mo" (setq nav-stack-hydra--universal-mode 'other)
        (nav-stack-hydra--decorate-universal-mode 'other))
  ("u" nav-stack-hydra-universal-mode-cycle)
  ("TAB" nav-stack-hydra-universal-mode-cycle)
  ("<tab>" nav-stack-hydra-universal-mode-cycle)
  ("p" (nav-stack-go-back nil
                          (nav-stack-hydra--get-universal-mode-predicates)))
  ("n" (nav-stack-go-forward nil
                             (nav-stack-hydra--get-universal-mode-predicates)))
  ("a" (nav-stack-set-pointer 0))
  ("e" (nav-stack-set-pointer t))
  ("s" nav-stack-set-pointer)
  ("c" nav-stack-compact-all)
  ("l" nav-stack-list :exit t)
  ("L" nav-stack-view-push-log :exit t)
  ("C" nav-stack-reset-all-stacks :exit t)
  ("i" nav-stack-ivy :exit t)
  ("q" nil nil))

(provide 'nav-stack-ivy)
;;; nav-stack-ivy.el ends here
