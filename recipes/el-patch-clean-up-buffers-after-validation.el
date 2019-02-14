;; -*- lexical-binding: t; -*-

;; `el-patch-validate' can/will open buffers and leave them open,
;; which is irritating.
;;
;; I am amused that use of el-patch itself is probably the best way to
;; fix this problem, though really I should just kick this upstream.
;; Originally I put some advice around `el-patch-validate' that added
;; a temporary `find-file-hook' that watched for files being opened.
;; Unfortunately, at one point (startup, actually) I called
;; `el-patch-validate' and somehow a timer/hook that opened some
;; unrelated files fired in the middle of `el-patch-validate' (I
;; think), and I ended up killing a bunch of buffers I cared about.
;; That's how we landed on the below fix.

(require 'el-patch)

(el-patch-defun el-patch-locate-variable (definition)
  "Return the source code of DEFINITION.
DEFINITION is a list starting with `defvar' or similar."
  (el-patch-let (($finder (find-variable-noselect (nth 1 definition))))
    (el-patch-swap
      (el-patch-wrap-locator
        $finder)
      (let ((my:existing-buffers (buffer-list))
            my:el-patch-buf)
        (prog1
            (el-patch-wrap-locator
              (let ((my:buffer-point $finder))
                (setq my:el-patch-buf (car my:buffer-point))
                my:buffer-point))
          ;; Note that `el-patch-wrap-locator' does a "horrifying
          ;; bit of hackery" to try and make sure it gets a fresh
          ;; buffer, not an existing one, but it doesn't work.  I
          ;; think `find-file-visit-truename' might be to blame.
          ;; Anyway, we have to make sure not to kill an existing
          ;; buffer.  This is ugly.
          (when (and my:el-patch-buf
                     (not (memq my:el-patch-buf my:existing-buffers)))
            (kill-buffer my:el-patch-buf)))))))

(el-patch-defun el-patch-locate-function (definition)
  "Return the source code of DEFINITION.
DEFINITION is a list starting with `defun' or similar."
  (el-patch-let (($finder
                  (find-function-noselect (nth 1 definition) 'lisp-only)))
    (el-patch-swap
      (el-patch-wrap-locator
        $finder)
      (let ((my:existing-buffers (buffer-list))
            my:el-patch-buf)
        (prog1
            (el-patch-wrap-locator
              (let ((my:buffer-point $finder))
                (setq my:el-patch-buf (car my:buffer-point))
                my:buffer-point))
          (when (and my:el-patch-buf
                     (not (memq my:el-patch-buf my:existing-buffers)))
            (kill-buffer my:el-patch-buf)))))))

(el-patch-validate 'el-patch-locate-variable 'defun t)
(el-patch-validate 'el-patch-locate-function 'defun t)
