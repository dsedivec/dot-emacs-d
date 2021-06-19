;; -*- lexical-binding: t; -*-

;; Better error message than wrong-type-argument when dumb-jump can't
;; determine the project root.

(require 'el-patch)

(declare-function dumb-jump-get-config "dumb-jump")
(defvar dumb-jump-project)

(el-patch-feature dumb-jump)

(with-eval-after-load 'dumb-jump
  (el-patch-defun dumb-jump-get-project-root (filepath)
    "Keep looking at the parent dir of FILEPATH until a denoter file/dir is found."
    (el-patch-let
        (($proj (or
                 dumb-jump-project
                 (locate-dominating-file filepath #'dumb-jump-get-config)
                 dumb-jump-default-project)))
      (el-patch-wrap 2 0
        (let ((root $proj))
          (el-patch-add
            (unless root
              (user-error "dumb-jump cannot figure out project root")))
          (s-chop-suffix
           "/"
           (expand-file-name (el-patch-swap $proj root)))))))

  (el-patch-validate 'dumb-jump-get-project-root 'defun t))
