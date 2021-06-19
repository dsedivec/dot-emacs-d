;; -*- lexical-binding: t; -*-

;; Fix for https://github.com/volrath/treepy.el/issues/9.

(require 'el-patch)

(el-patch-feature treepy)

(with-eval-after-load 'treepy
  (el-patch-defun treepy-remove (loc)
    "Remove the node at LOC.
Return the loc that would have preceded it in a depth-first
walk."
    (treepy--with-loc loc (context pnodes ppath l r)
      (if (not context)
          (error "Remove at top")
        (if (> (length l) 0)
            (let ((nloc (treepy--with-meta (cons (car l)
                                                 (treepy--context-assoc context
                                                                        ':l (cdr l)
                                                                        ':changed? t))
                                           (treepy--meta loc)))
                  (el-patch-remove (child nil)))
              (el-patch-remove
                (while (setq child (and (treepy-branch-p nloc) (treepy-children nloc)))
                  (setq nloc (treepy-rightmost child))))
              nloc)
          (treepy--with-meta
           (cons (treepy-make-node loc (car pnodes) r)
                 (and ppath (treepy--context-assoc context ':changed? t)))
           (treepy--meta loc))))))

  (el-patch-validate 'treepy-remove 'defun t))
