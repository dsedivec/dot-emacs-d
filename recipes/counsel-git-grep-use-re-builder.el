;; -*- lexical-binding: t; -*-

;; `counsel-git-grep-action' hard codes the use of `ivy--regex', which
;; breaks badly (due to `ivy--split') when I enter a search such as
;; "req\\(uest\\)?".  This patches `counsel-git-grep-action' to use
;; whatever re-builder was used in the search, which in my case is an
;; re-builder that knows to treat "req\\(uest\\)?" (for example) as a
;; literal regexp.

(require 'ivy)
(require 'swiper)

(eval-when-compile
  (require 'el-patch))

(el-patch-feature counsel)

(el-patch-defun counsel-git-grep-action (x)
  "Go to occurrence X in current Git repository."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let ((file-name (match-string-no-properties 1 x))
          (line-number (match-string-no-properties 2 x))
          (el-patch-add
            (regexp (funcall (ivy-state-re-builder ivy-last) ivy-text))))
      (find-file (expand-file-name
                  file-name
                  (ivy-state-directory ivy-last)))
      (goto-char (point-min))
      (forward-line (1- (string-to-number line-number)))
      (when (re-search-forward (el-patch-swap
                                 (ivy--regex ivy-text t)
                                 regexp)
                               (line-end-position) t)
        (when swiper-goto-start-of-match
          (goto-char (match-beginning 0))))
      (swiper--ensure-visible)
      (run-hooks 'counsel-grep-post-action-hook)
      (unless (eq ivy-exit 'done)
        (swiper--cleanup)
        (swiper--add-overlays (el-patch-swap
                                (ivy--regex ivy-text)
                                regexp))))))
