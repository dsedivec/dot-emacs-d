;; -*- lexical-binding: t; -*-

;; When searching forward and you end the search successfully, put
;; point at the start of the search term, not the end.  I prefer this
;; for navigation.  All props to
;; http://emacswiki.org/emacs/IncrementalSearch from which this was
;; taken.
(defun my:isearch-ends-at-start-of-match ()
  (when (and isearch-forward
             (not isearch-mode-end-hook-quit)
             isearch-other-end)
    (goto-char isearch-other-end)))

(add-hook 'isearch-mode-end-hook 'my:isearch-ends-at-start-of-match)

(defun my:avy-isearch ()
  "avy-isearch but set isearch-other-end after a match is selected.

I need this to keep my isearch-mode-end-hook function working,
which leaves point at the beginning of a successful match rather
than the end.  Without the below wrapper around avy-isearch, we
end up moving to a potentially totally different match after
selecting a match with avy."
  (interactive)
  (let ((my:avy-isearch-real-avy-process (symbol-function 'avy--process)))
    (cl-letf (((symbol-function 'avy--process)
               (lambda (&rest args)
                 (prog1
                     (apply my:avy-isearch-real-avy-process args)
                   (save-match-data
                     (when (and isearch-forward
                                (re-search-forward isearch-string nil t))
                       (setq isearch-other-end (match-beginning 0))))))))
      (avy-isearch))))
