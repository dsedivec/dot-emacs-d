;; -*- lexical-binding: t; -*-

;; markdown-mode's `markdown-enter-key' uses `newline' rather than
;; `newline-and-indent'.  Too bad, because the latter would
;; `delete-horizontal-space' for us before inserting the newline.  To
;; illustrate the pain this can cause, imagine hitting enter twice
;; starting with point at the end of "bar" in this example:
;;
;;     * foo
;;
;;       bar
;;
;; (You may need my recipe that auto-indents based on the indent of
;; the last line.)  Without this advice applying
;; `delete-horizontal-space', you will end up with two spaces on an
;; otherwise blank line following the "bar" line.

(defun my:markdown-enter-key-remove-trailing-space (&rest _args)
  (delete-horizontal-space t))

(advice-add 'markdown-enter-key :before
            #'my:markdown-enter-key-remove-trailing-space)
