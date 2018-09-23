;; -*- lexical-binding: t; -*-

;;; Teach org-babel-read-element to descend into dynamic blocks

;; This is useful for referring to clocktable results from inside an
;; elisp source block, e.g. for filtering.  Here's an example:
;;
;;     #+NAME: timesheet
;;     #+BEGIN: my:daily-time-summary :scope file-with-archives :tstart "<2018-09-09 Sun>" :tend "<2018-09-16 Sun>"
;;     #+CAPTION: Generated at [2018-09-11 Tue 16:13]
;;     | [2018-09-09 Sun 00:00] | 0.00h |
;;     | [2018-09-10 Mon 00:00] | 1.00h |
;;     | [2018-09-11 Tue 00:00] | 2.00h |
;;     | [2018-09-12 Wed 00:00] | 0.00h |
;;     | [2018-09-13 Thu 00:00] | 0.00h |
;;     | [2018-09-14 Fri 00:00] | 0.00h |
;;     | [2018-09-15 Sat 00:00] | 0.00h |
;;     |------------------------+-------|
;;     | *Total time*           | 6.66h |
;;     #+TBLFM: @>$>=vsum(@<$>..@-I$>)
;;     #+END:
;;
;;     #+BEGIN_SRC elisp :var table=timesheet :colnames yes
;;     (print (car table))
;;     #+END_SRC
;;
;; Eval that source block without the below and I estimate it'll fail
;; to find "timesheet".

(define-advice org-babel-read-element
    (:before-until (element) my:read-dynamic-block-elements)
  (when (eq (org-element-type element) 'dynamic-block)
    (org-babel-read-element
     (org-with-wide-buffer
      (goto-char (org-element-property :contents-begin element))
      (org-element-at-point)))))
