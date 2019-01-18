;; -*- lexical-binding: t; -*-

;; `which-func-update' always tries to update `selected-window'.  When
;; you're using, say, `swiper', the minibuffer is selected.  The
;; result is that `which-function-mode' in the buffer you're searching
;; doesn't get updated as you move around your buffer with Swiper,
;; which can make it harder-than-necessary to figure out where moving
;; to a new search result has landed you within your buffer.
;;
;; My fix is to notice when the minibuffer is active, and to instead
;; update `which-function-mode' in the window that was selected before
;; you entered the minibuffer.
;;
;; Maybe I should propose this upstream?

(defun my:which-func-update-avoid-minibuffer (orig-fun)
  (if (minibuffer-window-active-p (selected-window))
      (with-selected-window (minibuffer-selected-window)
        (funcall orig-fun))
    (funcall orig-fun)))

(advice-add 'which-func-update :around #'my:which-func-update-avoid-minibuffer)
