;; -*- lexical-binding: t; -*-

;; Key prefixes (as strings) listed in
;; `my:which-key-no-delay-prefixes' will have a very short delay, as
;; specified by `which-key-idle-delay'.  Any other prefixes will have
;; a delay as specified by `my:which-key-normal-idle-delay'.

(require 'which-key)

(defvar my:which-key-no-delay-prefixes nil
  "Prefixes in this list will have which-key help displayed quickly.
If T is in this list then both the global and any buffer-local
value for this variable will be checked.")

(defvar my:which-key-normal-idle-delay 1.0
  "The desired idle delay for prefixes not listed in
`my:which-key-no-delay-prefixes'.")

(defun my:which-key-delay-function (prefix _len)
  (unless (or (member prefix my:which-key-no-delay-prefixes)
              (and (local-variable-p 'my:which-key-no-delay-prefixes)
                   (member t my:which-key-no-delay-prefixes)
                   (member prefix
                           (default-value 'my:which-key-no-delay-prefixes))))
    (max (- my:which-key-normal-idle-delay which-key-idle-delay) 0.0)))

(add-to-list 'which-key-delay-functions #'my:which-key-delay-function)

;; This should be very short but non-zero, because 0.0 breaks stuff.
;; See also `my:which-key-delay-function'.
(setq which-key-idle-delay 0.1)
