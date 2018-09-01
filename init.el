;;; Prologue

;; Prefer loading a newer .el to an older .elc.  Probably keeps me
;; from getting in trouble if I forget to byte compile.
(setq load-prefer-newer t)


;;; package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
