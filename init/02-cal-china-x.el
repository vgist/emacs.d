;; cal-china-x

(provide '02-cal-china-x)

(add-to-list 'load-path "~/.emacs.d/mode-lisp/cal-china-x")

(when (require 'cal-china-x)
	(setq calendar-mark-holidays-flag t)
	(setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
	(setq calendar-holidays cal-china-x-important-holidays))