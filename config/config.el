;;;;;;;;;;;;;;;;;;;;;;;;;;; 各类mode使用开始 ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mode path
(add-to-list 'load-path "~/.emacs.d/elisp")

;; php mode
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.module$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-hook 'php-mode-user-hook 'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;; 各类mode使用结束 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
