(provide '01-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;; 各类mode使用开始 ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mode path
(add-to-list 'load-path "~/.emacs.d/mode-lisp/markdown-mode")
(add-to-list 'load-path "~/.emacs.d/mode-lisp/ruby-mode")
(add-to-list 'load-path "~/.emacs.d/mode-lisp/scss-mode")
(add-to-list 'load-path "~/.emacs.d/mode-lisp/vimrc-mode")
(add-to-list 'load-path "~/.emacs.d/mode-lisp/yaml-mode")

;; vimrc mode
(autoload 'vimrc-mode "vimrc-mode"
	"Major mode for vimrc files" t)
(add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))

;; sass mode
(autoload 'scss-mode "scss-mode"
	"Major mode for editing SCSS files" t)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; ruby mode
(autoload 'ruby-mode "ruby-mode"
	"Major mode for editing Ruby code" t)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("/\\.\\(irb\\|cap\\)rc$" . ruby-mode))
(add-hook 'ruby-mode-hook (lambda ()
														(local-set-key "\r" 'newline-and-indent)))
(require 'inf-ruby)
(require 'ruby-electric)
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))

;; markdown mode
(autoload 'markdown-mode "markdown-mode"
	"Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; yaml mode
(autoload 'yaml-mode "yaml-mode"
	"Major mode to edit YAML file" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;; 各类mode使用结束 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
