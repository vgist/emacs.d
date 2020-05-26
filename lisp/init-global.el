;;; init-global.el --- Global settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 不产生临时、备份文件
;;(setq-default make-backup-files nil)
;;(setq make-backup-files nil)

;; 备份文件、临时文件 ~/.emacs.d/.tmp/emacs$UID
(defconst emacs-tmp-dir (expand-file-name (format ".tmp/emacs%d" (user-uid)) user-emacs-directory))
    (setq backup-directory-alist
        `((".*" . ,emacs-tmp-dir)))
    (setq auto-save-file-name-transforms
        `((".*" ,emacs-tmp-dir t)))
    (setq auto-save-list-file-prefix
        emacs-tmp-dir)

;; locales 配置
(defun sanityinc/locale-var-encoding (v)
  "Return the encoding portion of the locale string V, or nil if missing."
  (when v
    (save-match-data
      (let ((case-fold-search t))
        (when (string-match "\\.\\([^.]*\\)\\'" v)
          (intern (downcase (match-string 1 v))))))))

(dolist (varname '("LC_ALL" "LANG" "LC_CTYPE"))
  (let ((encoding (sanityinc/locale-var-encoding (getenv varname))))
    (unless (memq encoding '(nil utf8 utf-8))
      (message "Warning: non-UTF8 encoding in environment variable %s may cause interop problems with this Emacs configuration." varname))))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))


;; 字体配置
(when (eq system-type 'Linux)
  (set-frame-font "DejaVu Sans Mono-12")
  (set-fontset-font "fontset-default"
                    'unicode '("Souce Han Sans CN" . "unicode-bmp")))

(when (eq system-type 'Darwin)
  (set-frame-font "Monaco-12")
  (set-fontset-font t 'unicode (font-spec :name "PingFang SC")))


;; UI 设置
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(if *is-a-mac*
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (unless (display-graphic-p frame)
                  (set-frame-parameter frame 'menu-bar-lines 0))))
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))

;(let ((no-border '(internal-border-width . 0)))
;  (add-to-list 'default-frame-alist no-border)
;  (add-to-list 'initial-frame-alist no-border))

;; 启动窗口大小
(when window-system
  (setq default-frame-alist
        '((height . 60)
          (width . 120)
          ;(left . 400)
          ;(top . 150)
          )))

;; 窗体的透明度
;(set-frame-parameter (selected-frame) 'alpha (list 85 85))

;; 在标题栏提示路径
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                 (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; 显示时间，格式如下
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)


;; 快捷键配置
(global-set-key (kbd "C-SPC") 'nil)         ; 注销 ctrl_space 组合健
(global-set-key (kbd "C-x t") 'term)        ; 进入 shell
(global-set-key
  (kbd "C-x C-r") 'revert-buffer)           ; 重载当前文件
(global-set-key
  (kbd "C-\\") 'split-window-vertically)     ; 纵向分割 buffer
(global-set-key
  (kbd "C-|") 'split-window-horizontally)   ; 横向分割 buffer
(global-set-key
  (kbd "C-S-k") 'delete-other-windows)      ; 关闭其他 buffer
(global-set-key
  (kbd "C-k") 'kill-this-buffer)            ; 关闭当前 buffer
(global-set-key [f9] 'calendar)             ; 打开日历
(global-set-key [f10] 'other-window)        ; 窗口间跳转

;; 鼠标滚轮缩放字体大小
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

;; 格式化整个文件并绑定到C-F9键
(global-set-key [C-f9] 'indent-whole)
(defun indent-whole ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "format successfully"))

(global-set-key (kbd "M-'") 'my-toggle-selective-display)
(defun my-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 4))))

;; MacOS
(when *is-a-mac*
  ;(setq mac-command-modifier 'meta)        ; option as meta
  (setq mac-option-modifier 'meta)          ; command as meta
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  (with-eval-after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
  (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen)
  )


;; 可以保存你上次光标所在的位置
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/.tmp/emacs-places")


(setq inhibit-startup-message t         ;; 关闭启动画面
      gnus-inhibit-startup-message t
      visible-bell t                    ;; 关闭出错时的提示声
      default-tab-width 4               ;; 每次缩进4个空格
      mouse-yank-at-point t             ;; 支持中键粘贴
      x-select-enable-clipboard t       ;; emacs和外部程序的粘贴
      default-fill-column 80            ;; 默认显示80列就换行
      suggest-key-bindings t            ;; 若命令有组合键，则提示
      kill-ring-max 200                 ;; 设定删除保存记录
      ;confirm-kill-emacs 'y-or-n-p      ;; C-x X-c 时需确认
      ;user-full-name "nil"
      ;user-mail-address "email"
)


;; 语法高亮
(global-font-lock-mode t)

;; 打开图片显示功能
(auto-image-file-mode t)

;; y/n means yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; 显示行号
(global-linum-mode t)

;; 显示列号
(column-number-mode t)

;; 高亮当前行
(global-hl-line-mode 1)

;; 显示括号匹配
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; shift选择
;(pc-selection-mode)

;; 光标靠近鼠标指针时，让鼠标指针自动让开
(mouse-avoidance-mode 'animate)

;; 允许临时设置标记
;(transient-mark-mode t)

;; 网页复制乱码问题
;(set-selection-coding-system 'iso-2022-8bit-ss2-dos)

;; 光标上下1行处开始滚动
(setq scroll-margin 1 scroll-conservatively 10000)

;; 系统本身内置的智能自动补全括号
(electric-pair-mode t)
(require 'electric)
;; 编辑时智能缩进，类似于C-j的效果
(electric-indent-mode t)
;; 系统本身内置的智能自动补全括号
(electric-pair-mode t)
;; 特定条件下插入新行
;;(electric-layout-mode t)

;; 直接打开图片
(auto-image-file-mode)

;; 插入时间
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string ";; %Y-%m-%d %H:%M %a")))

;; ls --dired issue
(when *is-a-mac*
  (setq dired-use-ls-dired nil))

(provide 'init-global)
;;; init-global.el ends here
