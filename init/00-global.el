(provide '00-global)

;; 不产生临时、备份文件
;;(setq-default make-backup-files nil)
;;(setq make-backup-files nil)

;; 备份文件、临时文件
;;(setq temporary-file-directory "~/.emacs.d/.tmp/")
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "~/.emacs.d/.tmp/" (user-uid)))
    (setq backup-directory-alist
        `((".*" . ,emacs-tmp-dir)))
    (setq auto-save-file-name-transforms
        `((".*" ,emacs-tmp-dir t)))
    (setq auto-save-list-file-prefix
        emacs-tmp-dir)

;; 可以保存你上次光标所在的位置
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/.tmp/emacs-places")

;; 关闭启动画面
(setq inhibit-startup-message t)
(setq gnus-inhibit-startup-message t)

;; 关闭出错时的提示声
(setq visible-bell t)

;; 打开就启用text模式。
(setq default-major-mode 'text-mode)

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

;; 显示括号匹配
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; 每次缩进2个空格
(setq default-tab-width 2)

;; shift选择
;(pc-selection-mode)

;; 光标靠近鼠标指针时，让鼠标指针自动让开
(mouse-avoidance-mode 'animate)

;; 支持中键粘贴
(setq mouse-yank-at-point t)

;; 允许临时设置标记
;(transient-mark-mode t)

;; 支持emacs和外部程序的粘贴
(setq x-select-enable-clipboard t)

;; 默认显示80列就换行
(setq default-fill-column 80)

;; 若命令有组合键，则提示该组合键
(setq suggest-key-bindings t)

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

;; 设定删除保存记录
(setq kill-ring-max 200)

;; 内置输入法
(setq default-input-method "chinese-py")

;; 直接打开图片
(auto-image-file-mode)

;; 插入时间
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string ";; %Y-%m-%d %H:%M %a")))

;; 个人信息
(setq user-full-name "nil")
(setq user-mail-address "admin@domain.com")
