;;;;;;;;;;;;;;;;;;;;;;;;;;;  设置窗口界面开始 ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-foreground-color "grey")
(set-background-color "black")
(set-cursor-color "gold1")
(set-mouse-color "gold1")

(set-scroll-bar-mode nil);取消滚动栏

;;(customize-set-variable 'scroll-bar-mode 'right));设置滚动栏在窗口右侧，而默认是在左侧

(tool-bar-mode nil);取消工具栏
(menu-bar-mode nil);关闭菜单

;; 启动时窗口最大化
;(when window-system
;  (my-maximized))

;; 启动窗口大小
(when window-system
  (setq default-frame-alist
        '((left . 0) (top . 0)
                     ;(height . 20) (width . 20)
                     (menu-bar-lines . 20) (tool-bar-lines . 0))))

;; 设置另外一些颜色：语法高亮显示的背景和主题，区域选择的背景和主题，二次选择的背景和选择
(set-face-foreground 'highlight "white")
(set-face-background 'highlight "blue")
(set-face-foreground 'region "cyan")
(set-face-background 'region "blue")
(set-face-foreground 'secondary-selection "skyblue")
(set-face-background 'secondary-selection "darkblue")

(setq frame-title-format "^_^ @%b");在标题栏提示你目前在什么位置

;; 显示时间，格式如下
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

;;全屏
(defun my-fullscreen ()
  (interactive)
  (x-send-client-message
    nil 0 nil "_NET_WM_STATE" 32
    '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

;最大化
(defun my-maximized ()
  (interactive)
  (x-send-client-message
    nil 0 nil "_NET_WM_STATE" 32
    '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message
    nil 0 nil "_NET_WM_STATE" 32
    '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;  设置界面结束  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;  设置窗口界面开始 ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-foreground-color "grey")
(set-background-color "black")
(set-cursor-color "gold1")
(set-mouse-color "gold1")

(set-scroll-bar-mode nil);取消滚动栏

;;(customize-set-variable 'scroll-bar-mode 'right));设置滚动栏在窗口右侧，而默认是在左侧

(tool-bar-mode nil);取消工具栏
(menu-bar-mode nil);关闭菜单

;; 启动时窗口最大化
;(when window-system
;  (my-maximized))

;; 启动窗口大小
(when window-system
  (setq default-frame-alist
        '((left . 5) (top . 5) (height . 42) (width . 120) (menu-bar-lines . 20) (tool-bar-lines . 0))))

;; 设置另外一些颜色：语法高亮显示的背景和主题，区域选择的背景和主题，二次选择的背景和选择
(set-face-foreground 'highlight "white")
(set-face-background 'highlight "blue")
(set-face-foreground 'region "cyan")
(set-face-background 'region "blue")
(set-face-foreground 'secondary-selection "skyblue")
(set-face-background 'secondary-selection "darkblue")

(setq frame-title-format "^_^ @%b");在标题栏提示你目前在什么位置

;; 显示时间，格式如下
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

;;全屏
(defun my-fullscreen ()
  (interactive)
  (x-send-client-message
    nil 0 nil "_NET_WM_STATE" 32
    '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

;最大化
(defun my-maximized ()
  (interactive)
  (x-send-client-message
    nil 0 nil "_NET_WM_STATE" 32
    '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message
    nil 0 nil "_NET_WM_STATE" 32
    '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;  设置界面结束  ;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;  other  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default make-backup-files nil);不产生临时文件
(setq inhibit-startup-message t);关闭起动时闪屏
(setq visible-bell nil);关闭出错时的提示声
(setq make-backup-files nil);不产生备份文件
(setq default-major-mode 'text-mode);打开就启用text模式。
(global-font-lock-mode t);语法高亮
;(auto-image-file-mode t);打开图片显示功能
(fset 'yes-or-no-p 'y-or-n-p);y/n means yes/no
(global-linum-mode t);显示行号
(column-number-mode t);显示列号
(show-paren-mode t);显示括号匹配
(setq default-tab-width 4);每次缩进4个空格

;(pc-selection-mode);shift选择
;(mouse-avoidance-mode 'animate);光标靠近鼠标指针时，让鼠标指针自动让开
(setq mouse-yank-at-point t);支持中键粘贴
;(transient-mark-mode t);允许临时设置标记
(setq x-select-enable-clipboard t);支持emacs和外部程序的粘贴
;(setq default-fill-column 120);默认显示120列就换行
(setq suggest-key-bindings t);若命令有组合键，则提示该组合键
;(set-selection-coding-system 'iso-2022-8bit-ss2-dos);网页复制乱码问题
;;;;;;;;;;;;;;;;;;;;;;;;;;;  other end  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
