;;; init-global.el --- Global settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; 字体，窗口大小以及位置
(when window-system
  (progn
    (when *linux*
      (set-frame-font "DejaVu Sans Mono 12")
      (set-fontset-font "fontset-default"
                        'unicode '("Souce Han Sans CN" . "unicode-bmp")))
    (when *is-a-mac*
      (set-frame-font "Monaco 14")
      (set-fontset-font t
                        'unicode (font-spec :name "PingFang SC")))
    ;; left = 20%, top 0
    (set-frame-position (selected-frame)
                        (/ (x-display-pixel-width) 5)
                        ;;(/ (x-display-pixel-height) 25)
                        0)
    ;; width 50%, height 90%
    (add-to-list 'default-frame-alist
                 (cons 'width (/ (* 5 (x-display-pixel-width))
                                 (* 10 (frame-char-width)))))
    (add-to-list 'default-frame-alist
                 (cons 'height (/ (* 9 (x-display-pixel-height))
                                  (* 10 (frame-char-height)))))
    ))


;; locales 配置
(defun sanityinc/locale-var-encoding (v)
  "Return the encoding portion of the locale string V, or nil if missing."
  (when v (save-match-data
            (let ((case-fold-search t))
              (when (string-match "\\.\\([^.]*\\)\\'" v)
                (intern (downcase (match-string 1 v))))))))

(dolist (varname '("LC_ALL" "LANG" "LC_CTYPE"))
  (let ((encoding (sanityinc/locale-var-encoding (getenv varname))))
    (unless (memq encoding '(nil utf8 utf-8))
      (message "Warning: non-UTF8 encoding in environment variable %s may
               cause interop problems with this Emacs configuration."
               varname))))

(when (fboundp 'set-charset-priority) (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt) (set-selection-coding-system 'utf-8))


;; UI 设置
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-message t         ;; 关闭启动画面
      gnus-inhibit-startup-message t
      visible-bell t                    ;; 关闭出错时的提示声
      default-tab-width 4               ;; 每次缩进4个空格
      mouse-yank-at-point t             ;; 支持中键粘贴
      x-select-enable-clipboard t       ;; emacs和外部程序的粘贴
      default-fill-column 80            ;; 默认显示80列就换行
      suggest-key-bindings t            ;; 若命令有组合键，则提示
      kill-ring-max 200                 ;; 设定删除保存记录
      ;;confirm-kill-emacs 'y-or-n-p      ;; C-x X-c 时需确认
      ;;user-full-name "nil"
      ;;user-mail-address "email"
      )


(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))
(unless (and *is-a-mac* window-system)
            (menu-bar-mode -1))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))


;; 窗体的透明度
(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
(global-set-key (kbd "M-0")
                (lambda ()
                  (interactive)
                  (modify-frame-parameters nil `((alpha . 100)))))
(global-set-key (kbd "M--")
                (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-=")
                (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))


;; 在标题栏提示路径
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))


;; 快捷键配置
(global-set-key (kbd "C-SPC") 'nil)                     ; 注销 ctrl_space
(global-set-key (kbd "C-x C-r") 'revert-buffer)         ; 重载当前文件
(global-set-key (kbd "C-k") 'kill-this-buffer)          ; 关闭当前 buffer
(global-set-key (kbd "C--") 'split-window-vertically)   ; 左右分割当前窗口
(global-set-key (kbd "C-=") 'split-window-horizontally) ; 上下分割当前窗口
(global-set-key (kbd "s-0") 'text-scale-mode)           ; 恢复文字默认大小
(global-set-key (kbd "s--") 'text-scale-decrease)       ; 缩小文字
(global-set-key (kbd "s-=") 'text-scale-increase)       ; 放大文字
(global-set-key [f10] 'other-window)                    ; 当前窗口间跳转
(global-set-key (kbd "C-s-l") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-h") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-j") 'shrink-window)
(global-set-key (kbd "C-s-k") 'enlarge-window)
;; bound tab to C-tab
(global-set-key [C-tab] '(lambda () (interactive) (insert-char 9 1)))


;; 格式化整个文件并绑定到C-F9键
(defun indent-whole ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "format successfully"))
(global-set-key [C-f9] 'indent-whole)

(defun my-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 4))))
(global-set-key (kbd "M-'") 'my-toggle-selective-display)


;; MacOS
(when *is-a-mac*
  (setq dired-use-ls-dired nil)         ; ls --dired issue
  (setq mac-command-modifier 'super     ; command as super
        mac-option-modifier 'meta       ; option as meta
        ;; mac-option-key-is-meta
        ;; mac-command-key-is-meta
        )
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key
       (read-kbd-macro
        (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  ;; Command-Control-f to toggle fullscreen mode
  (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
  )


;; 某些模式关闭行号
(global-linum-mode nil)
(setq linum-mode-except-modes
      '(ansi-term-mode
        eshell-mode
        erc-mode
        shell-mode
        term-mode
        vterm-mode))
(defadvice linum-on (around linum-on-except-for-modes)
  "Stop the load of linum-mode for some major modes."
  (unless (or (minibufferp) (member major-mode linum-mode-except-modes))
    ad-do-it))
(ad-activate 'linum-on)


;; 其他配置
(defun personal-clutter()
  "Personal clutter"
  (display-time-mode 1)                     ; 显示时间
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t)
  (fset 'yes-or-no-p 'y-or-n-p)             ; y/n means yes/no
  (global-font-lock-mode t)                 ; 语法高亮
  (auto-image-file-mode t)                  ; 图片显示
  (column-number-mode t)                    ; 显示列号
  (global-hl-line-mode 1)                   ; 高亮当前行
  (show-paren-mode t)                       ; 显示括号匹配
  (setq show-paren-style 'parentheses)
  (require 'electric)                       ; 内置的智能自动补全括号
  (electric-pair-mode t)
  (electric-indent-mode t)                  ; 编辑时智能缩进，类似于C-j的效果
  ;;(electric-layout-mode t)                ; 特定条件下插入新行
  ;;(mouse-avoidance-mode 'animate)         ; 鼠标指针自动避让
  ;;(pc-selection-mode)                     ; shift选择
  (windmove-default-keybindings 'shift)     ; shift + 箭头
  (setq scroll-margin 1                     ; 光标上下1行处开始滚动
        scroll-conservatively 10000)
  ;;(setq make-backup-files nil)            ; 不产生临时备份文件
  (defconst emacs-tmp-dir
            (expand-file-name
              (format "emacs%d" (user-uid)) temporary-file-directory))
  (setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
  (setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
  (setq auto-save-list-file-prefix emacs-tmp-dir)
  (save-place-mode 1)
  )
(add-hook 'after-init-hook 'personal-clutter)


(provide 'init-global)
;;; init-global.el ends here
