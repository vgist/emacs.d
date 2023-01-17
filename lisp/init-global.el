;;; init-global.el --- Global settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; 默认设置
(setq-default
 display-time-mode t                    ; 显示时间
 display-time-24hr-format t
 display-time-day-and-date t
 ;; case-fold-search t
 use-file-dialog nil
 use-dialog-box nil
 inhibit-startup-screen t               ; 关闭启动画面
 inhibit-startup-message t
 initial-buffer-choice t
 gnus-inhibit-startup-message t
 visible-bell t                         ; 关闭可见错误提示
 column-number-mode t                   ; 显示列号
 ;; confirm-kill-emacs 'y-or-n-p        ; C-x C-c 需确认
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil                   ; 替换tab 为空格
 select-enable-clipboard t              ; 使用剪贴板来剪贴和粘贴
 create-lockfiles nil
 mouse-yank-at-point t                  ; linux 下支持中键粘贴
 suggest-key-bindings t                 ; 提示快捷键
 kill-ring-max 200                      ; 设定删除保存记录
 truncate-lines nil                     ; 不要自动断行
 truncate-partial-width-windows nil)


;; 临时文件相关配置
(add-hook 'after-init-hook
          (lambda ()
            (save-place-mode 1)
            (defconst emacs-tmp-dir
              (expand-file-name
               (format "emacs%d" (user-uid)) temporary-file-directory))
            (setq backup-directory-alist `((".* . ,emacs-tmp-dir"))
                  make-backup-files nil ; 不产生临时文件
                  auto-save-default nil ; 关闭自动保存
                  auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
                  auto-save-list-file-prefix emacs-tmp-dir)))


;; 其他配置
(add-hook 'after-init-hook
          (lambda ()
            (fset 'yes-or-no-p 'y-or-n-p)
            (global-font-lock-mode 1)   ; 语法高亮
            (auto-image-file-mode 1)    ; 自动显示图片
            (global-hl-line-mode 1)     ; 高亮光标所在行
            (show-paren-mode 1)         ; 高亮括号匹配
            (setq show-paren-style 'parenthesis)
            ;; (mouse-avoidance-mode 'animate)
            (global-auto-revert-mode 1) ; 自动加载其他编辑器修改过的文件
            (setq global-auto-revert-non-file-buffers t
                  auto-revert-verbose nil)
            (with-eval-after-load 'autorevert
              (diminish 'auto-revert-mode))
            (transient-mark-mode 1)
            (windmove-default-keybindings 'shift)
            (delete-selection-mode 1)))


;; 启动后打开 TODO
;; (setq initial-buffer-choice t)
;; (defconst my-remember-file
;;   (expand-file-name "Remember.org" "Documents/Emacs"))
;; (when (file-exists-p my-remember-file)
;;   (setq initial-buffer-choice my-remember-file))


;; 字体，窗口大小及打开位置
(when window-system
  (progn
    ;; 字体
    (when *linux*
      (set-frame-font "DejaVu Sans Mono 12" "Source Han Sans CN 12")
      (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend))
    (when *is-a-mac*
      (set-frame-font "Monaco 14" "PingFang SC 14")
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))
    (when *windows*
      (set-frame-font "Consolas 12" "MicroSoft YaHei 12")
      (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'prepend)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset (font-spec :family "MicroSoft YaHei"))))
    ;; 位置 left 20%, top 0
    (set-frame-position (selected-frame)
                        (/ (x-display-pixel-width) 5)
                        ;; (/ (x-display-pixel-height) 25)
                        0)
    ;; 窗口大小 宽 60%, 高 85%
    (add-to-list 'default-frame-alist
                 (cons 'width (/ (* 6 (x-display-pixel-width))
                                 (* 10 (frame-char-width)))))
    (add-to-list 'default-frame-alist
                 (cons 'height (/ (* 85 (x-display-pixel-height))
                                  (* 100 (frame-char-height)))))))


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


;; 像素级滚动
(if (version< emacs-version "29")
    (pixel-scroll-mode 1)
  ((pixel-scroll-precision-mode 1)
   (setq pixel-scroll-precision-large-scroll-height 40.0
         pixel-scroll-precision-interpolation-factor 30)))
(setq scroll-margin 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 'always)


(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode)
  (prefer-coding-system 'cp936)
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8
        system-time-locale "C")
  (unless *windows* (set-selection-coding-system 'utf-8)))


;; 某些模式关闭行号
(when (fboundp 'linum-mode)
  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'text-mode-hook 'linum-mode))


;; 显示列宽指示器
(when (boundp 'display-fill-column-indicator)
  (setq-default fill-column 78)
  ;; (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?\u254e)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))


(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))
(when (or *linux* *windows* (not (display-graphic-p)))
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;; 内置的括号智能补全与智能锁进
;; (require 'electric)
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(when (fboundp 'electric-indent-mode)
  (add-hook 'after-init-hook 'electric-indent-mode))


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
(global-set-key (kbd "C-SPC") 'nil)
(global-set-key (kbd "C-x C-r") 'revert-buffer)                 ; 重载当前文件
(global-set-key (kbd "C-x -") 'split-window-vertically)         ; 左右分割窗口
(global-set-key (kbd "C-x =") 'split-window-horizontally)       ; 上下分割窗口
(global-set-key (kbd "C-c 0") 'text-scale-mode)                 ; 恢复文字大小
(global-set-key (kbd "C-c -") 'text-scale-decrease)             ; 缩小文字
(global-set-key (kbd "C-c =") 'text-scale-increase)             ; 放大文字
(global-set-key (kbd "C-s-l") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-h") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-j") 'shrink-window)
(global-set-key (kbd "C-s-k") 'enlarge-window)
(global-set-key (kbd "C-x n") 'next-buffer)                     ; 前一缓冲区
(global-set-key (kbd "C-x p") 'previous-buffer)                 ; 后一缓冲区
;; bound tab to C-tab
(global-set-key [C-tab] (lambda () (interactive) (insert-char 9 1)))


;; 格式化整个文件并绑定到 C-F9 键
(defun indent-whole ()
  "Indenting the whole file."
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
  (setq dired-use-ls-dired nil          ; ls --dired
        mac-command-modifier 'super     ; command as super
        mac-option-modifier 'meta       ; option as meta
        ;; mac-option-key-is-meta
        ;; mac-command-key-is-meta
        )
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key
       (read-kbd-macro
        (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  ;; Control-Command-f to toggle fullscreen mode
  (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen))


(provide 'init-global)
;;; init-global.el ends here
