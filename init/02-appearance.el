(provide '02-appearance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;  设置窗口界面开始 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 使用 theme-monokai 配色
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/monokai")
(load-theme 'monokai t)
(set-cursor-color "Gold")

;; 设置滚动栏在窗口右侧，而默认是在左侧
;;(customize-set-variable 'scroll-bar-mode 'right))

(tool-bar-mode -1);取消工具栏
(menu-bar-mode -1);关闭菜单
(scroll-bar-mode 0);隐藏滚动条

;; 启动时窗口最大化
;;(when window-system
;;  (my-maximized))
;;(defun my-maximized ()
;;	(interactive)
;;  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;												 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;;  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;												 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

;; 启动窗口大小
(when window-system
  (setq default-frame-alist
        '((height . 50) (width . 120) (left . 400) (top . 150)
					(menu-bar-lines . 0) (tool-bar-lines . 0))))

;; 窗体的透明度
;;(set-frame-parameter (selected-frame) 'alpha (list 85 85))

;; 在标题栏提示路径
																				;(setq frame-title-format "^_^ @%f")
(setq frame-title-format "^_^ @%b")

;; 显示时间，格式如下
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;  设置界面结束  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
