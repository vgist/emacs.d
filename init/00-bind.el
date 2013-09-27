(provide '00-bind)

;;;;;;;;;;;;;;;;;;;;;;;;;;;  快捷键配置开始  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-SPC") 'nil);注销ctrl_space组合健，已启用输入法
(global-set-key [f5] 'calendar);打开日历
(global-set-key [f7] 'term);进入Shel
(global-set-key [f8] 'revert-buffer);重载当前文件
(global-set-key [f9] 'split-window-vertically);分割buffer
(global-set-key [f10] 'other-window);窗口间跳转
(global-set-key [f11] 'delete-other-windows);关闭其他buffer
(global-set-key [C-return] 'kill-this-buffer);C-return关闭当前buffer

;; 全屏
(global-set-key [f12] 'my-fullscreen)
(defun my-fullscreen ()
  (interactive)
  (x-send-client-message
	 nil 0 nil "_NET_WM_STATE" 32
	 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

;; 鼠标滚轮缩放字体大小
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)


;; 格式化整个文件并绑定到C-F9键
(global-set-key [C-f9] 'indent-whole)
(defun indent-whole ()
	(interactive)
	(indent-region (point-min) (point-max))
	(message "format successfully"))

;; 折叠
;;(define-key global-map (kbd "M-'") 'my-toggle-selective-display)
;;(defun my-toggle-selective-display()
;;	"set-selective-display to current column or toggle selective-display --lgfang"
;;	(interactive)
;;	(let ((arg (progn (back-to-indentation) (current-column))))
;;		(set-selective-display (if (eq arg selective-display) nil arg))))

(global-set-key (kbd "M-'") 'my-toggle-selective-display)
(defun my-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display 
   (if selective-display nil (or column 4))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;  快捷键配置结束  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
