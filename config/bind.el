;;;;;;;;;;;;;;;;;;;;;;;;;;;  快捷键配置开始  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-SPC") 'nil);注销ctrl_space组合健，已启用输入法

(global-set-key [f2] 'shell);F2进入Shell
(global-set-key [f7] 'calendar);打开日历
(global-set-key [f8] 'other-window);F8窗口间跳转
(global-set-key [C-return] 'kill-this-buffer);C-return关闭当前buffer
(global-set-key [f10] 'split-window-vertically);F10分割buffer
(global-set-key [f11] 'delete-other-windows);F11 关闭其他buffer
(global-set-key [f12] 'my-fullscreen);F12 全屏

;; 鼠标滚轮缩放字体大小
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

;;;;;;;;;;;;;;;;;;;;;;;;;;;  快捷键配置结束  ;;;;;;;;;;;;;;;;;;;;;;;;;;;