;;; init-pyim.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'pyim)

(setq default-input-method "pyim"                   ; 默认输入法 pyim
      pyim-page-tooltip 'popup                      ; 设置选词框的绘制方式
      pyim-page-length 5                            ; 显示5个候选词。
      pyim-page-style 'one-line                     ; 单行显示
      pyim-punctuation-translate-p '(yes no auto)   ; 使用全角标点
      pyim-indicator-list                           ; 指示器
      (list #'pyim-indicator-with-cursor-color
            #'pyim-indicator-with-modeline))

;; 激活快捷键，Control-\
(global-set-key (kbd "C-\\") 'toggle-input-method)

;; 默认全拼音
(pyim-default-scheme 'quanpin)

;; 基本词库
;;(require-package 'pyim-basedict)
;;(pyim-basedict-enable)

;; 清华中文词库
(require 'pyim-tsinghua-dict)
(pyim-tsinghua-dict-enable)

(provide 'init-pyim)
;;; init-themes.el ends here
