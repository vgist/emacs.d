;;; init-pyim.el --- Settings for pyim -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'pyim)

(setq default-input-method "pyim")                      ; 默认输入法 pyim
(global-set-key (kbd "C-c \\") 'toggle-input-method)

(with-eval-after-load 'pyim
  (setq pyim-page-tooltip 'popup                        ; 设置选词框的绘制方式
        pyim-page-length 5                              ; 显示5个候选词。
        pyim-page-style 'one-line                       ; 横排显示
        pyim-punctuation-translate-p '(yes no auto)     ; 使用全角标点
        pyim-indicator-list                             ; 指示器
        (list #'pyim-indicator-with-cursor-color
              #'pyim-indicator-with-modeline))
  (setq pyim-dcache-directory (locate-user-emacs-file  ".cache/pyim/dcache/"))
  (pyim-default-scheme 'quanpin)                        ; 默认全拼
  (require-package 'pyim-basedict)                      ; 基础词库
  (pyim-basedict-enable)
  ;; 清华中文词库，注视掉上面两行，取消下面的注视
  ;; https://github.com/redguardtoo/pyim-tsinghua-dict
  ;; 下载 `pyim-tsinghua-dict.el` 和 `pyim-tsinghua-dict.pyim` 至
  ;; 目录 site-lisp 下
  ;; (require 'pyim-tsinghua-dict)
  ;; (pyim-tsinghua-dict-enable)
  )

(provide 'init-pyim)
;;; init-pyim.el ends here
