;;; early-init.el --- Emacs 27+ pre-initialisation config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil)

;; 启动阶段放大 GC 阈值以加速启动；启动完成后由 gcmh 接管（见 init.el）
(setq gc-cons-threshold (* 128 1024 1024))
(setq gc-cons-percentage 0.1)

(provide 'early-init)
;;; early-init.el ends here
