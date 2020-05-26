;;; init-vimrc.el --- vimrc support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'vimrc-mode)
  (add-auto-mode 'vimrc-mode ".vim\\(rc\\)?$"))

(provide 'init-vimrc)
;;; init-vimrc.el ends here
