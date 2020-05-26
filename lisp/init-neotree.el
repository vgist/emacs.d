;;; init-neotree.el --- neotree support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'neotree)
  (global-set-key (kbd "C-x n") 'neotree-toggle)
  (setq neo-smart-open t)
  (setq-default neo-autorefresh t))

(provide 'init-neotree)
;;; init-neotree.el ends here
