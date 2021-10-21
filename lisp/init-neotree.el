;;; init-neotree.el --- neotree support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'neotree)
  (global-set-key (kbd "C-x n") 'neotree-toggle))

(with-eval-after-load 'neotree
  (setq neo-smart-open t
        neo-theme 'arrow)
  (setq-default neo-autorefresh t
                neo-show-hidden-files t))

(provide 'init-neotree)
;;; init-neotree.el ends here
