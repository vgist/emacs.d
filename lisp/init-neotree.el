;;; init-neotree.el --- neotree support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'neotree)
  (global-set-key (kbd "C-c n") 'neotree-toggle))

(with-eval-after-load 'neotree
  (setq neo-smart-open t
        neo-theme 'arrow
        neo-autorefresh t
        neo-show-hidden-files t
        neo-mode-line-type 'none))

(provide 'init-neotree)
;;; init-neotree.el ends here
