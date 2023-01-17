;;; init-minimap.el --- Minimap configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'minimap)
  (setq minimap-window-location 'right
        minimap-hide-fringes t))

(provide 'init-minimap)
;;; init-minimap.el ends here
