;;; init-tabbar.el --- Tab bar configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(maybe-require-package 'tabbar)


(defun myinc/tabbar-opt ()
  "Tabbar Settings."
  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
                   (cons "" nil))))
  (setq tabbar-scroll-left-help-function nil
        tabbar-scroll-right-help-function nil
        tabbar-help-on-tab-function nil
        tabbar-home-help-function nil
        tabbar-separator (quote (1.5))
        erc-header-line-uses-tabbar-p t)
  (tabbar-mode 1))

(add-hook 'window-setup-hook 'myinc/tabbar-opt)


;; customize to show all normal files in one group
(defun myinc/tabbar-buffer-groups ()
  "Return' the name of the tab group names the current buffer belongs to."
  (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
              ((eq major-mode 'dired-mode) "emacs")
              (t "user"))))

(setq tabbar-buffer-groups-function 'myinc/tabbar-buffer-groups)


(provide 'init-tabbar)
;;; init-tabbar.el ends here
