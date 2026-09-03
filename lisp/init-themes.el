;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'spacemacs-theme)


;; Auto load gui appearance color by time
(defvar myinc/current-theme 'spacemacs-light)
(defun myinc/toggle-theme ()
  "Toggle appearance color between light mode and dark mode."
  (let* ((now-time (current-time-string))
         (hour (string-to-number (substring now-time 11 13)))
         (target (if (member hour (number-sequence 6 17))  ; 6:00 - 17:59
                     'spacemacs-light
                   'spacemacs-dark)))
    (unless (eq target myinc/current-theme)
      (load-theme target t)
      (setq myinc/current-theme target))))
(if (display-graphic-p)
    (add-hook 'after-init-hook
              (lambda () (run-with-timer 0 3600 'myinc/toggle-theme)))
  (add-hook 'after-init-hook
            (lambda () (load-theme 'spacemacs-dark t))))


;; Unset background color in terminal
(defun myinc/unset-terminal-background (&optional frame)
  "Unset Background colours in terminal FRAME."
  (or frame (setq frame (selected-frame)))
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)
    (set-face-background 'mode-line "unspecified-bg" frame)
    (set-face-background 'mode-line-inactive "unspecified-bg" frame)))
(add-hook 'after-make-frame-functions 'myinc/unset-terminal-background)
(add-hook 'window-setup-hook 'myinc/unset-terminal-background)


;; 主题安全白名单：不再信任任意主题（原为 t）。
;; 只信任当前安装的 spacemacs 主题（SHA256 按文件内容计算）。
;; 注意：secure-hash 需先读文件内容再哈希，而非直接传入路径字符串。
(setq custom-safe-themes
      '("9af2b1c0728d278281d87dc91ead7f5d9f2287b1ed66ec8941e97ab7a6ab73c0"    ; spacemacs-light-theme.el
        "01f347a923dd21661412d4c5a7c7655bf17fb311b57ddbdbd6fce87bd7e58de6"    ; spacemacs-dark-theme.el
        "f4097216151fc72e4e8473b4fb134839c005a221cda2b06f50dc4637d1102054"))  ; spacemacs-theme.el


;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables
   `(custom-enabled-themes (quote ,custom-enabled-themes))))
(add-hook 'after-init-hook 'reapply-themes)

(when (maybe-require-package 'dimmer)
  (setq dimmer-fraction 0.15)
  (add-hook 'after-init-hook 'dimmer-mode)
  (with-eval-after-load 'dimmer
    ;; TODO: file upstream as a PR
    (advice-add 'frame-set-background-mode :after
                (lambda (&rest args) (dimmer-process-all))))
  (with-eval-after-load 'dimmer
    ;; Don't dim in terminal windows. Even with 256 colours it can
    ;; lead to poor contrast.  Better would be to vary dimmer-fraction
    ;; according to frame type.
    (defun myinc/display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates
                 'myinc/display-non-graphic-p)))

(provide 'init-themes)
;;; init-themes.el ends here
