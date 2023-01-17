;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'spacemacs-theme)


;; Auto load gui appearance color by time
(setq current-theme (load-theme 'spacemacs-light t))
(defun myinc/toggle-theme ()
  "Toggle appearance color between light mode and dark mode."
  (setq hour (string-to-number (substring (current-time-string) 11 13)))
  (if (member hour (number-sequence 6 17))      ; 6:00 - 17:59
      (setq now '(load-theme 'spacemacs-light t))
    (setq now '(load-theme 'spacemacs-dark t)))
  (if (equal now current-theme) nil (setq current-theme now) (eval now)))
(if (display-graphic-p)
    (add-hook 'after-init-hook
              (lambda () (run-with-timer 0 3600 'myinc/toggle-theme)))
  (add-hook 'after-init-hook
            (lambda () (load-theme 'spacemacs-dark t))))


;; switch appearance colors by F5
;; (when (display-graphic-p)
;;   (defun myinc/toggle-theme ()
;;     "Toggle between light mode and dark mode."
;;     (if (eq (car custom-enabled-themes) 'spacemacs-dark)
;;         (disable-theme 'spacemacs-dark)
;;       (load-theme 'spacemacs-dark))))
;; (global-set-key [f5] 'my/inc/toggle-theme)


;; Unset background color in terminal
(defun on-after-init (&optional frame)
  "Unset Background colours in terminal FRAME."
  (or frame (setq frame (selected-frame)))
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)
    (set-face-background 'mode-line "unspecified-bg" frame)
    (set-face-background 'mode-line-inactive "unspecified-bg" frame)
    (set-face-background 'linum "unspecified-bg" frame)))
(add-hook 'after-make-frame-functions 'on-after-init)
(add-hook 'window-setup-hook 'on-after-init)


;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)


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
  (setq-default dimmer-fraction 0.15)
  (add-hook 'after-init-hook 'dimmer-mode)
  (with-eval-after-load 'dimmer
    ;; TODO: file upstream as a PR
    (advice-add 'frame-set-background-mode :after
                (lambda (&rest args) (dimmer-process-all))))
  (with-eval-after-load 'dimmer
    ;; Don't dim in terminal windows. Even with 256 colours it can
    ;; lead to poor contrast.  Better would be to vary dimmer-fraction
    ;; according to frame type.
    (defun sanityinc/display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates
                 'sanityinc/display-non-graphic-p)))

(provide 'init-themes)
;;; init-themes.el ends here
