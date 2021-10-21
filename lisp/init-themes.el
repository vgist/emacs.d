;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'github-theme)
(require-package 'monokai-theme)

;; Auto load gui appearance color by time
(setq current-theme (load-theme 'github t))
(defun toggle-theme ()
  "Toggle appearance color between light mode and dark mode."
  (setq hour (string-to-number (substring (current-time-string) 11 13)))
  (if (member hour (number-sequence 6 17))    ;; 6:00 - 17:59
    (setq now '(load-theme 'github t))
    (setq now '(load-theme 'monokai t)))
  (if (equal now current-theme)
    nil
    (setq monokai-background  "#2B3E50"
          monokai-comments    "#8F908A"
          monokai-highlight   "#405160"
          current-theme now)
    (eval now)))
(if (display-graphic-p)
  (add-hook 'after-init-hook
            (lambda () (run-with-timer 0 3600 'toggle-theme)))
  (add-hook 'after-init-hook
            (lambda () (load-theme 'monokai t))))

;; Adjust minibuffer color
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (make-local-variable 'face-remapping-alist)
            (add-to-list
              'face-remapping-alist
              '(ivy-current-match
                 (:background "#8F908A" :inherit bold :underline nil)
                ivy-minibuffer-match-face-2
                 (:foreground "#333333" :background "#8F908A" :inherit bold)))))

;; switch appearance colors by F5
;;(when (display-graphic-p)
;;  (defun toggle-theme ()
;;    "Toggle between light mode and dark mode."
;;    (interactive)
;;    (if (eq (car custom-enabled-themes) 'github)
;;      (disable-theme 'github)
;;      (load-theme 'github)
;;  (global-set-key [f5] 'toggle-theme))


;; Unset background color in terminal
(defun on-after-init (&optional frame)
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
