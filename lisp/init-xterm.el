;;; init-xterm.el --- Integrate with terminals such as xterm -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'init-frame-hooks)


;; Don't show the banner: "Welcome to the Emacs shell"
(setq eshell-banner-message "")


(defun sanityinc/console-frame-setup ()
  "Mouse in a terminal ()Use shift to paste with middle button."
  (xterm-mouse-mode 1)
  (mwheel-install)
  (autoload 'mwheel-install "mwheel")
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))
  (setq system-uses-terminfo nil))

(add-hook 'after-make-console-frame-hooks 'sanityinc/console-frame-setup)


(defadvice
    term-sentinel (around do.term/close-on-exit-advice (proc msg))
    "Close window or buffer on exit."
    (if (and (memq (process-status proc) '(signal exit)) (not (one-window-p)))
      (let* ((buffer (process-buffer proc)))
        ad-do-it (delete-window) (kill-buffer buffer))
      (let* ((buffer (process-buffer proc)))
        ad-do-it (kill-buffer buffer)))
    ad-do-it)

(ad-activate 'term-sentinel)


(defun eshell/x () "Close window on eshell exit."
  (when (not (one-window-p)) (delete-window)))

(advice-add 'eshell-life-is-too-much :after 'eshell/x)


(defun term-bottom ()
  "Run term in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3)))
    (split-window-vertically (- height))
    (other-window 1)
    (ansi-term (getenv "SHELL"))))

(global-set-key (kbd "C-x t") 'term-bottom)


(defun eshell-bottom ()
  "Run eshell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                   (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3)))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")))

(global-set-key (kbd "C-x e") 'eshell-bottom)


;; Disable hl-line-mode for eshell, shell and term mode
(defun disable-hl-line-mode ()
  "Disable \"hl-line-mode\" for eshell shell and term mode."
  (setq-local global-hl-line-mode nil))
(add-hook 'shell-mode-hook 'disable-hl-line-mode)
(add-hook 'eshell-mode-hook 'disable-hl-line-mode)
(add-hook 'term-mode-hook 'disable-hl-line-mode)


;; Non-zero values for 'line-sppacing'
(add-hook 'shell-mode-hook (lambda () (setq line-spacing 0)))
(add-hook 'eshell-mode-hook (lambda () (setq line-spacing 0)))
(add-hook 'term-mode-hook (lambda () (setq line-spacing 0)))


(provide 'init-xterm)
;;; init-xterm.el ends here
