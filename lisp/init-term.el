;;; init-term.el --- Integrate with terminals such as xterm -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'init-frame-hooks)


(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

(autoload 'mwheel-install "mwheel")

(defun sanityinc/console-frame-setup ()
  "Mouse in a terminal (Use shift to paste with middle button)."
  (xterm-mouse-mode 1)
  (mwheel-install)
  (setq system-uses-terminfo nil))

(add-hook 'after-make-console-frame-hooks 'sanityinc/console-frame-setup)


;; Don't show the banner: "Welcome to the Emacs shell"
(setq eshell-banner-message "")


(with-eval-after-load 'eshell
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))


;; Run into `ansi-term` automatically
(add-hook 'eshell-mode-hook
          (lambda ()
            ;;(add-to-list 'eshell-visual-commands "git")
            (add-to-list 'eshell-visual-commands "ssh")
            (add-to-list 'eshell-visual-commands "vim")))


;; Disable hl-line-mode for eshell, shell and term mode
(defun myinc/disable-hl-line-mode ()
  "Disable \"hl-line-mode\" for eshell shell and term mode."
  (setq-local global-hl-line-mode nil))
(add-hook 'eshell-mode-hook 'myinc/disable-hl-line-mode)
(add-hook 'ansi-term-mode-hook 'myinc/disable-hl-line-mode)
(add-hook 'term-mode-hook 'myinc/disable-hl-line-mode)
(add-hook 'vterm-mode-hook 'myinc/disable-hl-line-mode)


;; Non-zero values for 'line-sppacing'
(add-hook 'after-make-console-frame-hooks (lambda () (setq line-spacing 0)))


(defun myinc/term-on-exit (orig-fun proc msg)
  "Run ORIG-FUN, then close the window and buffer when process ends."
  (funcall orig-fun proc msg)
  (when (memq (process-status proc) '(signal exit))
    (let ((buffer (process-buffer proc)))
      (unless (one-window-p)
        (delete-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))
(advice-add 'term-sentinel :around #'myinc/term-on-exit)


(defun myinc/eshell-quit () "Close window on eshell exit."
       (when (not (one-window-p)) (delete-window)))

(advice-add 'eshell-life-is-too-much :after 'myinc/eshell-quit)


(defun myinc/bottom-window ()
  "Open term window in the base of current buffer."
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3)))
    (split-window-vertically (- height))
    (other-window 1)))

(global-set-key (kbd "C-x e") (lambda ()
                                (interactive)
                                (myinc/bottom-window) (eshell "new")))
(global-set-key (kbd "C-x t") (lambda ()
                                (interactive)
                                (myinc/bottom-window)
                                (ansi-term (or (getenv "SHELL") shell-file-name))))


(provide 'init-term)
;;; init-term.el ends here
