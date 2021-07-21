;;; init-erc.el --- erc auth -*- lexical-binding: t -*-
;;; Commentary:
;;; /nick yourname
;;; /msg nickserv register your-password your-email
;;; Then, you will recive an email, follow the email to verify register
;;; Code:


(require-package 'erc-hl-nicks)
(require-package 'erc-image)

(require 'erc-fill)
(require 'erc-join)
(require 'erc-match)
(require 'erc-netsplit)
(require 'erc-nicklist)
(require 'erc-ring)
(require 'erc-track)

(erc-track-mode t)

(add-hook 'erc-mode-hook
          '(lambda ()
             (require 'erc-pcomplete)
             (pcomplete-erc-setup)
             (erc-completion-mode 1)))

;; http://www.emacswiki.org/emacs/ErcAutoQuery
(setq erc-auto-query 'buffer)
(add-hook 'erc-after-connect
          (lambda (server nick)
            (add-hook 'erc-server-NOTICE-hook 'erc-auto-query)))

;; Ignoring notices
(setq erc-hide-list '("JOIN" "MODE" "NICK" "PART" "QUIT")
      erc-echo-notices-in-minibuffer-flag t)
(erc-fill-mode t)
(erc-ring-mode t)
(setq whitespace-global-modes '(not erc-mode))
;; Don't flood bufer with quit/joins when a netsplit happens
(erc-netsplit-mode t)
(erc-timestamp-mode t)
(setq erc-timestamp-format "[%H:%M]")
(erc-button-mode nil) ;slow

;; Coding utf-8
(setq erc-default-coding-system '(utf-8 . utf-8)
      erc-join-buffer 'bury)

;; logging:
(setq erc-log-insert-log-on-open nil
      erc-log-channels nil
      erc-enable-logging t
      erc-log-channels-directory "~/.irclogs/"
      erc-hide-timestamps nil
      erc-save-buffer-on-part t
      erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)

(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda ()
                         (when (and (eq major-mode 'erc-mode)
                                    (not (null buffer-file-name)))))))

(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
(add-hook 'erc-mode-hook
          '(lambda ()
             (setq erc-log-file-coding-system 'utf-8)
             (when (not (featurep 'xemacs))
               (set (make-variable-buffer-local
                     'coding-system-for-write)
                    'emacs-mule))))

;; Truncate buffers so they don't hog core.
(setq erc-max-buffer-size 20000)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(setq erc-truncate-buffer-on-save t)

;; Clears out annoying erc-track-mode stuff for when we don't care.
;; Useful for when ChanServ restarts :P
(defun reset-erc-track-mode ()
  "Clears out."
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))

;; Erc button url
(setq erc-button-url-regexp
      "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")

;; Show inlined images
(add-to-list 'erc-modules 'image)
(erc-update-modules)
(setq erc-image-inline-rescale 300)

;; Autojoin when we start-up
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("libera.chat"
         "#archlinux-cn" "#gentoo-zh" "#gentoo-arm" "#lfs-support")
        ;;("oftc.net" "#arch-cn")
        ))
(setq erc-autojoin-timing 'ident
      erc-autojoin-delay 3)

;; Highlight keywords
(setq erc-keywords '("gist"))
(erc-match-mode)

;; Finally, connect to the networks with M-x ierc
(defun ierc ()
  "Start connecting to IRC."
  (interactive)
  (let
      ((password-cache nil))
    (erc-tls
     :server "irc.libera.chat" :port 6697 :nick "gist" ;:full-name "nickname"
     :password (password-read (format "Your password for Libera Chat? ")))
    ;;(erc-tls
    ;; :server "irc.oftc.net" :port 6697 :nick "nickname" :full-name "fullname"
    ;; :password (password-read (format "Your password for oftc?")))
    ))


(provide 'init-erc)
;;; init-erc.el ends here
