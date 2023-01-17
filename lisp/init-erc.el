;;; init-erc.el --- erc auth -*- lexical-binding: t -*-
;;; Commentary:
;;; /nick yourname
;;; /msg nickserv register your-password your-email
;;; Then, you will recive an email, follow the email to verify register
;;; Code:


;; Connect to the networks with M-x ierc
(defun myinc/ierc ()
  "Start connecting to IRC."
  (interactive)
  ;; Load authentication info from an external source.
  ;; Put sensitive nick passwords and the like in file.
  ;; With content:
  ;; (setq erc-nick "your-nick")
  ;; (setq erc-password "your-password")
  ;; (setq erc-autojoin-channels-alist '(("libera.chat" "#gentoo")))
  (when (file-exists-p (locate-user-emacs-file ".erc-auth.el"))
    (load (locate-user-emacs-file ".erc-auth.el"))
    (let
        ((password-cache nil))
      (erc-tls
       :server "irc.libera.chat" :port 6697
       :nick erc-nick
       :password erc-password
       ;; :password (password-read (format "Password for Libera Chat? "))
       ))
    ))

(global-set-key (kbd "C-c e") 'myinc/ierc)


;; Common settings
(defun myinc/messy-settings-for-erc()
  "Messy settings for erc."

  ;; Basic
  (setq erc-default-coding-system '(utf-8 . utf-8)
        erc-join-buffer 'bury           ; bury, bufer, frame, window...
        erc-hide-list '("JOIN" "MODE" "NICK" "PART" "QUIT")
        erc-echo-notice-hook '(erc-echo-notice-in-minibuffer)
        erc-kill-buffer-on-part t
        erc-kill-queries-on-quit t
        erc-kill-server-buffer-on-quit t
        whitespace-global-modes '(not erc-mode)
        erc-timestamp-format "%H:%M "
        erc-fill-prefix "        "
        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-log-insert-log-on-open nil
        erc-log-channels nil
        erc-enable-logging t
        erc-log-channels-directory "~/.irclogs/"
        erc-hide-timestamps nil
        erc-save-buffer-on-part t
        erc-save-queries-on-quit nil
        erc-log-write-after-send t
        erc-log-write-after-insert t
        erc-button-url-regexp
        "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]"
        erc-rename-buffers t)

  ;; Disable highlighting line
  (add-hook 'erc-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

  ;; Auto join
  (require 'erc-join)
  (erc-autojoin-mode t)
  (setq erc-autojoin-timing 'ident
        erc-autojoin-delay 3)

  ;; Truncate buffers so they don't hog core.
  (setq erc-max-buffer-size 20000)
  (defvar erc-insert-post-hook)
  (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
  (setq erc-truncate-buffer-on-save t)

  ;; Programmable completion for ERC
  (require 'erc-pcomplete)
  (pcomplete-erc-setup)
  (erc-completion-mode t)

  ;; Clears out annoying erc-track-mode stuff
  ;; Useful for when ChanServ restarts :P
  (require 'erc-track)
  (erc-track-mode t)
  (setq erc-track-exclude-server-buffer t
        erc-track-exclude-types '("JOIN" "LEAVE" "MODE" "NICK" "PART" "QUIT"
                                  "301" "305" "306" "324" "329" "332" "333"
                                  "353" "477"))

  (require 'erc-fill)
  (erc-fill-mode t)

  (require 'erc-ring)
  (erc-ring-mode t)

  (require 'erc-netsplit)
  (erc-netsplit-mode t)

  (erc-timestamp-mode t)
  (erc-button-mode nil)
  ;; prompts
  (setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))))

(add-hook 'erc-mode-hook 'myinc/messy-settings-for-erc)


;; http://www.emacswiki.org/emacs/ErcAutoQuery
(setq erc-auto-query 'buffer)
(add-hook 'erc-after-connect
          (lambda (server nick)
            (add-hook 'erc-server-NOTICE-hook 'erc-auto-query)))

(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda ()
                         (when (and (eq major-mode 'erc-mode)
                                    (not (null buffer-file-name)))))))


(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
(add-hook 'erc-mode-hook
          (lambda ()
            (setq erc-log-file-coding-system 'utf-8)
            (when (not (featurep 'xemacs))
              (set (make-variable-buffer-local
                    'coding-system-for-write)
                   'emacs-mule))))


;; display emoji
;; (when (maybe-require-package 'emojify)
;;   (setq emojify-emojis-dir (locate-user-emacs-file ".cache/emojis")
;;         emojify-download-emojis-p 't))
;; (add-hook 'erc-mode-hook #'global-emojify-mode)


;; erc-image
;; (require-package 'erc-image)
;; (add-hook 'erc-mode-hook
;;           (lambda ()
;;              (add-to-list 'erc-modules 'image)
;;              (erc-update-modules)
;;              (setq erc-image-inline-rescale 800)))


;; Dynamic colume adjustment
(add-hook 'window-configuration-change-hook
          (lambda ()
            (setq erc-fill-column (- (window-width) 2))))


;; Clears out annoying erc-track-mode stuff for when we don't care.
;; Useful for when ChanServ restarts :P
(defun reset-erc-track-mode ()
  "Clears out annoying stuff."
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))


;; Ignore specific annoying stuff
(defcustom erc-foolish-content '("gif.mp4$" ".webp$" ".webm$")
  "Regular expressions to identify foolish content.
Usually what happens is that you add the bots to
`erc-ignore-list' and the bot commands to this list."
  :group 'erc
  :type '(repeat regexp))

(defun erc-foolish-content (msg)
  "Check whether MSG is foolish."
  (erc-list-match erc-foolish-content msg))

(add-hook 'erc-insert-pre-hook
          (lambda (s)
            (when (erc-foolish-content s)
              (setq erc-insert-this nil))))


(provide 'init-erc)
;;; init-erc.el ends here
