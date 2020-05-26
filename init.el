;;; init.el --- Load the full configuration -*- lexical-binding: t; -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Emacs v%s or higher is required" minver)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name ".tmp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Bootstrap config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;; Allow users to provide an optional "init-preload-local.el"
(require 'init-preload-local nil t)

;; Load configs for specific features and modes
(require-package 'diminish)
(maybe-require-package 'scratch)

(require 'init-frame-hooks)
(require 'init-global)
(require 'init-term)
(require 'init-themes)
(require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-whitespace)

(require 'init-ivy)
(require 'init-smex)
(require 'init-company)
(require 'init-mmm)
(require 'init-neotree)

(require 'init-css)
(require 'init-csv)
(require 'init-docker)
(require 'init-markdown)
(require 'init-php)
(require 'init-python)
(require 'init-rails)
(require 'init-ruby)
(require 'init-rust)
(require 'init-yaml)
(require 'init-vimrc)

(require 'init-folding)
(require 'init-erc)

(when *is-a-mac*
  (require-package 'osx-location))
(unless (eq system-type 'windows-nt)
  (maybe-require-package 'daemons))
(maybe-require-package 'dotenv-mode)

(when (maybe-require-package 'uptimes)
  (setq-default uptimes-keep-count 200)
  (add-hook 'after-init-hook (lambda () (require 'uptimes))))

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;; Allow users to provide an optional "init-local" containing personal settings
(require 'init-local nil t)

(when *is-a-mac*
  (delete-file "~/Library/Colors/Emacs.clr"))

(provide 'init)
;;; init.el ends here
