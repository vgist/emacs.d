;;; init-flymake.el --- Configure Flymake global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'flymake "1.2.1")

;; Prevent Flymake from creating/populating the *Flymake log* buffer.
;; Since Emacs 26, `flymake-log-level' is obsolete and ignored: Flymake
;; emits its log messages via `display-warning', which (by default
;; `warning-minimum-log-level' = :warning) unconditionally creates the
;; *Flymake log* buffer for any :warning-or-higher message, e.g. when a
;; backend gets disabled on startup. Suppressing the 'flymake warning
;; type stops that buffer from ever being created.
(add-to-list 'warning-suppress-log-types '(flymake))

;; Use flycheck checkers with flymake, to extend its coverage
(when (maybe-require-package 'flymake-flycheck)
  ;; Disable flycheck checkers for which we have flymake equivalents
  (with-eval-after-load 'flycheck
    (setq-default
     flycheck-disabled-checkers
     (append (default-value 'flycheck-disabled-checkers)
             '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package sh-shellcheck))))

  (add-hook 'flymake-mode-hook 'flymake-flycheck-auto)
  (add-hook 'prog-mode-hook 'flymake-mode)
  (add-hook 'text-mode-hook 'flymake-mode))


(with-eval-after-load 'flymake
  ;; Provide some flycheck-like bindings in flymake mode to ease transition
  (define-key flymake-mode-map (kbd "C-c ! l") 'flymake-show-buffer-diagnostics)
  (define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c ! c") 'flymake-start))

(unless (version< emacs-version "28.1")
  (setq eldoc-documentation-function 'eldoc-documentation-compose)

  (add-hook 'flymake-mode-hook
            (lambda ()
              (add-to-list 'eldoc-documentation-functions 'flymake-eldoc-function))))

(provide 'init-flymake)
;;; init-flymake.el ends here
