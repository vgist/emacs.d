;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  ;; Work with neotree, when C-c p p, neotree will change root automatically
  (setq projectile-switch-project-action 'neotree-projectile-action)

  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden"))

  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

  (maybe-require-package 'ibuffer-projectile))


(provide 'init-projectile)
;;; init-projectile.el ends here
