;; Project management
;;; Code:
(use-package projectile
  :ensure t
  :bind (
         ("C-c p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'ivy
        projectile-enable-caching nil))
;; Projectile support for counsel
(use-package counsel-projectile
  :after (counsel projectile)
  :hook (after-init . counsel-projectile-mode))
(provide 'init-projectile)
;;; init-projectile.el ends here
