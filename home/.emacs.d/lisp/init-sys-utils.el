;;; Code:
;;; Minor Modes for config files
;; i3
(use-package i3wm
  :ensure t)
;; Systemd mode
(use-package systemd
  :ensure t)
;; SSH-Config
(use-package ssh-config-mode
  :ensure t)

(provide 'init-sys-utils)
