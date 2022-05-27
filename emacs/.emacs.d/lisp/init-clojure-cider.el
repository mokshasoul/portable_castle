;;; init-clojure-cider.el --- Cider support for clojure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-clojure)

(use-package cider
  :ensure t :defer t
  :config
  (setq
    cider-repl-history-file ".cider-repl-history"  ;; not squiggly-related, but I like it
    nrepl-log-messages t)                          ;; not necessary, but useful for trouble-shooting
  (flycheck-clojure-setup))                        ;; run setup *after* cider load

(use-package flycheck-clojure
  :defer t
  :after (clojure flycheck)
  :commands (flycheck-clojure-setup)               ;; autoload
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'init-clojure-cider)
;;; init-clojure-cider.el ends here
