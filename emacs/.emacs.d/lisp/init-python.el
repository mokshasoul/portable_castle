;;; init-python.el --- Personal configuration for python -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))) ; or lsp-defered for lazy


(use-package virtualenvwrapper
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(use-package poetry
  :ensure t)
(use-package pipenv
  :ensure t
  :hook (python-mode . poetry-tracking-mode))

(setq flycheck-flake8-maximum-line-length 80)

(provide 'init-python)
;;; init-python.el Ends here
