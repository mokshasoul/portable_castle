;;; init-python.el --- Personal configuration for python -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))) ; or lsp-defered for lazy


(use-package pyvenv
  :ensure t
  :hook (
          (python-mode . pyvenv-activate)))

(use-package poetry
  :ensure t
  :hook (
          (python-mode . poetry-tracking-mode)))

(use-package pipenv
  :ensure t)

(setq flycheck-flake8-maximum-line-length 120)

(provide 'init-python)
;;; init-python.el Ends here
