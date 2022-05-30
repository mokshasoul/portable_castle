;;; init-lsp --- LSP MODE

;; Copyright (C) 2022 Charis-Nicolas Georgiou

;; Author: Charis-Nicolas Georgiou <cng_it@posteo.net>
;; Created: 25 May 2022
;; Version: 1.0
;; Keywords: lsp
;; X-URL: https://github.com/mokshasoul/init-lsp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:
;;
(setq read-process-output-max (* 3 1024 1024))

(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         (clojure-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :command lsp-ui-mode)


(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :hook (
          (python-mode . dap-ui-mode)
          (python-mode . dap-mode)))

(use-package tree-sitter
  :ensure t
  :hook
  (python-mode . tree-sitter))

(use-package tree-sitter-langs
  :after (tree-sitter)
  :ensure t)

(provide 'init-lsp)
;;; init-lsp.el ends here
