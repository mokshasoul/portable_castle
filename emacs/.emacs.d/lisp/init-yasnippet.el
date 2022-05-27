;;; init-yasnippet --- YASnippet Configuration

;; Copyright (C) 2022 Charis-Nicolas Georgiou

;; Author: Charis-Nicolas Georgiou <cng_it@posteo.net>
;; Created: 27 May 2022
;; Version: 1.0
;; Keywords: prog
;; X-URL: https://github.com/mokshasoul/portable_castle

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
;; Test 1 2 3
;;; Code:
;;

(use-package yasnippet
  :after prog-mode
  :defer 10
  :diminish yas-minor-mode
  :init
  (add-hook 'after-init-hook 'yas-global-mode)
  (add-hook 'term-mode-hook #'force-yasnippet-off)
  (add-hook 'shell-mode-hook #'force-yasnippet-off)
  :config
  (yas-global-mode 1)
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t)

(defun force-yasnippet-off ()
  (setq-local yas-dont-activate t)
  (yas-minor-mode 1))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
