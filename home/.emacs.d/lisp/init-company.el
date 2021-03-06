;;; init-company --- Company Configuration

;; Copyright (C) 2020 Charis-Nicolas Georgiou

;; Author: Charis-Nicolas Georgiou <cng_it@posteo.net>
;; Created: 11 Feb 2020
;; Version: 1.0
;; Keywords: company-mode autocomplete company
;; X-URL: https://github.com/mokshasoul/dotfiles

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
(setq tab-always-indent 'complete)

(use-package company
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 3)
  (global-company-mode t))

(add-to-list 'completion-styles 'initials t)

(use-package company-jedi
  :config
  (progn
    (defun my/python-mode-hook ()
      (add-to-list 'company-backends  'company-jedi))
    (add-hook 'python-mode-hook 'my/python-mode-hook)))

(use-package company-auctex
  :after (company latex))
(use-package company-php
  :after (company php))

(provide 'init-company)
;;; init-company.el ends here
