;;; init-terraform --- Terraform Configuration

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
;;; Help 1 2 3

;;; Code:
;;
(use-package terraform-mode
  :ensure t)

(use-package company-terraform
  :after (company)
  :ensure t)

(provide 'init-terraform)
;;; init-terraform.el ends here