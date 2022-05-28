;;; init-ansible --- Initialize ansible settings

;; Copyright (C) 2018 Charis-Nicolas Georgiou

;; Author: Charis-Nicolas Georgiou <cng_it@posteo.net>
;; Created: 25 Apr 2018
;; Version: 1.0
;; Keywords: dotfiles ansible
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
;; This is awesomeness
;;; Code:
(use-package ansible
  :ensure t)
(use-package ansible-doc
  :ensure t)
(use-package company-ansible
  :ensure t)

(provide 'init-ansible)
;;; init-ansible.el ends here
