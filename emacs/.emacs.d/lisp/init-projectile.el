;;; init-projectile --- Projectile setup

;; Copyright (C) 2019 Charis-Nicolas Georgiou

;; Author: Charis-Nicolas Georgiou <cng_it@posteo.net>
;; Created: 24 Nov 2019
;; Version: 1.0
;; Keywords: projectile counsel
;; X-URL: https://github.com/mokshasoul/init-projectile

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
;; We use projectile together with counsel
;; as of the latest versions we just need to enable counsel
;; after init

(provide 'init-projectile)

;;; init-projectile.el ends here

;; Project management
;;; Code:
(use-package projectile
  :ensure t
  :bind (
         ("C-c p" . projectile-command-map))
  :hook
  (after-init . projectile-mode)
  :config (
           setq projectile-completion-system 'ivy
           projectile-enable-caching nil
           projectile-git-submodule-command nil))

;; Projectile support for counsel
(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

(provide 'init-projectile)
;;; init-projectile.el ends here
