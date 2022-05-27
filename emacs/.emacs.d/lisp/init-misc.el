;;; init-misc --- MISCELLANEOUS PACKAGES

;; Copyright (C) 2020 Charis-Nicolas Georgiou

;; Author: Charis-Nicolas Georgiou <cng_it@posteo.net>
;; Created: 01 Feb 2020
;; Version: 1.0
;; Keywords: crux misc
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
;;; Miscallaneous packages and functions

;;; Code:
;; Here are extra packages that provide QoL features
;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
(add-auto-mode 'tcl-mode "^Portfile\\'")
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'prog-mode-hook 'goto-address-prog-mode)

(use-package regex-tool
  :config
  (setq-default regex-tool-backend 'perl))

(use-package dotenv-mode
  :ensure t
  :mode "\\.env\\..*\\'")

;; Set URL-Handler
(if (eq system-type 'darwin)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (setq browse-url-browser-function 'browse-url-xdg-open))


(setq auto-mode-alist
  (append
    (list
      '("\\.\\(vcf\\|gpg\\)$" . sensitive-minor-mode)
      )
    auto-mode-alist))
(progn
  (setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))
  (setq-default save-place-mode 1)
  (setq save-interprogram-paste-before-kill t
    require-final-newline t
    visible-bell t
    load-prefer-newer t

    save-place-file (concat user-emacs-directory "places")
    backup-directory-alist `(("." . ,(concat user-emacs-directory
						                           "backups")))))
;;; Faster than default
(setq tramp-default-method "ssh")
;;; required for sudo piping on remote host
(add-to-list 'tramp-default-host-alist
             '("reno" "root" "/ssh:admin@reno:")
             '("dragon" "root" "/ssh:admin@dragon:"))

;;; Default is org
(setq-default major-mode 'org-mode)
;;; Backup and autosave to temp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))
(setq lock-file-name-transforms
  `((".*" ,(concat user-emacs-directory "locks/") t)))
;;; CruX
(use-package crux)
(provide 'init-misc)

;;; init-misc.el ends here
