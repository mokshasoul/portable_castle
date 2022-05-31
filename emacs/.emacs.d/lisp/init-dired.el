;;; init-dired --- Dired Configuration

;; Copyright (C) 2022 Charis-Nicolas Georgiou

;; Author: Charis-Nicolas Georgiou <cng_it@posteo.net>
;; Created: 30 May 2022
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

;; My Dired configuration

;;; Code:
;;
(require 'dired-x)

(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))


;; More colors for dired
(use-package diredfl
  :ensure t
  :after dired
  :config
  (diredfl-global-mode))

(after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode))



(use-package diff-hl
  :ensure t
  :config
  (after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(provide 'init-dired)

;;; init-dired.el ends here
