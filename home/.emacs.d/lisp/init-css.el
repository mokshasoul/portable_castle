;;; init-css --- CSS Configuration

;; Copyright (C) 2020 Charis-Nicolas Georgiou

;; Author: Charis-Nicolas Georgiou <cng_it@posteo.net>
;; Created: 11 Feb 2020
;; Version: 1.0
;; Keywords: css
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
;; Install SASS-Mode, assign to extensions, set indent on init
(use-package sass-mode
  :ensure t
  :mode "\\.sass\\.erb"
  :init
  (setq css-indent-offset 2))
;; Install SCSS-Mode assign file endings
(use-package scss-mode
  :ensure t
  :mode "\\.scss"
  :init
  (setq-default scss-compile-at-save nil))

(provide 'init-css)
;;; init-css.el ends here
