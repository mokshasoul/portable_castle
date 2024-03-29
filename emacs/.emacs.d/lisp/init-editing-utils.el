;;; init-editing-utils --- EditingUtils

;; Copyright (C) 2022 Charis-Nicolas Georgiou

;; Author: Charis-Nicolas Georgiou <cng_it@posteo.net>
;; Created: 25 May 2022
;; Version: 1.0
;; Keywords: wp
;; X-URL: https://github.com/mokshasoul/portable_castle/

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
;;; Utilities taken from different repos
;;; Code:
;;
(use-package unfill :ensure t)
;;; visual line wrap
(visual-line-mode t)
(global-visual-line-mode t)
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(when (eval-when-compile (version< "24.4" emacs-version))
  (add-hook 'after-init-hook 'electric-indent-mode))

(use-package list-unicode-display
  :ensure t)

(setq-default blink-cursor-interval 0.4
  bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
  buffers-menu-max-size 30
  case-fold-search t
  column-number-mode t
  delete-selection-mode t
  ediff-split-window-function 'split-window-horizontally
  ediff-window-setup-function 'ediff-setup-windows-plain
  indent-tabs-mode nil ;;turns tabs to spaces
  make-backup-files nil
  mouse-yank-at-point t
  save-interprogram-paste-before-kill t
  scroll-preserve-screen-position 'always
  set-mark-command-repeat-pop t
  tooltip-delay 1.5
  truncate-lines nil
  truncate-partial-width-windows nil
  scroll-conservatively 10000)

;;; Autoread file changed on disk
(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'transient-mark-mode)
;;; NFO-File Fix
(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))

(use-package dynamic-spaces
  :ensure t
  :init
  (dynamic-spaces-global-mode))


;; Huge files
(use-package vlf  :ensure t)

(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))
(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))

(use-package beacon
  :diminish
  :hook (after-init . beacon-mode)
  :init
  (setq-default beacon-lighter "")
  (setq-default beacon-size 5))


(global-set-key (kbd "RET") 'newline-and-indent)

(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)


;;; todo figure out how to make it nice in use-package
(use-package symbol-overlay
  :diminish
  :hook ((prog-mode . symbol-overlay-mode)
         (html-mode . symbol-overlay-mode)
         (css-mode . symbol-overlay-mode))
  :config
  (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
  (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev))

(use-package browse-kill-ring
  :init
  (setq browse-kill-ring-separator "\f")
  (global-set-key (kbd "M-Y") 'browse-kill-ring))
(after-load 'browse-kill-ring
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))
(after-load 'page-break-lines
  (push 'browse-kill-ring-mode page-break-lines-modes))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook 'show-paren-mode)
(setq show-paren-delay 0)
(setq show-paren-when-point-inside-paren t)

;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(use-package expand-region
  :init
  (global-set-key (kbd "C-=") 'er/expand-region))

;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;----------------------------------------------------------------------------
;; Rectangle selections, and overwrite text when the selection is active
;;----------------------------------------------------------------------------
(cua-selection-mode t)                  ; for rectangles, CUA is nice
;;--------------------
;;; Set CUA-Mode: Re-enable wif we want it
;;--------------------
;; (cua-mode 1)
;; (setq cua-auto-tabify-rectangles nil) ;; on't tabify after rectangle command
;; (transient-mark-mode 1) ;; no region when it is not highlighted
;; (setq cua-keep-region-after-copy nil) ;; standard windows behavior



(use-package avy
  :bind* ("C-;" . avy-goto-char-timer)
  :config
  (avy-setup-default))
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))
(use-package multiple-cursors
  :init
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  ;; From active region to multiple cursors:
  (global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
  (global-set-key (kbd "C-c m c") 'mc/edit-lines)
  (global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines))

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)
;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

(use-package smartparens-config
  :ensure smartparens
  :config (progn (show-smartparens-global-mode t))
  :hook ((prog-mode markdown-mode) . 'turn-on-smartparens-strict-mode))

;; (use-package smartparens-config
;;   :commands smartparens-mode)

;;; Which-key Configuration (interactive key commands)
(use-package which-key
  :defer 0
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode))
;; Global line mode
(global-linum-mode t)

;;; Remap list-buffers to buffer menu
(global-set-key [remap list-buffers] #'buffer-menu)

(provide 'init-editing-utils)

;;; init-editing-utils.el ends here
