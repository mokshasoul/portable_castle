;;; init --- Summary
;;; Commentary:
;;; Code:
;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold (* 20 1024 1024)))))
;;; Follow simlinks to git controlled files
(setq vc-follow-symlinks t)

;;; Add lisp folder to path in order to load the init files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Bootstrap config (taken from purcell)
;;----------------------------------------------------------------------------
;; Offload custom file which gets autowritten by package
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; disable annoying s-SPC undefined
(define-key global-map (kbd "s-SPC") 'ignore)
(require 'init-utils)

(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)   ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

(use-package wgrep)
(use-package diminish)
(use-package scratch)
(use-package command-log-mode)
(use-package dash)
;; Configure interface
(require 'init-theme)
(require 'init-gui-frames)
(require 'init-windows)
;; Better dired configuration, taken from purcell
(require 'init-dired)
(require 'init-grep)
;; Make unique buffer names
(require 'init-uniquify)
;; (require 'init-ibuffer)
;; Autocompletion, syntax checkers and expanders
;;; FlyCheck
(use-package flycheck
  :init
  (global-flycheck-mode)
  :ensure t)

(use-package flycheck-color-mode-line
  :ensure t
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(setq flymake-start-on-flymake-mode nil)
;;; Hippe Expand
(use-package hippie-exp
  :bind (("M-/"   . hippie-expand)
         ("C-M-/" . dabbrev-completion)))

(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))


(require 'init-ivy)
;;; Company
(setq tab-always-indent 'complete)

(use-package company
  :defer 5
  :diminish
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 3)
  (global-company-mode t))

(add-to-list 'completion-styles 'initials t)


(use-package company-auctex
  :ensure t
  :after (company latex))

(use-package company-php
  :ensure t
  :after (company php))

(use-package company-terraform
  :after (company terraform-mode)
  :ensure t)

(use-package company-ansible
  :after (company ansible))

;; Projectmanagement
;; Projectile
(require 'init-projectile)
(require 'init-recentf)

;; (require 'init-smex) we use swiper and ivy
;; Org mode
(require 'init-org)
;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)


(desktop-save-mode t)

;;----------------------------------------------------------------------------
;; Restore histories and registers after saving
;;----------------------------------------------------------------------------
(setq-default history-length 1000)
(add-hook 'after-init-hook 'savehist-mode)

(use-package session :ensure t)

(setq session-save-file (expand-file-name ".session" user-emacs-directory)
      session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)"
      session-save-file-coding-system 'utf-8)
(add-hook 'after-init-hook 'session-initialize)
;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((comint-input-ring        . 50)
                (compile-history          . 30)
                desktop-missing-file-warning
                (dired-regexp-history     . 20)
                (extended-command-history . 30)
                (face-name-history        . 20)
                (file-name-history        . 100)
                (grep-find-history        . 30)
                (grep-history             . 30)
                (ido-buffer-history       . 100)
                (ido-last-directory-list  . 100)
                (ido-work-directory-list  . 100)
                (ido-work-file-list       . 100)
                (ivy-history              . 100)
                (magit-read-rev-history   . 50)
                (minibuffer-history       . 50)
                (org-clock-history        . 50)
                (org-refile-history       . 50)
                (org-tags-history         . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                register-alist
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                tags-table-list)))

(require 'init-editing-utils)

(use-package flyspell
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode)))

;;; CSV Mode settings
(use-package csv-mode
  :ensure t
  :init
  (setq csv-separators '("," ";" "|" " ")))
(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")


;; Languages
;;; python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))) ; or lsp-defered for lazy
(use-package poetry
  :hook (
         (python-mode . poetry-tracking-mode)))

(use-package pipenv)

(setq flycheck-flake8-maximum-line-length 120)

;;; PHP
(use-package php-mode)
(use-package smarty-mode)

;;; Go
(use-package go-mode)
;;; LaTeX
(use-package auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . auto-fill-mode)
         (LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . flyspell-mode))
  :custom
  (custom-set-variables '(LaTeX-command "latex -synctex=1"))
  :init
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  (setq TeX-view-program-list
        '(("PDF Viewer" "okular --unique %o#src:%n%b")))
  (setq TeX-view-program-selection
        '((output-pdf "PDF Viewer"))))

;; Devops Stuffnn
;;; Ansible
(use-package ansible
  :ensure t)

(use-package ansible-doc
  :ensure t)
;;; Docker

;;; TODO: jwiegly config
(use-package docker)
(use-package docker-compose-mode)
(use-package dockerfile-mode
  :mode "\Dockerfile\\'")

;;; WebDev Stuff
(use-package web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.djhtml?\\'" . web-mode))
  :init
  (setq web-mode-code-indent-offset 2
        web-mode-markupt-indent-offset 2
        web-mode-enable-auto-pairing nil))

;; make web-mode play nice with smartparens taken from
;; prelude
;; (sp-with-modes '(web-mode)
;;   (sp-local-pair "%" "%"
;;                  :unless '(sp-in-string-p)
;;                  :post-handlers '(((lambda (&rest _ignored)
;;                                      (just-one-space)
;;                                      (save-excursion (insert " ")))
;;                                    "SPC" "=" "#")))
;;   (sp-local-tag "%" "<% "  " %>")
;;   (sp-local-tag "=" "<%= " " %>")
;;   (sp-local-tag "#" "<%# " " %>")
;;; EMMET is nice ;)
(use-package emmet-mode
  :requires web-mode
  :hook web-mode
  :init
  (setq emmet-indentation 2))

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

;; Javascript

;;; Currently top javascript package for emacs
(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.pac\\'" . js2-mode))
  :commands js2-mode
  :interpreter "node"
  :config
  (setq mode-name "JS2")
  (js2-imenu-extras-mode t)
  (setq-default
   js2-mode-indent-ignore-first-tab t))

;; JSON support
(use-package json-mode
  :ensure t
  :commands json-mode
  :config
  (bind-keys :map json-mode-map
             ("C-c <tab>" . json-mode-beautify)))
;; Adddinsx
(use-package json-snatcher :ensure t)
(use-package js-doc :ensure t)

(use-package tagedit
  :ensure t
  :mode "\\.\\(jsp\\tmpl\\)\\'"
  :config
  (after-load 'sgml-mode
    (tagedit-add-paredit-like-keybindings)
    (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1)))))
;;; LISP
(require 'init-lisp)
(require 'init-slime)
;;; CLOJURE
(require 'init-clojure)
(require 'init-clojure-cider)
;; Utils
;;; Paredit
(use-package paredit
  :diminish
  :ensure t
  :init
  (autoload 'enable-paredit-mode "paredit"))

(defun maybe-map-paredit-newline ()
  (unless (or (memq major-mode '(inferior-emacs-lisp-mode cider-repl-mode))
              (minibufferp))
    (local-set-key (kbd "RET") 'paredit-newline)))

(after-load 'paredit
  (diminish 'paredit-mode " Par")
  ;; Suppress certain paredit keybindings to avoid clashes, including
  ;; my global binding of M-?
  (dolist (binding '("C-<left>" "C-<right>" "C-M-<left>" "C-M-<right>" "M-s" "M-?"))
    (define-key paredit-mode-map (read-kbd-macro binding) nil)))


;; Compatibility with other modes


;; Use paredit in the minibuffer
;; TODO: break out into separate package
;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
  "Interactive commands for which paredit should be enabled in the minibuffer.")

(defun conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))

(require 'init-editing-utils)
;;; Whitespace management
(setq-default show-trailing-whitespace t)

(defun sanityinc/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))
;;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook #'sanityinc/no-trailing-whitespace))
(use-package whitespace-cleanup-mode
  :ensure t
  :hook (after-init . global-whitespace-cleanup-mode))
(global-set-key [remap just-one-space] 'cycle-spacing)
;;; YaSnippet
(require 'init-yasnippet)
;; GIT Setup
;;; Magit
(use-package magit
  :ensure t
  :bind (("<f8>" . magit-blame)
         ("C-x g" . magit-status))
  :commands (magit-status
             magit-blame
             magit-checkout
             magit-og-buffer))
;;; Timemachine
(use-package git-timemachine :ensure t)
;; Snippets from gist
(use-package yagist :ensure t)
(use-package magit-gh-pulls
  :hook (magit-mode-hook . turn-on-magit-gh-pulls))

(use-package git-modes
  :ensure t)
;; YAML, Markdown and latex support
;;; YAML
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\.erb\\'"
  :hook (yaml-mode . goto-address-prog-mode))
;;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(after-load 'whitespace-cleanup-mode
  (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))


;; Misc
;;; EPUB
(use-package nov
  :mode "\\.epub\\'")
;;; System Utilities
;; i3
(use-package i3wm
  :ensure t)

;; Systemd mode
(use-package systemd
  :ensure t)

;; SSH-Config
(use-package ssh-config-mode
  :ensure t)

;; fish-configs
(use-package fish-mode
  :ensure t)

(require 'init-misc)
;; Extra packages which don't require any configuration purcell
(use-package gnuplot)
(use-package lua-mode)
(use-package htmlize)
(use-package dsvn)
(use-package try) ;; test install packages
(use-package ggtags)
;;; Setup evil
;; (use-package evil
;;  :ensure t
;;  :init
;;  (evil-mode t))
;;; `server' for emacs
(require 'server)
(if (not (eq system-type 'windows-nt))
  (unless (server-running-p)
    (server-start)))
;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))
;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(defun sanityinc/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun sanityinc/locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (sanityinc/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (sanityinc/utf8-locale-p (getenv "LC_ALL"))
      (sanityinc/utf8-locale-p (getenv "LC_CTYPE"))
      (sanityinc/utf8-locale-p (getenv "LANG"))))

(when (or window-system (sanityinc/locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (set-language-environment-coding-systems 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))

;;----------------------------------------------------------------------------
;; Personal settings
;;----------------------------------------------------------------------------

(setq user-full-name "Charis-Nicolas Georgiou"
      user-mail-address "cng_it@posteo.net")
(save-place-mode)
(add-hook 'before-save-hook 'save-place-kill-emacs-hook)
(setq epa-pinentry-mode 'loopback) ; This will fail if gpg>=2.1 is not available.
(when (require 'pinentry nil t)
  (pinentry-start))

(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
