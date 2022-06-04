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
(require 'init-flycheck)
(require 'init-ivy)
(require 'init-hippie-expand)

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
(require 'init-sessions)
(require 'init-editing-utils)
(require 'init-flyspell)
;;; CSV Mode settings
(require 'init-csv)
;; Languages
(require 'init-python)

;;; PHP
(use-package php-mode)
(use-package smarty-mode)

(require 'init-latex)
;;; Devops Stuffnn
(use-package ansible
  :ensure t)

(use-package ansible-doc
  :ensure t)

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

(require 'init-js)
;;; Paredit
(require 'init-paredit)
(require 'init-html)
;;; Untils
(require 'init-editing-utils)
(require 'init-whitespace)
(require 'init-yasnippet)
(require 'init-git)
;; YAML, Markdown and latex support
(require 'init-yaml)
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(after-load 'whitespace-cleanup-mode
  (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))

(require 'init-lisp)
(require 'init-slime)
(require 'init-clojure)
(require 'init-clojure-cider)
;; Misc
(require 'init-docker)
(require 'init-docmodes)
(require 'init-sys-utils)
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
(require 'init-locales)
;;----------------------------------------------------------------------------
;; Personal settings
;;----------------------------------------------------------------------------
(require 'init-personal)
(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
