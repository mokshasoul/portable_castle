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

;;; SETUP EXEC PATH
(use-package exec-path-from-shell)

(after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package wgrep)
(use-package diminish)
(use-package scratch)
(use-package command-log-mode)
(use-package dash)
;; Configure interface
;;; Themes
(use-package intellij-theme)
(use-package leuven-theme)
(use-package monokai-theme)
(add-to-list 'default-frame-alist '(font . "Hack-12"))
(load-theme 'leuven t)

;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode nil))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'pixel-scroll-mode)
  (pixel-scroll-mode 1))
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

(setq scroll-step 1)

;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
;;; Code:
(add-hook 'after-init-hook 'winner-mode)


;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)


;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'sanityinc/split-window)



(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(global-set-key (kbd "C-c <down>") 'sanityinc/toggle-current-window-dedication)



(unless (memq window-system '(nt w32))
  (windmove-default-keybindings 'control))
;;; Ace-Window Configuration
(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-dispatch-always nil
        aw-keys '(?a ?s ?d ?f ?g ?j ?k ?l))
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 2.0))))))

;; Better dired configuration, taken from purcell
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

;; Grep Configuration
(setq-default grep-highlight-matches t
              grep-scroll-output t)

;; If executable ag exists, download package
(when (and (executable-find "ag")
           (use-package ag :ensure t))
  (use-package wgrep-ag
    :ensure t
    :init
    (setq-default ag-highlight-search t)))

(when (and (executable-find "rg")
           (use-package rg :ensure t)))

;; Make unique buffer names
(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(use-package fullframe
  :init
  (fullframe list-packages quit-window))

(after-load 'ibuffer
  (fullframe ibuffer ibuffer-quit))

(use-package ibuffer-vc
  :after (ibuffer))

(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)

(setq-default ibuffer-show-empty-filter-groups nil)


(after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size))))))

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


(use-package ivy
  :diminish
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-dynamic-exhibit-delay-ms 200)
  (ivy-height 10)
  (ivy-initial-input-alist nil t)
  (ivy-use-virtual-buffers t)
  (ivy-magic-tilde nil)
  (ivy-virtual-abbreviate 'fullpath)
  (ivy-count-format "")
  :config
  (ivy-mode 1)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur))

(use-package counsel
  :after ivy
  :demand t
  :diminish
  :bind (("C-*" . counsel-org-agenda-headlines)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c l" . counsel-locate))
  :commands counsel-minibuffer-history
  :custom
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

;; (use-package counsel-tramp
;;   :commands counsel-tramp)

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper))
  :config
  (progn
    (setq-default
     magit-completing-read-function 'ivy-completing-read
     enable-recursive-minibuffers t)))

(use-package ivy-xref
  :config
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))

(use-package lsp-ivy
  :after (lsp-mode ivy))
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
(use-package projectile
  :ensure t
  :bind (
         ("C-c p" . projectile-command-map))
  :hook
  (after-init . projectile-mode)
  :config (
           setq projectile-completion-system 'ivy
           projectile-git-submodule-command nil))

;; Projectile support for counsel
(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

(use-package recentf
  :hook (after-init . recentf-mode)
  :init
  (setq-default
   recentf-max-saved-items 40
   recentf-exclude '("/tmp/" "/ssh:")))


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
(use-package ansible)
(use-package ansible-doc)

;;; Terraform
(use-package terraform-mode)

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
(setq-default debugger-bury-or-kill 'kill)

(use-package elisp-slime-nav)

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ELisp")))

(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

(use-package ipretty
  :hook (after-init . ipretty-mode))



;; Automatic byte compilation
(use-package auto-compile
  :config
  (setq auto-compile-delete-stray-dest nil)
  (add-hook 'after-init-hook 'auto-compile-on-save-mode)
  (add-hook 'after-init-hook 'auto-compile-on-load-mode))


;; Load .el if newer than corresponding .elc

(setq load-prefer-newer t)



(use-package immortal-scratch
  :hook (after-init . immortal-scratch))

(use-package aggressive-indent
  :hook (lisp-mode . aggressive-indent-mode))


(require 'derived)

(use-package macrostep)
(with-eval-after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c x") 'macrostep-expand))


;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") 'find-function-on-key)

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))
(use-package flycheck-relint)
(use-package cask-mode)

(use-package slime)
(push (expand-file-name "contrib" (file-name-directory (locate-library "slime"))) load-path)

(use-package slime-company
  :after (slime company)
  :init
  (setq  slime-company-completion 'fuzzy
         slime-company-after-completion 'slime-company-just-one-space))



;;; Lisp buffers

(with-eval-after-load 'slime
  (setq slime-protocol-version 'ignore)
  (setq slime-net-coding-system 'utf-8-unix)
  (let ((features '(slime-fancy slime-repl slime-fuzzy)))
    (when (require 'slime-company nil t)
      (push 'slime-company features))
    (slime-setup features)) )


;;; REPL

(with-eval-after-load 'slime-repl
  ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (with-eval-after-load 'paredit
    (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil))

  ;; Bind TAB to `indent-for-tab-command', as in regular Slime buffers.
  (define-key slime-repl-mode-map (kbd "TAB") 'indent-for-tab-command)

  (add-hook 'slime-repl-mode-hook 'sanityinc/slime-repl-setup))
;;; CLOJURE
(use-package clojure-mode)

(use-package cljsbuild-mode
  :after (clojure-mode))

(use-package elein
  :after (clojure-mode cljsbuld-mode))

(use-package cider
  :ensure t :defer t
  :config
  (setq cider-repl-history-file ".cider-repl-history"  ;; not squiggly-related, but I like it
        nrepl-log-messages t)                          ;; not necessary, but useful for trouble-shooting
  (flycheck-clojure-setup))                        ;; run setup *after* cider load

(use-package flycheck-clojure
  :defer t
  :after (clojure flycheck)
  :commands (flycheck-clojure-setup)               ;; autoload
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;; LSP
(setq read-process-output-max (* 3 1024 1024))

(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         (clojure-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)


(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :hook (
         (python-mode . dap-ui-mode)
         (python-mode . dap-mode))
  :init
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (defun dap-python--pyenv-executable-find (command)
    (with-venv (executable-find "python")))
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

(use-package tree-sitter
  :ensure t
  :hook
  (python-mode . tree-sitter))

(use-package tree-sitter-langs
  :after (tree-sitter)
  :ensure t)

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
(use-package yasnippet
  :after prog-mode
  :defer 10
  :diminish yas-minor-mode
  :init
  (add-hook 'after-init-hook 'yas-global-mode)
  (add-hook 'term-mode-hook #'force-yasnippet-off)
  (add-hook 'shell-mode-hook #'force-yasnippet-off)
  :config
  (yas-global-mode 1)
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t)

(defun force-yasnippet-off ()
  (setq-local yas-dont-activate t)
  (yas-minor-mode 1))

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
