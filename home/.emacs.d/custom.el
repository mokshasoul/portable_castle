(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("672bb062b9c92e62d7c370897b131729c3f7fd8e8de71fc00d70c5081c80048c" "fa2af0c40576f3bde32290d7f4e7aa865eb6bf7ebe31eb9e37c32aa6f4ae8d10" default)))
 '(custom-set-variables (quote (LaTeX-command "latex -synctex=1")) t)
 '(define-key
    (quote
     (keymap
      (9 . completion-at-point)
      (27 keymap
          (9 . completion-at-point))
      keymap
      (menu-bar keymap
                (minibuf "Minibuf" keymap
                         (previous menu-item "Previous History Item" previous-history-element :help "Put previous minibuffer history element in the minibuffer")
                         (next menu-item "Next History Item" next-history-element :help "Put next minibuffer history element in the minibuffer")
                         (isearch-backward menu-item "Isearch History Backward" isearch-backward :help "Incrementally search minibuffer history backward")
                         (isearch-forward menu-item "Isearch History Forward" isearch-forward :help "Incrementally search minibuffer history forward")
                         (return menu-item "Enter" exit-minibuffer :key-sequence "" :help "Terminate input and exit minibuffer")
                         (quit menu-item "Quit" abort-recursive-edit :help "Abort input and exit minibuffer")
                         "Minibuf"))
      (10 . exit-minibuffer)
      (13 . exit-minibuffer)
      (7 . minibuffer-keyboard-quit)
      (C-tab . file-cache-minibuffer-complete)
      (9 . self-insert-command)
      (XF86Back . previous-history-element)
      (up . previous-line-or-history-element)
      (prior . previous-history-element)
      (XF86Forward . next-history-element)
      (down . next-line-or-history-element)
      (next . next-history-element)
      (27 keymap
          (63 . session-minibuffer-history-help)
          (114 . previous-matching-history-element)
          (115 . next-matching-history-element)
          (112 . previous-history-element)
          (110 . next-history-element)))) t)
 '(global-set-key "k" t)
 '(ivy-count-format "")
 '(ivy-dynamic-exhibit-delay-ms 200)
 '(ivy-height 10)
 '(ivy-initial-input-alist nil t)
 '(ivy-magic-tilde nil)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote fullpath))
 '(package-selected-packages
   (quote
    (evil-org evil ztree ripgrep pkgbuild-mode auto-sudoedit ssh-config-mode ag nginx-mode monokai-theme molokai-theme yasnippet-snippets yagist whitespace-cleanup-mode which-key wgrep web-mode vlf virtualenvwrapper use-package unfill undo-tree try tagedit systemd symbol-overlay switch-window smarty-mode session scss-mode scratch sass-mode regex-tool rainbow-delimiters pipenv paredit-everywhere page-break-lines org-projectile org-cliplink org-bullets nov nlinum multiple-cursors mode-line-bell markdown-mode magit-gh-pulls lua-mode list-unicode-display leuven-theme js2-mode js-doc ivy-xref i3wm htmlize gnuplot git-timemachine ggtags fullframe flycheck-color-mode-line expand-region exec-path-from-shell emmet-mode elpy editorconfig dynamic-spaces dsvn dotenv-mode dockerfile-mode docker-compose-mode docker diredfl diminish diff-hl csv-mode crux counsel-projectile company-php company-jedi company-auctex company-ansible command-log-mode calfw-org calfw browse-kill-ring beacon ansible-doc ansible ace-window)))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))
