;;; Commentary:

;;; Code:
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; Steve purcell org settings
;; Various preferences
(setq org-refile-targets `(
                            (,org-directory :maxlevel . 9)
                            (,org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling                           (org-agenda-files :maxlevel . 9)))
(setq org-log-done t
  org-edit-timestamp-down-means-later t
  org-archive-mark-done t
  org-hide-emphasis-markers t
  org-catch-invisible-edits 'show
  org-export-coding-system 'utf-8
                                        ; org-fast-tag-selection-single-key 'expert
  org-html-validation-link nil
  org-export-kill-product-buffer-when-displayed t
  org-tags-column 5
  org-directory (expand-file-name "_org/" "~")
  org-agenda-files  (list (expand-file-name "agenda/" org-directory))
  org-default-notes-file (expand-file-name "notes.org" org-directory))
;; inserts full filename at top of file to link different org files
;; (use-package org-fstree
;;   :ensure t)
;; ****************************************************************
;; GTD Configuration
;; ****************************************************************
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

;; link from clip
(use-package org-cliplink
  :no-require
  :config
  (define-key org-mode-map (kbd "C-c M-l") 'org-cliplink))

;;; Cool Bullets
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;; Set Org-Capture templates
(setq org-capture-templates
  `(("t" "todo" entry
      (file+headline ,(expand-file-name "tasks.org" org-agenda-files) "Tasks")
      "** TODO %^{Brief description} %^g\n%?\nAdded: %U")
     ("n" "note" entry
       (file+headline ,org-default-notes-file "Notes")
       "** :NOTE:\n%U\n%a\n" :clock-keep t)
     ("h" "habit" entry
       (file+headline ,(expand-file-name "tasks.org" org-agenda-files) "Habits")
       "** TODO %^{Brief description}%^t")))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Lots of stuff from http://doc.norang.ca/org-mode.html

;; TODO: fail gracefully
(defun sanityinc/grab-ditaa (url jar-name)
  "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
  ;; TODO: handle errors
  (message "Grabbing " jar-name " for org.")
  (let ((zip-temp (make-temp-name "emacs-ditaa")))
    (unwind-protect
        (progn
          (when (executable-find "unzip")
            (url-copy-file url zip-temp)
            (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
                                   " " (shell-quote-argument jar-name) " > "
                                   (shell-quote-argument org-ditaa-jar-path)))))
      (when (file-exists-p zip-temp)
        (delete-file zip-temp)))))

(after-load 'ob-ditaa
  (unless (and (boundp 'org-ditaa-jar-path)
               (file-exists-p org-ditaa-jar-path))
    (let ((jar-name "ditaa0_9.jar")
          (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
      (setq org-ditaa-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
      (unless (file-exists-p org-ditaa-jar-path)
        (sanityinc/grab-ditaa url jar-name)))))

(after-load 'ob-plantuml
  (let ((jar-name "plantuml.jar")
        (url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
    (setq org-plantuml-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
    (unless (file-exists-p org-plantuml-jar-path)
      (url-copy-file url org-plantuml-jar-path))))

;;; integrate projectile todos with org-todos
(use-package org-projectile
  :after (org projectile)
  :bind (("C-c n p" . org-projectile-project-todo-completing-read))
  :config
  (progn
    (setq org-projectile-projects-file
          (expand-file-name "projects.org" org-directory))
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))

;; Calendar packages for org
(use-package calfw)
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

(use-package calfw-org
  :after calfw)

(use-package org-roam
  :ensure t)
;; org-roam
(setq org-roam-directory (expand-file-name "roam" org-directory))
(org-roam-db-autosync-mode)
(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))
(provide 'init-org)
;;; init-org.el Ends here
