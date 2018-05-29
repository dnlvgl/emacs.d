;;
;; add melpa packages
;;

;; melpa packages
(require 'package)

;; Add melpa package sourhttps://www.emacswiki.org/emacs/BookMarksce when using package list
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; Load emacs packages and activate them
;; This must come before configurations of installed packages.
;; Don't delete this line.
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;
;; basic appearance
;;

;; disable startup screen
(setq inhibit-startup-screen t)

;; Start maximised (cross-platf)
;;(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; disable toolbar
(tool-bar-mode -1)

;; disable scrollbar
(toggle-scroll-bar -1)

;; set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; color theme
(use-package spacemacs-theme
  :ensure t
  :defer t
  :init
  (load-theme 'spacemacs-dark t))

;; enable line numbers
(global-linum-mode t)

;;
;; set default behaviour
;;

;; declare defaults, overwrite if custom.el exists
(setq orgfile-path '("~/Dokumente/org/"))

;; load custom file, ignore if not existing
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; load local.el (additional config depending on machine) if it exists
(let ((local-settings "~/.emacs.d/local.el"))
 (when (file-exists-p local-settings)
   (load-file local-settings))
)

;; buffer menu
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; set all backup files to certain folder
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; enable selection with mouse
(defun mouse-start-rectangle (start-event)
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point start-event)
  (rectangle-mark-mode +1)
  (let ((drag-event))
    (track-mouse
      (while (progn
               (setq drag-event (read-event))
               (mouse-movement-p drag-event))
        (mouse-set-point drag-event)))))

(global-set-key (kbd "S-<down-mouse-1>") #'mouse-start-rectangle)

;; refile into datetree
(defun org-refile-to-datetree (&optional file)
  "Refile a subtree to a datetree corresponding to it's timestamp.

The current time is used if the entry has no timestamp. If FILE
is nil, refile in the current file."
  (interactive "f")
    (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP_IA" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date))
         )
    (save-excursion
      (with-current-buffer (current-buffer)
        (org-cut-subtree)
        (if file (find-file file))
        (org-datetree-find-date-create date)
        (org-narrow-to-subtree)
        (show-subtree)
        (org-end-of-subtree t)
        (newline)
        (goto-char (point-max))
        (org-paste-subtree 4)
        (widen)
        ))
    )
  )

(global-set-key (kbd "C-c C-d") #'org-refile-to-datetree)


;; export org file headings into org files
(defun org-file-from-subtree (&optional name)
  "Copy the subtree currently being edited and create a new file
from it. Ask for directory.

If called with the universal argument, prompt for new filename,
otherwise use the subtree title."
  (interactive "P")
  (org-back-to-heading)
  (let ((filename (cond
                   (current-prefix-arg
                    (expand-file-name
                     (read-file-name "New file name: ")))
                   (t
                    (concat
                     (expand-file-name
                      (org-element-property :title
                                            (org-element-at-point))
                      (read-directory-name "Directory:"))
                     ".org")))))
    (org-copy-subtree)
    (find-file-noselect filename)
    (with-temp-file filename
      (org-mode)
      (yank)
      (beginning-of-buffer)
      (kill-whole-line)
      ;;(org-promote-subtree)
      )))

;; open config on C-c e
(global-set-key (kbd "C-c e") '(lambda ()
			   (interactive)
			   (find-file "~/.emacs.d/init.el")))

;; replace deft with dired?
(global-set-key (kbd "<f2>")
  (lambda ()
    (interactive)
    (dired "~/Dokumente/org")))

;; autoclose brackets
(electric-pair-mode 1)

;; highlight brackets
(setq show-paren-delay 0)
(show-paren-mode 1)
;;
;; config packages
;;

;; set up ido mode
(use-package ido
  :ensure t 
  :config
  (progn
    (setq ido-enable-flex-matching t)
    (setq ido-everywhere t)
    (ido-mode 1)))

;; set up ace-jump-mode
(use-package ace-jump-mode
  :ensure t 
  :bind ("C-." . ace-jump-mode))

;; orgmode
;; set custom todo states
(setq org-todo-keywords 
  '((sequence "TODO" "DOING" "BLOCKED" "|" "DONE")))

;; all child tasks must be 'done' for parent to be marked 'done'
(setq org-enforce-todo-dependencies t)

;; don't show done items in agenda
(setq org-agenda-skip-scheduled-if-done t)

;; set source for agenda
(setq org-agenda-files orgfile-path)

;; degfine capture
(define-key global-map "\C-cc" 'org-capture)

;; capture templates
(setq org-capture-templates
      '(
	;;("t"
	;; "Todo" entry (file+headline "~/Dokumente/org/todo.org" "Tasks")
	;; "* TODO %?\n %i\n")
	("l"
	 "Log"
	 entry (file+datetree "~/Dokumente/org/log.org")
	 "** %u %^{Title}\n %?")
	("n"
	 "Notes" entry (file+datetree  "~/Dokumente/org/taskdiary.org") 
	 "* %^{Description} %?%^g Added: %U")
	("t"
	 "Task Diary" entry (file+datetree "~/Dokumente/org/taskdiary.org") 
	 "* %^{Description} %^g Added: %U\n %?")))
 
;; use org-bullets-mode for utf8 symbols as org bullets
(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
	'("●" "◉" "◍" "○" "✸"))
  :hook (org-mode . org-bullets-mode))

(use-package deft
  :ensure t 
  :bind ("<f8>" . deft)
  :commands (deft)
  ;; only search for org files in org directory
  ;; use file name as title
  :config (setq deft-directory "~/Dokumente/org"
                deft-extensions '("org")
		deft-default-extension "org"
		deft-use-filename-as-title t
		deft-current-sort-method 'title
		deft-file-naming-rules '((noslash . "_")(nospace . "_w")(case-fn . downcase))))

;; emacs-dashboard
(use-package dashboard
    :ensure t
    :diminish dashboard-mode
    :config
    (setq dashboard-banner-logo-title "your custom text")
    (setq dashboard-startup-banner "~/.emacs.d/dasboard-logo.png")
    (setq dashboard-items '((bookmarks . 10)
			    (agenda . 5)			    
			    (projects . 5)
			    (recents . 0)
			    (registers . 5)))
    (dashboard-setup-startup-hook))

;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode 1)
  ;;change neotree root on project change
  (setq projectile-switch-project-action 'neotree-projectile-action))

;; magit
(use-package magit
  :ensure t)

;; ivy, counsel, swiper
(use-package ivy
  :ensure t
  :config (ivy-mode 1))

;; counsel should load ivy as dep
(use-package counsel
  :ensure t
  :bind (
	 ("C-x C-f" . counsel-find-file)
	 ("C-s" . swiper)
	 ("M-y" . counsel-yank-pop)
	 ("M-x" . counsel-M-x))
  :config 
  (setq projectile-completion-system 'ivy)
  (setq magit-completing-read-function 'ivy-completing-read))
;; add https://github.com/ericdanan/counsel-projectile ?

;; use smex for M-x enhancement
(use-package smex
  :ensure t)

;; js2-mode
(use-package js2-mode
  :ensure t
  :config (progn
	    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
	    ;; replaced with rjsx for better highlighting
	    ;;(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
	    ))

;; react mode
(use-package rjsx-mode
  :ensure t)

(defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
  "Workaround sgml-mode and follow airbnb component style."
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))

;; web-mode
(use-package web-mode
  :ensure t
  :config (progn
	    (add-to-list 'auto-mode-alist '("\\.njs\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
	    (setq web-mode-markup-indent-offset 2)
	    (setq web-mode-code-indent-offset 2)
	    (setq web-mode-css-indent-offset 2)
	    (setq web-mode-script-padding 2)
	    ;; highlight columns
	    (setq web-mode-enable-current-column-highlight t)
	    (setq web-mode-enable-current-element-highlight t)))


;; emmet
(use-package emmet-mode
  :ensure t
  :hook (web-mode . emmet-mode)
  :init
  ;; toggle autocompletion on inline css
  (add-hook 'web-mode-before-auto-complete-hooks
    '(lambda ()
     (let ((web-mode-cur-language
  	    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "css")
    	   (setq emmet-use-css-transform t)
	   (setq emmet-use-css-transform nil))))))

;; iconset
;; run 'M-x all-the-icons-install-fonts' to install all fonts
(use-package all-the-icons
  :ensure t)

;; neotree
(use-package neotree
  :ensure t
  :bind ("<f1>" . neotree-toggle)
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; company mode autocomplete
(use-package company
  :ensure t
  ;; use company mode everywhere
  :hook (after-init . global-company-mode))

;; company tern
;; install tern 'npm install -g tern tern-lint'
;; add global '.tern-config' file
(use-package company-tern
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-tern))

(use-package tern
  :ensure t
  :defer t
  :diminish tern-mode
  :hook (js-mode . tern-mode)
  :config (setq tern-command (append tern-command '("--no-port-file"))))

;; show changes from git
(use-package git-gutter-fringe+
  :ensure t
  :config
  (global-git-gutter+-mode)
  (setq git-gutter-fr+-side 'left-fringe)
  (set-face-foreground 'git-gutter-fr+-modified "#4f97d7")
  (set-face-foreground 'git-gutter-fr+-added    "#293235")
  (set-face-foreground 'git-gutter-fr+-deleted  "#f2241f"))




