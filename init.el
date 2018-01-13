(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (neotree emmet-mode ivy org-bullets use-package web-mode counsel deft doom-themes ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;
;; add melpa packages
;;

;; melpa packages
(require 'package)

;; Add melpa package source when using package list
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
(use-package doom-themes
  :ensure t 
  :config
  (load-theme 'doom-one t))

;; enable line numbers
(global-linum-mode t)

;;
;; set default behaviour
;;

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

;; open config on C-c e
(global-set-key (kbd "C-c e") '(lambda ()
			   (interactive)
			   (find-file "~/.emacs.d/init.el")))

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
;; set custom todo states, using gtd states
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "WAITING" "|" "DONE" "CANCELED")))

;; all child tasks must be 'done' for parent to be marked 'done'
(setq org-enforce-todo-dependencies t)

;; set source for agenda
(setq org-agenda-files '("~/Dokumente/org/"))

;; Setting Colours (faces) for todo states to give clearer view of work 
(setq org-todo-keyword-faces
  '(("TODO" . org-warning)
   ("DOING" . "yellow")
   ("WAITING" . "red")
   ("DONE" . "green")
   ("CANCELED" .  "grey"))) 

;; use org-bullets-mode for utf8 symbols as org bullets
;;(require 'org-bullets)
;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package org-bullets
  :ensure t 
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
		deft-use-filename-as-title t))

;; ivy, counsel, swiper
(use-package ivy
  :ensure t)

;; counsel should load ivy as dep
(use-package counsel
  :ensure t
  :bind (
	 ("C-x C-f" . counsel-find-file)
	 ("C-s" . swiper)
	 ("M-y" . counsel-yank-pop)
	 ("M-x" . counsel-M-x))
  :config (ivy-mode 1))

;; config for magit and projectile
;;(setq magit-completing-read-function 'ivy-completing-read)
;;(setq projectile-completion-system 'ivy)

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