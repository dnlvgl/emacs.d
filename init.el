;;
;; barebone init.el, load config in literate orgmode file 
;;

;; load packages
(package-initialize t)
(setq package-enable-at-startup nil)

;; load emacs config in orgmode file
(require 'org)
(org-babel-load-file "~/.emacs.d/emacs.org")
;;(org-babel-load-file "~/.emacs.d/secrets.org")

;; keep emacs custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
