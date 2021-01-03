;;
;; barebone init.el, load config in literate orgmode file
;;

;; load emacs config in orgmode file
(require 'org)
(org-babel-load-file "~/.emacs.d/emacs.org")

;; alt/new way of loading
;;(org-babel-load-file
;; (expand-file-name
;;  "emacs.org"
;;  user-emacs-directory))

;; keep emacs custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

