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
;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-sideline-show-code-actions nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
