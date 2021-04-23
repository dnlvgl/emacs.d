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
 '(custom-safe-themes
   '("de1f10725856538a8c373b3a314d41b450b8eba21d653c4a4498d52bb801ecd2" default))
 '(lsp-ui-sideline-show-code-actions nil)
 '(package-selected-packages
   '(brutal-theme which-key web-mode use-package switch-window spacemacs-theme smex polymode org-bullets neotree multiple-cursors magit lsp-ui js2-mode god-mode git-gutter-fringe+ flycheck emojify emmet-mode editorconfig doom-modeline dashboard counsel-projectile company ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
