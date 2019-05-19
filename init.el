;;
;; Emacs Customizations
;;

;;
;; PACKAGE MANAGER - package install and MELPA
;;
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("local" . "~/.emacs.d/local")))

(package-initialize)

(require 'use-package)

;;
;; EMACS CONFIGURATIONS
;;
(setq make-backup-files nil)
(setq column-number-mode t)
(setq dired-listing-switches "-alh")
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)

(setq js-indent-level 2)
(setq css-indent-offset 2)

;;
;; MODES CONFIG
;;
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . ng2-mode))


;;
;; HOOKS CONFIG
;;
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;
;; CUSTOM FUNCTIONS
;;
(defun insert-current-date () (interactive)
    (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

;;
;; GENERAL KEYBINDINGS
;;
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-x g") 'magit-status)

;;
;; ACE WINDOW - better window movement
;;
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-background nil)
(setq aw-dispatch-always t)

;;
;; AUTOCOMPLETE
;;
(require 'auto-complete)
(ac-config-default)

;;
;; WINDMOVE - better window movement
;;
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;
;; WRAP-REGION - surround regions with ", ', (, etc.
;;
(require 'wrap-region)
(add-hook 'text-mode-hook (lambda () (wrap-region-mode t)))
(add-hook 'prog-mode-hook (lambda () (wrap-region-mode t)))

;;
;; EXEC-PATH-FROM-SHELL - get environment variables from the shell
;;
(exec-path-from-shell-initialize)

;;
;; DRACULA THEME
;;
(load-theme 'dracula t)

;;
;; NEOTREE - directory navigation
;;
(require 'neotree)
(global-set-key (kbd "C-x C-j") 'neotree-toggle)
(setq neo-smart-open t)
(setq neo-show-hidden-files t)
(setq neo-window-fixed-size nil)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(add-hook 'after-init-hook #'neotree-toggle)

;;
;; ESS - gotta have r highlighting
;;
(setq ess-default-style 'RStudio)
(use-package ess
  :ensure t
  :init (require 'ess-site))
(ess-toggle-underscore nil)
(setq ess-roxy-fontify-examples nil)
(setq ess-roxy-str "#'")

;;
;; LOREM IPSUM -
;;
(require 'lorem-ipsum)
(lorem-ipsum-use-default-bindings)

;;
;; MARKDOWN MODE
;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))


;;
;; ADDED AUTOMATICALY
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (edit-indirect ng2-mode doom-themes js2-mode typescript ## lorem-ipsum typescript-mode symon ace-window wrap-region web-mode use-package neotree markdown-mode magit helm go-autocomplete font-lock+ exec-path-from-shell ess dracula-theme color-theme-modern all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
