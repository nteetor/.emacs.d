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

;;
;; EMACS CONFIGURATIONS
;;
(setq make-backup-files nil)
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)

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

;;
;; WINDMOVE - better window movement
;; 
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

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
(use-package ess
             :ensure t
             :init (require 'ess-site))

