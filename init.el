;;
;; Emacs Customizations
;;

;;
;; PACKAGE MANAGER - package install and MELPA
;;
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; (un)comment these lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives
  ;;              (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives
                 (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(require 'use-package)

;;
;; EMACS CONFIGURATIONS
;;
(setq linum-format "%4d ") ;; \u2503 ")
(set-face-attribute 'line-number nil :background "gray96" :foreground "gray42")
(setq make-backup-files nil)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq column-number-mode t)
(setq dired-listing-switches "-alh")
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq frame-resize-pixelwise t)

(global-auto-revert-mode t)
(global-linum-mode t)
(menu-bar-mode -1)

(load-file "~/.emacs.d/split-window-sensibly.el")
(setq split-window-preferred-function 'split-window-really-sensibly)

;;
;; MODES CONFIG
;;
(add-to-list 'auto-mode-alist '("\\.ts\\'" . ng2-mode))
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;
;; HOOKS CONFIG
;;
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;
;; CUSTOM FUNCTIONS
;;
(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun shorter-path (path)
  (let* ((components (seq-filter
                      (lambda (a) (not (equal a "")))
                      (split-string (expand-file-name path) "/"))))
    (concat
     (seq-reduce
      (lambda (a b) (concat a "/" b))
      (seq-map (lambda (s) (substring s 0 1)) (butlast components))
      "")
     "/"
     (car (last components)))))

(defun custom-eshell-prompt ()
  (concat (shorter-path (eshell/pwd))
          (if (= (user-uid) 0) " # " " $ ")))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;
;; GENERAL KEYBINDINGS
;;
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "M-Q") 'unfill-paragraph)

;;
;; ESHELL PROMPT
;;
(setq eshell-prompt-function 'custom-eshell-prompt)

;;
;; FONTS / ICONS
;;
(use-package all-the-icons)

;;
;; JS2 MODE
;;
(require 'js2-mode)
(require 'xref-js2)

(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(define-key js-mode-map (kbd "M-.") nil)
(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(setq js2-strict-missing-semi-warning nil)

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
;; THEME
;;
(use-package dracula-theme
  :ensure t
  :defer)
(use-package leuven-theme
  :ensure t
  :defer)

(use-package circadian
  :ensure t
  :config
  (setq calendar-latitude 42.440500)
  (setq calendar-longitude -76.495700)
  (setq circadian-themes '((:sunset . dracula)
                           (:sunrise . leuven)))
  (circadian-setup))

;;
;; NEOTREE - directory navigation
;;
(require 'neotree)

(global-set-key (kbd "C-x C-j") 'neotree-toggle)

(setq neo-smart-open t)
(setq neo-show-hidden-files t)
(setq neo-window-width 20)
(setq neo-window-fixed-size t)
(setq neo-hide-cursor t)
(setq neo-autorefresh t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(setq neo-confirm-create-file 'off-p)
(setq neo-confirm-create-directory 'off-p)
(setq neo-confirm-delete-file 'y-or-n-p)
(setq neo-confirm-delete-directory-recursively 'off-p)
(setq neo-confirm-kill-buffers-for-files-in-directory 'off-p)

(setq neo-vc-integration '(face))
(setq neo-filepath-sort-function 'neo-sort-hidden-last)

(defun neotree-startup ()
  (interactive)
  (neotree-show)
  (call-interactively 'other-window))

;; (add-hook 'after-init-hook #'neotree-startup)

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
;; WHITESPACE
;;
;; (require 'whitespace)
;; (setq whitespace-style '(face empty tabs lines-tail trailing))
;; (global-whitespace-mode t)

;;
;; AGGRESSIVE INDENT
;;
;; (global-aggressive-indent-mode t)

;;
;; ADDED AUTOMATICALY
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-safe-themes
   '("a339f231e63aab2a17740e5b3965469e8c0b85eccdfb1f9dbd58a30bdad8562b" "777a3a89c0b7436e37f6fa8f350cbbff80bcc1255f0c16ab7c1e82041b06fccd" "672bb062b9c92e62d7c370897b131729c3f7fd8e8de71fc00d70c5081c80048c" "6731049cee8f7cbd542d7b3e1c551f3fab716a92119bd7c77f0bd1ef20849fb8" default))
 '(hl-sexp-background-color "#efebe9")
 '(inhibit-startup-screen t)
 '(ns-use-srgb-colorspace nil)
 '(package-selected-packages
   '(beacon leuven-theme twilight-bright-theme circadian poly-R poly-markdown polymode nodejs-repl ag xref-js2 edit-indirect ng2-mode doom-themes js2-mode typescript ## lorem-ipsum typescript-mode symon ace-window wrap-region web-mode use-package neotree markdown-mode magit helm go-autocomplete font-lock+ exec-path-from-shell ess dracula-theme color-theme-modern all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "nil" :family "Fira Code")))))
