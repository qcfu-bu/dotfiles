(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;; straight bootstrap
(setq straight-check-for-modifications '(check-on-save)
      straight-cache-autoloads t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

;; system
(server-start)
(setq make-backup-files nil
      dired-use-ls-dired nil
      frame-resize-pixelwise t
      delete-by-moving-to-trash t
      trash-directory "~/.Trash")

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package esup
  :straight t
  :config
  (setq esup-depth 0))

(use-package savehist
  :init
  (savehist-mode))

(use-package recentf
  :init
  (setq recentf-max-menu-items 25
	recentf-max-saved-items 25)
  (recentf-mode 1)
  :config
  (add-to-list 'recentf-exclude "/private/var/folders/*"))

;; evil
(use-package evil
  :straight t
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-escape
  :straight t
  :after evil
  :config
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
	evil-escape-excluded-major-modes '(vterm)
	evil-escape-key-sequence "jk"
	evil-escape-delay 0.2)
  (evil-escape-mode))

(use-package evil-commentary
  :straight t
  :config
  (evil-commentary-mode))

(use-package evil-anzu
  :straight t
  :after evil
  :config
  (global-anzu-mode 1))

(use-package evil-easymotion
  :straight t
  :after evil
  :config
  (evilem-default-keybindings "gs"))

(use-package general
  :straight t
  :after evil
  :config
  (general-override-mode 1)
  (general-create-definer spc-leader-def
    :states '(normal)
    :keymaps 'override
    :prefix "SPC")
  (general-create-definer spc-local-leader-def
    :states '(normal)
    :keymaps 'override
    :prefix "SPC m"))

(use-package which-key
  :straight t
  :config
  (which-key-mode))

;; completion
(use-package ivy-prescient
  :straight t)

(use-package counsel
  :straight t
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode)
  (counsel-mode))

(use-package company-prescient
  :straight t
  :defer t)

(use-package company
  :straight t
  :init
  (global-company-mode)
  (company-prescient-mode)
  (prescient-persist-mode))

(use-package lsp-mode
  :straight t
  :defer t
  :config
  (setq lsp-headerline-breadcrumb-enable nil
	lsp-lens-enable nil))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

;; git
(use-package magit
  :straight t
  :defer t)

(use-package git-gutter
  :straight t
  :config
  (global-git-gutter-mode 1))

(use-package gitignore-templates
  :straight t
  :defer t)

(use-package projectile
  :straight t
  :defer t
  :config
  (setq projectile-ignored-projects '("~/")
	projectile-project-root-files '()
	projectile-project-root-files-bottom-up '(".projectile" ".git")
	projectile-project-root-files-top-down-recurring '("Makefile")))

(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode))

;; tools
(use-package vterm
  :straight t
  :defer t
  :config
  (setq vterm-kill-buffer-on-exit t))

(use-package vterm-toggle
  :straight t
  :defer t)

;; appearance
(use-package modus-themes
  :straight t
  :defer t
  :init (load-theme 'modus-operandi t))

(use-package smartparens
  :straight t
  :hook ((prog-mode text-mode) . smartparens-global-mode))

(use-package rainbow-delimiters
  :straight t
  :hook ((prog-mode text-mode) . rainbow-delimiters-mode))

(use-package minions
  :straight t
  :config
  (minions-mode 1))

(set-face-attribute 'default nil :font "Fira Code-14")
(set-face-attribute 'variable-pitch nil :font "Fira Sans-16")

;; prose
(use-package adaptive-wrap
  :straight t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

(use-package auctex
  :straight t
  :hook ((LaTeX-mode . lsp)
	 (LaTeX-mode . visual-line-mode)
	 (LaTeX-mode . flyspell-mode))
  :init
  (setq TeX-command-extra-options "-shell-escape"
	TeX-auto-local ".auctex-auto"
	TeX-style-local ".auctex-style"
	TeX-source-correlate-mode t
	TeX-source-correlate-method 'synctex
	TeX-source-correlate-start-server nil
	TeX-electric-sub-and-superscript t))

(use-package org-superstar
  :straight t
  :defer t)

(use-package org
  :straight t
  :defer t
  :hook ((org-mode . org-superstar-mode)
	 (org-mode . visual-line-mode)
	 (org-mode . flyspell-mode))
  :config
  (setq org-startup-indented t))

;; code
(use-package company-coq
  :straight t
  :defer t)

(use-package proof-general
  :straight t
  :hook (coq-mode . company-coq-mode)
  :init
  (setq proof-splash-enable nil))

(use-package tuareg
  :straight t
  :hook ((tuareg-mode . lsp)
	 (tuareg-mode . (lambda () (add-hook 'before-save-hook 'lsp-format-buffer nil t)))))

(use-package utop
  :straight t
  :defer t
  :hook (tuareg-mode . utop-minor-mode)
  :config
  (setq utop-command "opam config exec -- utop -emacs"))

(use-package dune
  :straight t)

(use-package dune-format
  :straight t
  :hook (dune-mode . dune-format-on-save-mode))

;; keybinding
(defun find-user-init-file ()
  (interactive)
  (find-file user-init-file))

(spc-leader-def
  ;; general
  ""    nil
  "SPC" 'counsel-M-x
  "\\" 'toggle-input-method
  ;; files
  "ff" 'counsel-find-file
  "fr" 'counsel-recentf
  "fp" 'find-user-init-file
  "fs" 'save-buffer
  ;; buffers
  "bb" 'counsel-switch-buffer
  "bp" 'previous-buffer
  "bn" 'next-buffer
  "bd" 'kill-this-buffer
  ;; windows
  "ws" 'evil-window-split
  "wv" 'evil-window-vsplit
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wl" 'evil-window-right
  "wd" 'evil-window-delete
  ;; projects
  "pp" 'counsel-projectile-switch-project
  "pf" 'counsel-projectile-find-file
  "pd" 'counsel-projectile-find-dir
  "pg" 'counsel-projectile-grep
  ;; themes
  "tt" 'counsel-load-theme
  ;; terminal
  "'" 'vterm-toggle
  "\"" 'vterm
  ;; git
  "gg" 'magit
  ;; quit
  "qq" 'save-buffers-kill-emacs)

(spc-local-leader-def
  :keymaps 'emacs-lisp-mode-map
  "e" 'eval-buffer)

(spc-local-leader-def
  :keymaps 'LaTeX-mode-map
  "c" 'TeX-command-master
  "v" 'TeX-view)

(spc-local-leader-def
  :keymaps 'coq-mode-map
  "." 'proof-goto-point
  "f" 'proof-assert-next-command-interactive
  "b" 'proof-undo-last-successful-command
  "pp" 'proof-process-buffer
  "pr" 'proof-retract-buffer
  "pk" 'proof-shell-exit)

(spc-local-leader-def
  :keymaps 'tuareg-mode-map
  "e" 'utop-eval-buffer
  "u" 'utop)
