;;; init.el -*- lexical-binding: t; -*-
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;;------------------------------------------------------------------------------
;; Straight Bootstrap
;;------------------------------------------------------------------------------

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

;;------------------------------------------------------------------------------
;; System
;;------------------------------------------------------------------------------

(use-package emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Enable recursive minibuffers.
  (setq enable-recursive-minibuffers t)
  ;; Better defaults.
  (setq frame-resize-pixelwise t
        frame-inhibit-implied-resize t)
  (setq use-short-answers t)
  (setq-default truncate-lines t)
  (setq-default indent-tabs-mode nil)
  ;; Scrolling
  (setq hscroll-margin 2
	hscroll-step 1
	;; Emacs spends too much effort recentering the screen if you scroll the
	;; cursor more than N lines past window edges (where N is the settings of
	;; `scroll-conservatively'). This is especially slow in larger files
	;; during large-scale scrolling commands. If kept over 100, the window is
	;; never automatically recentered.
	scroll-conservatively 101
	scroll-margin 0
	scroll-preserve-screen-position t
	;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
	;; for tall lines.
	auto-window-vscroll nil
	;; mouse
	mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
	mouse-wheel-scroll-amount-horizontal 2)
  (when (memq window-system '(mac ns x))
    (setq mac-redisplay-dont-reset-vscroll t
	  mac-mouse-wheel-smooth-scroll nil)
    (setq delete-by-moving-to-trash t
	  trash-directory "~/.Trash")))

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-arguments nil)
    (exec-path-from-shell-initialize)))

(use-package esup
  :straight t
  :config
  (setq esup-depth 0))

;;------------------------------------------------------------------------------
;; File History
;;------------------------------------------------------------------------------

(use-package files
  :config
  (setq make-backup-files nil)
  (setq auto-save-default nil))

(use-package undo-fu
  :straight t)

(use-package undo-fu-session
  :straight t
  :after undo-fu
  :config
  (undo-fu-session-global-mode))

(use-package autorevert
  :init
  (global-auto-revert-mode))

(use-package saveplace
  :init
  (save-place-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package recentf
  :init
  (setq recentf-max-menu-items 100
	recentf-max-saved-items 100)
  (recentf-mode 1)
  :config
  (add-to-list 'recentf-exclude "/private/var/folders/*"))

;;------------------------------------------------------------------------------
;; Evil
;;------------------------------------------------------------------------------

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-undo-system 'undo-fu)
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

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode t))

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

(use-package which-key
  :straight t
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

;;------------------------------------------------------------------------------
;; Completion
;;------------------------------------------------------------------------------

;; Vertico
(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :straight t
  :config
  (setq vertico-resize nil
	vertico-count 17)
  (vertico-mode))

(use-package consult
  :straight t
  :defer t
  :after vertico
  :config
  (setq consult-preview-key nil))

(use-package marginalia
  :straight t
  :after vertico
  :config
  (marginalia-mode))

(use-package embark
  :straight t
  :bind
  (("C-;" . embark-act))
  :config
  (setq embark-quit-after-action nil))

(use-package embark-consult
  :straight t
  :defer t)

;; Company
(use-package company-prescient
  :straight t
  :defer t)

(use-package company
  :straight t
  :init
  (setq company-minimum-prefix-length 2)
  :config
  (global-company-mode)
  (company-prescient-mode)
  (prescient-persist-mode))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

;; LSP
(use-package lsp-mode
  :straight t
  :after yasnippet
  :init
  (setq lsp-diagnostics-provider :flymake
        lsp-lens-enable nil
        lsp-signature-auto-activate nil
        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil))

;;------------------------------------------------------------------------------
;; Project
;;------------------------------------------------------------------------------

(use-package magit
  :straight t
  :defer t)

(use-package git-gutter-fringe
  :straight t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  (setq git-gutter-fr:side 'right-fringe)
  (global-git-gutter-mode t))

(use-package gitignore-templates
  :straight t
  :defer t)

(use-package dired
  :hook
  (after-init . dired-jump)
  (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-files "^\\(?:\\..*\\|.*~\\)$"
        dired-use-ls-dired nil
	dired-dwim-target t))

(use-package diredfl
  :straight t
  :config
  (diredfl-global-mode t))

(use-package compile
  :config
  (setq compilation-scroll-output t
	compile-command "make"))

;;------------------------------------------------------------------------------
;; Tools
;;------------------------------------------------------------------------------

(use-package tab-bar
  :init
  (setq tab-bar-show nil
	tab-bar-new-tab-choice "*scratch*"))

(use-package popper
  :straight t
  :config
  (setq popper-window-height 17
	popper-mode-line nil)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
	  "\\*Warnings\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
	  "^\\*eldoc.\*"
	  "^\\*Org Select\\*$"
          compilation-mode
          "^\\*vterm\\*$" vterm-mode
	  "^\\*utop\\*$"
	  "^\\*haskell\\*$"
	  "^\\*poly\\*$"
	  "^\\*Python\\*$"))
  (popper-mode 1))

(use-package hide-mode-line
  :straight t)

(use-package vterm
  :straight t
  :defer t
  :hook
  (vterm-mode . (lambda () (setq confirm-kill-processes nil)))
  :config
  (setq vterm-kill-buffer-on-exit t
	vterm-max-scrollback 5000))

(use-package vterm-toggle
  :straight t
  :defer t)

(use-package olivetti
  :straight t
  :config
  (setq-default fill-column 90))

(use-package presentation
  :straight
  (presentation
   :type git
   :host github
   :repo "zonuexe/emacs-presentation-mode"))

;;------------------------------------------------------------------------------
;; Appearance
;;------------------------------------------------------------------------------

(set-face-attribute 'default nil :font "JuliaMono-14")
(set-face-attribute 'fixed-pitch nil :font "JuliaMono-14")
(set-face-attribute 'variable-pitch nil :font "JuliaMono-14")

(use-package display-line-numbers
  :hook
  ((prog-mode text-mode) . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3))

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one t))

(use-package smartparens
  :straight t
  :hook
  (prog-mode . smartparens-mode)
  :init
  (require 'smartparens-config)
  (sp-pair "(" nil :unless '(sp-point-before-word-p))
  (sp-pair "[" nil :unless '(sp-point-before-word-p))
  (sp-pair "{" nil :unless '(sp-point-before-word-p))
  (sp-pair "\"" nil :unless '(sp-point-before-word-p
			      sp-point-after-word-p))
  (sp-local-pair 'latex-mode "$" nil :unless '(sp-point-before-word-p
					       sp-point-after-word-p))
  (sp-pair "'" nil :actions nil))

(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :straight t
  :defer t)

(use-package hl-todo
  :straight t
  :hook
  ((prog-mode text-mode) . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO" warning bold)
          ("FIXME" error bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("HACK" font-lock-constant-face bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("NOTE" success bold)
          ("BUG" error bold))))

(use-package doom-modeline
  :straight t
  :config
  (setq doom-modeline-icon nil
        doom-modeline-height 0
        doom-modeline-buffer-encoding nil)
  (doom-modeline-mode t))

;;------------------------------------------------------------------------------
;; Prose
;;------------------------------------------------------------------------------

(use-package adaptive-wrap
  :straight t
  :hook
  (visual-line-mode . adaptive-wrap-prefix-mode))

(use-package pdf-tools
  :straight t
  :defer t
  :hook
  ((pdf-view-mode
    . (lambda () (set (make-local-variable 'evil-normal-state-cursor) (list nil)))))
  :init
  (pdf-loader-install)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t
	pdf-view-use-imagemagick nil)
  (evil-define-key 'normal pdf-view-mode-map
    (kbd "zm") 'pdf-view-themed-minor-mode))

;; LaTeX
(use-package auctex
  :straight t
  :hook
  ((LaTeX-mode . lsp)
   (LaTeX-mode . visual-line-mode)
   (LaTeX-mode . flyspell-mode)
   (LaTeX-mode . smartparens-mode)
   (LaTeX-mode . rainbow-delimiters-mode))
  :init
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (setq-default TeX-PDF-mode t)
  (setq TeX-parse-self t
	TeX-auto-save t
	TeX-save-query nil
	TeX-command-extra-options "-shell-escape"
	TeX-auto-local ".auctex-auto"
	TeX-style-local ".auctex-style"
	TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
	TeX-source-correlate-mode t
	TeX-source-correlate-method 'synctex
	TeX-source-correlate-start-server t
	TeX-electric-sub-and-superscript t))

(use-package auctex-latexmk
  :straight t
  :after latex
  :hook
  ((LaTeX-mode . (lambda () (setq TeX-command-default "LatexMk"))))
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

(use-package org
  :straight t
  :defer t
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . flyspell-mode)
   (org-mode . (lambda ()
		 (modify-syntax-entry ?< "." org-mode-syntax-table)
		 (modify-syntax-entry ?> "." org-mode-syntax-table))))
  :config
  (setq org-startup-indented t
        org-hide-leading-stars t))

;; Markdown
(use-package markdown-mode
  :straight t
  :mode
  ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :hook
  ((markdown-mode . visual-line-mode)
   (markdown-mode . flyspell-mode))
  :init
  (setq markdown-enable-math t
	markdown-enable-wiki-links t
	markdown-italic-underscore t
	markdown-asymmetric-header t
	markdown-gfm-additional-languages '("sh")
	markdown-make-gfm-checkboxes-buttons t
	markdown-fontify-whole-heading-line t
	markdown-fontify-code-blocks-natively t
	markdown-command "multimarkdown"))

;;------------------------------------------------------------------------------
;; Code
;;------------------------------------------------------------------------------

;; Formatting
(use-package reformatter
  :straight t
  :config
  (reformatter-define sml-format
    :program "smlfmt"
    :args '("--stdio")))

;; Coq
(use-package company-coq
  :straight t
  :defer t)

(use-package proof-general
  :straight t
  :hook
  (coq-mode . company-coq-mode)
  :init
  (setq proof-splash-enable nil))

;; OCaml
(use-package tuareg
  :straight t
  :hook
  (tuareg-mode . lsp)
  (tuareg-mode . utop-minor-mode)
  (tuareg-mode . (lambda () (add-hook 'before-save-hook 'lsp-format-buffer)))
  (tuareg-mode . (lambda () (setq compile-command "dune build --profile release")))
  :config
  (setq tuareg-opam-insinuate t)
  (tuareg-opam-update-env (tuareg-opam-current-compiler)))

(use-package utop
  :straight t
  :defer t)

(use-package dune
  :straight t
  :hook
  (dune-mode . dune-format-on-save-mode))

(use-package dune-format
  :straight t
  :defer t)

;; Haskell
(use-package haskell-mode
  :straight t
  :hook
  (haskell-mode . lsp))

;; SML
(use-package sml-mode
  :straight t
  :defer t
  :hook
  (sml-mode . sml-format-on-save-mode)
  :config
  (setq sml-program-name "poly"))

;; ATS
(use-package ats2-mode
  :straight
  (ats2-mode
   :type git
   :host github
   :repo "qcfu-bu/ATS2-emacs")
  :defer t)

;; C/C++
(use-package cc
  :hook
  (c-mode . lsp))

;; Python
(use-package python
  :hook
  (python-mode . lsp))

(use-package yaml-mode
  :straight t
  :defer t)

;;------------------------------------------------------------------------------
;; Keybindings
;;------------------------------------------------------------------------------

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
    :prefix "SPC m")

  ;; default
  (spc-leader-def
    ;; general
    ""    nil
    "SPC" 'execute-extended-command
    "\\" 'toggle-input-method
    ;; help
    "hv" 'describe-variable
    "hf" 'describe-function
    "hF" 'describe-face
    "hs" 'describe-symbol
    "hb" 'embark-bindings
    "ht" 'consult-theme
    "hk" 'describe-key
    "hm" 'describe-mode
    "hi" 'describe-input-method
    "hd" 'consult-flymake
    "hh" 'eldoc
    ;; editor
    "el" 'goto-line
    "ec" 'goto-char
    "es" 'consult-line
    "er" 'anzu-query-replace
    "ey" 'consult-yank-pop
    ;; compilation
    "cc" 'compile
    "cr" 'recompile
    ;; files
    "ff" 'find-file
    "fr" 'consult-recent-file
    "fs" 'save-buffer
    ;; buffers
    "bb" 'consult-buffer
    "bi" 'consult-imenu
    "bp" 'switch-to-prev-buffer
    "bn" 'switch-to-next-buffer
    "bd" 'kill-this-buffer
    ;; windows
    "ws" 'evil-window-split
    "wv" 'evil-window-vsplit
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    "wp" 'evil-window-prev
    "wn" 'evil-window-next
    "wd" 'evil-window-delete
    ;; projects
    "pp" 'project-switch-project
    "pb" 'consult-project-buffer
    "pf" 'project-find-file
    "pd" 'project-find-dir
    "pc" 'project-compile
    ;; workspaces
    "TAB TAB" 'tab-bar-switch-to-tab
    "TAB o" 'tab-bar-new-tab
    "TAB p" 'tab-bar-switch-to-prev-tab
    "TAB n" 'tab-bar-switch-to-next-tab
    "TAB r" 'tab-bar-switch-to-recent-tab
    "TAB d" 'tab-bar-close-tab
    ;; toggles
    "tt" 'vterm-toggle
    "tl" 'display-line-numbers-mode
    "tp" 'popper-toggle-latest
    "tc" 'olivetti-mode
    "tz" 'presentation-mode
    ;; git
    "gg" 'magit
    "gr" 'consult-git-grep
    ;; quit
    "qq" 'save-buffers-kill-emacs)

  (spc-local-leader-def
    :keymaps 'emacs-lisp-mode-map
    "e" 'eval-buffer)

  (spc-local-leader-def
    :keymaps 'LaTeX-mode-map
    "c" 'TeX-command-master
    "e" 'TeX-command-run-all
    "v" 'TeX-view)

  (spc-local-leader-def
    :keymaps 'org-mode-map
    "i" 'org-insert-structure-template
    "l" 'org-insert-link
    "e" 'org-export-dispatch)

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
    "e" 'utop
    "b" 'utop-eval-buffer)

  (spc-local-leader-def
    :keymaps 'sml-mode-map
    "e" 'run-sml
    "b" 'sml-prog-proc-send-buffer)

  (spc-local-leader-def
    :keymaps 'python-mode-map
    "e" 'run-python
    "b" 'python-shell-send-buffer))
