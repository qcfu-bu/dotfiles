;;; init.el --- emacs dotfile -*- lexical-binding: t -*-

;;; bootstrap
;;;; straight
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

;;;; gc
(use-package gcmh
  :straight t
  :config (gcmh-mode 1))

;;;; path
(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-arguments nil)
    (exec-path-from-shell-initialize)))

;;; emacs
;;;; info
(setq user-full-name "Qiancheng Fu"
      user-mail-address "qcfu@bu.edu")

;;;; defaults
(setq use-short-answers t)
(setq frame-resize-pixelwise t)
(setq delete-by-moving-to-trash t)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq native-comp-async-report-warnings-errors nil)
(setq-default line-spacing 0.2)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(when (eq system-type 'mac)
  (setq mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil)
  (setq trash-directory "~/.Trash")
  (menu-bar-mode 1))

;;;; server
(use-package server
  :config
  (unless (server-running-p) (server-start)))

;;;; minibuffer
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq enable-recursive-minibuffers t)

;;;; scrolling
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

;;;; files
(use-package files
  :config
  (setq make-backup-files nil)
  (setq auto-save-default nil))

;;;; autorevert
(use-package autorevert
  :init (global-auto-revert-mode))

;;;; history
(use-package saveplace
  :init (save-place-mode))

(use-package savehist
  :init (savehist-mode))

(use-package recentf
  :init
  (setq recentf-max-menu-items 40
        recentf-max-saved-items 40)
  (recentf-mode 1)
  :config
  (add-to-list 'recentf-exclude "/private/var/folders/*"))

;;; completion
;;;; orderless
(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;;; vertico
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
  (setq consult-preview-key nil)
  (add-to-list 'consult-buffer-filter "^\\*"))

(use-package marginalia
  :straight t
  :after vertico
  :config
  (marginalia-mode))

(use-package embark
  :straight t
  :bind (("C-;" . embark-act))
  :config
  (setq embark-quit-after-action nil))

(use-package embark-consult
  :straight t
  :defer t)

;;;; corfu
(use-package corfu
  :straight t
  :custom-face
  (corfu-border ((t (:background "black"))))
  (corfu-current ((t (:background nil :inherit region))))
  (corfu-default ((t (:background nil :inherit default))))
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-count 16
        corfu-max-width 120
        corfu-on-exact-match nil)
  (global-corfu-mode t)
  (corfu-history-mode t))

(use-package cape
  :straight t
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

;;; editor
;;;; evil
(use-package evil
  :straight t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-undo-system 'undo-fu
        evil-respect-visual-line-mode t)
  :config
  (setq evil-emacs-state-tag "E"
        evil-insert-state-tag "I"
        evil-motion-state-tag "M"
        evil-normal-state-tag "N"
        evil-operator-state-tag "O"
        evil-replace-state-tag "R"
        evil-visual-char-tag "V"
        evil-visual-block-tag "Vb"
        evil-visual-line-tag "Vl"
        evil-visual-screen-line-tag "Vs")
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
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package evil-commentary
  :straight t
  :after evil
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

;;;; word-wrap
(use-package adaptive-wrap
  :straight t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

;;;; undo
(use-package undo-fu
  :straight t)

(use-package undo-fu-session
  :straight t
  :after undo-fu
  :config
  (undo-fu-session-global-mode))

;;;; snippets
(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

;;;; format
(use-package reformatter
  :straight t
  :commands reformatter-define)

;;; ui
;;;; fonts
(set-face-attribute 'default        nil :font "JetBrains Mono-14")
(set-face-attribute 'fixed-pitch    nil :font "JetBrains Mono-14")
(set-face-attribute 'variable-pitch nil :font "JetBrains Mono-14")

;;;; icons
(use-package nerd-icons
  :straight t)

(use-package nerd-icons-completion
  :straight t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :straight t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :straight t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;; themes
(use-package doom-themes
  :straight t
  :config (load-theme 'doom-one t))

;;;; modeline
(use-package doom-modeline
  :straight t
  :config
  (setq doom-modeline-bar-width nil
        doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-mode t))

;;;; popup
(use-package popper
  :straight t
  :config
  (setq popper-mode-line ""
        popper-window-height 20)
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
  (popper-mode 1)
  (popper-echo-mode 1))

;;;; tab-bar
(use-package tab-bar
  :init
  (setq tab-bar-show nil
        tab-bar-new-tab-choice "*scratch*"))

;;;; linum
(use-package display-line-numbers
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 4))

;;;; vi-tilde
(use-package vi-tilde-fringe
  :straight t
  :hook
  ((prog-mode text-mode conf-mode) . vi-tilde-fringe-mode))

;;;; delimiter
(use-package electric
  :config
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (electric-pair-mode t))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; highlight
(use-package rainbow-mode
  :straight t
  :defer t)

(use-package hl-todo
  :straight t
  :hook ((prog-mode text-mode) . hl-todo-mode)
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

;;;; presentation
(use-package olivetti
  :straight t
  :config
  (setq-default fill-column 90))

(use-package outli
  :straight
  (outli
   :type git
   :host github
   :repo "jdtsmith/outli")
  :hook (emacs-lisp-mode . outli-mode)
  :config
  (setq outli-default-nobar t))

(use-package presentation
  :straight
  (presentation
   :type git
   :host github
   :repo "zonuexe/emacs-presentation-mode"))

;;; tools
;;;; magit
(use-package magit
  :straight t
  :defer t
  :config
  (setq magit-bury-buffer-function 'magit-restore-window-configuration
        magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package git-gutter-fringe
  :straight t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  (global-git-gutter-mode t))

(use-package gitignore-templates
  :straight t
  :defer t)

;;;; eldoc
(use-package eldoc
  :straight t
  :config
  (setq eldoc-display-functions '(eldoc-display-in-buffer)))

(use-package eldoc-box
  :straight t
  :custom-face
  (eldoc-box-border ((t (:background "black")))))

;;;; flymake
(use-package flymake
  :straight t
  :defer t
  :init
  (setq flymake-fringe-indicator-position nil))

;;;; eglot
(use-package eglot
  :straight t
  :commands (eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((LaTeX-mode) . ("texlab"))))

;;;; dired
(use-package dired
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-files "^\\(?:\\..*\\|.*~\\)$\\|\\.vo\\(?:k\\|s\\)$"
        dired-listing-switches "-alh"
        dired-use-ls-dired nil
        dired-dwim-target t))

(use-package dired-subtree
  :straight t
  :after dired)

(use-package diredfl
  :straight t
  :config
  (diredfl-global-mode t))

;;;; compile
(use-package compile
  :commands (compile recompile)
  :config
  (setq compilation-scroll-output t
        compile-command "make"))

;;;; pdf
(use-package pdf-tools
  :straight t
  :defer t
  :hook
  ((pdf-view-mode
    . (lambda () (set (make-local-variable 'evil-normal-state-cursor) (list nil)))))
  :init (pdf-loader-install)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (evil-define-key 'normal pdf-view-mode-map
    (kbd "zm") 'pdf-view-themed-minor-mode))

;;;; restart
(use-package restart-emacs
  :straight t
  :defer t)

;;;; profiling
(use-package esup
  :straight t
  :config
  (setq esup-depth 0))

;;; term
;;;; vterm
(use-package vterm
  :straight t
  :defer t
  :hook (vterm-mode . (lambda () (setq confirm-kill-processes nil)))
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-max-scrollback 5000))

(use-package vterm-toggle
  :straight t
  :bind ("C-`" . vterm-toggle))

;;; lang
;;;; markdown
(use-package markdown-mode
  :straight t
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
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

;;;; org
(use-package org
  :defer t
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . flyspell-mode)
   (org-mode . (lambda ()
                 (modify-syntax-entry ?< "." org-mode-syntax-table)
                 (modify-syntax-entry ?> "." org-mode-syntax-table))))
  :config
  (setq org-startup-indented t
        org-hide-leading-stars t
        org-src-window-setup 'current-window))

;;;; latex
(use-package auctex
  :straight t
  :hook
  ((LaTeX-mode . eglot-ensure)
   (LaTeX-mode . visual-line-mode)
   (LaTeX-mode . flyspell-mode)
   (LaTeX-mode . rainbow-delimiters-mode))
  :init
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (setq-default TeX-PDF-mode t
                TeX-master nil)
  (setq TeX-parse-self t
        TeX-auto-save t
        TeX-save-query nil
        TeX-command-extra-options "-shell-escape"
        TeX-auto-local ".auctex-auto"
        TeX-style-local ".auctex-style"
        TeX-view-program-list '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -g -b %n %o %b"))
        TeX-view-program-selection '((output-pdf "Skim"))
        ;; TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-electric-math '("$" . "$")
        TeX-electric-sub-and-superscript t)
  (setq font-latex-fontify-script nil))

(use-package auctex-latexmk
  :straight t
  :after latex
  :hook ((LaTeX-mode . (lambda () (setq TeX-command-default "LatexMk"))))
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

;;;; ocaml
(use-package tuareg
  :straight t
  :hook
  (tuareg-mode . eglot-ensure)
  (tuareg-mode . utop-minor-mode)
  (tuareg-mode . ocp-format-on-save-mode)
  (tuareg-mode . (lambda () (setq-local compile-command "dune build --profile release")))
  (tuareg-menhir-mode . (lambda () (setq-local compile-command "dune build --profile release")))
  :config
  (reformatter-define ocp-format
    :program "ocp-indent")
  (setq tuareg-opam-insinuate t)
  (tuareg-opam-update-env (tuareg-opam-current-compiler)))

(use-package utop
  :straight t
  :defer t)

(use-package dune
  :straight t
  :hook (dune-mode . dune-format-on-save-mode))

(use-package dune-format
  :straight t
  :defer t)

;;;; coq
(use-package company-coq
  :straight t
  :defer t
  :config
  ;; disable company and use corfu
  (setq company-coq-disabled-features '(company company-defaults))
  (defvar corfu-coq-backends
    (mapcar #'cape-company-to-capf
            (list #'company-coq-reserved-keywords-backend
                  #'company-coq-block-end-backend
                  #'company-coq-user-snippets-backend
                  #'company-coq-modules-backend
                  #'company-coq-context-backend
                  #'company-coq-refman-ltac-abbrevs-backend
                  #'company-coq-refman-tactic-abbrevs-backend
                  #'company-coq-refman-vernac-abbrevs-backend
                  #'company-coq-refman-scope-abbrevs-backend
                  #'company-coq-pg-backend
                  #'company-coq-local-definitions-backend
                  #'company-coq-search-results-backend
                  #'company-coq-dynamic-tactics-backend
                  #'company-coq-dynamic-symbols-backend
                  #'company-coq-math-symbols-backend)))
  (setq-local completion-at-point-functions
              (append corfu-coq-backends completion-at-point-functions)))

(use-package proof-general
  :straight t
  :hook (coq-mode . company-coq-mode)
  :init
  (setq proof-splash-enable nil
        proof-three-window-mode-policy 'hybrid))

;;;; haskell
(use-package haskell-mode
  :straight t
  :hook (haskell-mode . eglot-ensure))

;;;; agda
(use-package agda2-mode
  :load-path
  (lambda ()
    (let ((coding-system-for-read 'utf-8))
      (file-name-directory (shell-command-to-string "agda-mode locate"))))
  :mode ("\\.l?agda\\'" . agda2-mode)
  :hook ((agda2-mode . (lambda () (activate-input-method "Agda")))))

;;;; sml
(use-package sml-mode
  :straight t
  :defer t
  :hook (sml-mode . sml-format-on-save-mode)
  :config
  (reformatter-define sml-format
    :program "smlfmt"
    :args '("--stdio"))
  (setq sml-program-name "poly"))

;;;; ats
(use-package ats2-mode
  :straight
  (ats2-mode
   :type git
   :host github
   :repo "qcfu-bu/ATS2-emacs")
  :defer t)

;;;; c/c++
(use-package cc
  :init
  (setq c-default-style "k&r")
  (setq-default c-basic-offset 4)
  :hook (c-mode . eglot-ensure))

;;;; python
(use-package python
  :hook (python-mode . eglot-ensure)
  :config
  (setq python-shell-interpreter "python3.10"))

(use-package yaml-mode
  :straight t
  :defer t)

;;;; tll
(use-package tll-mode
  :load-path "~/Git/TLL"
  :demand t
  :hook (tll-mode . prettify-symbols-mode))

;;; keybinds
;;;; which-key
(use-package which-key
  :straight t
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

;;;; general
(use-package general
  :straight t
  :after evil
  :config
  (general-override-mode 1))

;;;; global
(general-create-definer spc-leader-def
  :states '(normal)
  :keymaps 'override
  :prefix "SPC")

;;;;; core
(spc-leader-def
  "SPC" 'execute-extended-command
  "\\" 'toggle-input-method
  "qr" 'restart-emacs
  "qq" 'save-buffers-kill-emacs)

;;;;; help
(spc-leader-def
  "hh" 'eldoc-box-help-at-point
  "hv" 'describe-variable
  "hf" 'describe-function
  "hF" 'describe-face
  "hs" 'describe-symbol
  "hb" 'embark-bindings
  "ht" 'consult-theme
  "hk" 'describe-key
  "hm" 'describe-mode
  "hi" 'describe-input-method
  "hc" 'consult-flymake)

;;;;; editor
(spc-leader-def
  "el" 'goto-line
  "ec" 'goto-char
  "es" 'consult-line
  "er" 'anzu-query-replace
  "ey" 'consult-yank-pop)

;;;;; compile
(spc-leader-def
  "cc" 'compile
  "cr" 'recompile)

;;;;; files
(spc-leader-def
  "ff" 'find-file
  "fr" 'consult-recent-file
  "fs" 'save-buffer)

;;;;; buffers
(spc-leader-def
  "bb" 'consult-buffer
  "bm" 'consult-imenu
  "bi" 'ibuffer
  "bo" 'mode-line-other-buffer
  "bn" 'evil-next-buffer
  "bp" 'evil-prev-buffer
  "bd" 'kill-this-buffer)

;;;;; windows
(spc-leader-def
  "ws" 'evil-window-split
  "wv" 'evil-window-vsplit
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wl" 'evil-window-right
  "wp" 'evil-window-prev
  "wn" 'evil-window-next
  "wd" 'evil-window-delete)

;;;;; frames
(spc-leader-def
  "F+" 'make-frame
  "Fo" 'other-frame
  "Fd" 'delete-frame)

;;;;; projects
(spc-leader-def
  "pp" 'project-switch-project
  "pb" 'consult-project-buffer
  "pf" 'project-find-file
  "pd" 'project-find-dir
  "pc" 'project-compile)

;;;;; bookmarks
(spc-leader-def
  "Bb" 'consult-bookmark
  "B+" 'bookmark-set
  "Bd" 'bookmark-delete)

;;;;; tab-bar
(spc-leader-def
  "TAB TAB" 'tab-bar-switch-to-tab
  "TAB +" 'tab-bar-new-tab
  "TAB p" 'tab-bar-switch-to-prev-tab
  "TAB n" 'tab-bar-switch-to-next-tab
  "TAB r" 'tab-bar-switch-to-recent-tab
  "TAB d" 'tab-bar-close-tab)

;;;;; toggles
(spc-leader-def
  "tt" 'popper-toggle-latest
  "tn" 'popper-cycle
  "tp" 'popper-cycle-backwards
  "tl" 'display-line-numbers-mode
  "tc" 'olivetti-mode
  "tz" 'presentation-mode)

;;;;; git
(spc-leader-def
  "gg" 'magit
  "gr" 'consult-git-grep)

;;;; local
(general-create-definer spc-local-leader-def
  :states '(normal)
  :keymaps 'override
  :prefix "SPC m")

;;;;; elisp
(spc-local-leader-def
  :keymaps 'emacs-lisp-mode-map
  "e" 'eval-buffer)

;;;;; org
(spc-local-leader-def
  :keymaps 'org-mode-map
  "i" 'org-insert-structure-template
  "l" 'org-insert-link
  "e" 'org-edit-special
  "x" 'org-export-dispatch)

;;;;; latex
(spc-local-leader-def
  :keymaps 'LaTeX-mode-map
  "c" 'TeX-command-master
  "e" 'TeX-command-run-all
  "v" 'TeX-view)

;;;;; coq
(spc-local-leader-def
  :keymaps 'coq-mode-map
  "." 'proof-goto-point
  "f" 'proof-assert-next-command-interactive
  "b" 'proof-undo-last-successful-command
  "pp" 'proof-process-buffer
  "pr" 'proof-retract-buffer
  "pk" 'proof-shell-exit)

;;;;; ocaml
(spc-local-leader-def
  :keymaps 'tuareg-mode-map
  "e" 'utop
  "b" 'utop-eval-buffer)

;;;;; sml
(spc-local-leader-def
  :keymaps 'sml-mode-map
  "e" 'run-sml
  "b" 'sml-prog-proc-send-buffer)

;;;;; ats
(spc-local-leader-def
  :keymaps 'ats2-mode-map
  "c" 'ats-type-check-buffer)

;;;;; python
(spc-local-leader-def
  :keymaps 'python-mode-map
  "e" 'run-python
  "b" 'python-shell-send-buffer)
