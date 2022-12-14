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
(setq make-backup-files nil)
(setq dired-use-ls-dired nil)
(setq frame-resize-pixelwise t)
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
	trash-directory "~/.Trash"))
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package esup
  :straight t
  :config
  (setq esup-depth 0))

(use-package display-line-numbers
  :hook
  ((prog-mode text-mode) . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3))

(use-package undo-fu
  :straight t)

(use-package undo-fu-session
  :straight t
  :after undo-fu
  :config
  (global-undo-fu-session-mode))

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
	evil-undo-system 'undo-fu)
  :config (evil-mode 1))

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

(use-package ivy-rich
  :straight t
  :after counsel
  :config
  (ivy-rich-mode t))

(use-package orderless
  :straight t)

(use-package counsel
  :straight t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))
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
  (setq company-minimum-prefix-length 2)
  :config
  (global-company-mode)
  (company-prescient-mode)
  (prescient-persist-mode))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(use-package eglot
  :straight t
  :defer t
  :after yasnippet
  :config
  (add-to-list 'eglot-server-programs '(latex-mode . ("texlab"))))

;; git
(use-package magit
  :straight t
  :defer t)

(use-package git-gutter
  :straight t
  :config
  (global-git-gutter-mode 1))

(use-package git-gutter-fringe
  :straight t
  :config
  (setq git-gutter-fr:side 'right-fringe))

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
(use-package tab-bar
  :config
  (setq tab-bar-new-tab-choice "*scratch*"))

(use-package popwin
  :straight t
  :config
  (popwin-mode 1))

(use-package hide-mode-line
  :straight t)

(use-package vterm
  :straight t
  :defer t
  :hook
  ((vterm-mode . (lambda () (setq confirm-kill-processes nil)))
   (vterm-mode . hide-mode-line-mode))
  :config
  (setq vterm-kill-buffer-on-exit t
	vterm-max-scrollback 5000))

(use-package vterm-toggle
  :straight t
  :defer t
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
	       '((lambda (buffer-or-name _)
		   (let ((buffer (get-buffer buffer-or-name)))
		     (with-current-buffer buffer
		       (or (equal major-mode 'vterm-mode)
			   (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
		 (display-buffer-reuse-window display-buffer-at-bottom)
		 (reusable-frames . visible)
		 (window-height . 0.3))))


;; appearance
(setq modus-themes-subtle-line-numbers t
      modus-themes-vivendi-color-overrides
      '((bg-main . "#1d2021")
	(bg-dim . "#1d2021")
	(bg-tab-active . "#1d2021")
	(bg-tab-inactive . "#323232")))
(load-theme 'modus-vivendi t)
(set-face-attribute 'default nil :font "Fira Code-14")
(set-face-attribute 'variable-pitch nil :font "Fira Sans-16")

(use-package solaire-mode
  :straight t
  :init
  (defun solaire-mode-real-buffer-custom-p ()
    (cond ((string= (buffer-name (buffer-base-buffer)) "*scratch*") t)
          ((buffer-file-name (buffer-base-buffer)) t)
          (t nil)))
  (setq solaire-mode-real-buffer-fn #'solaire-mode-real-buffer-custom-p)
  :config
  (solaire-global-mode))

(use-package smartparens
  :straight t
  :hook
  ((prog-mode text-mode) . smartparens-global-mode)
  :config
  (sp-pair "'" nil :actions :rem))

(use-package rainbow-delimiters
  :straight t
  :hook
  ((prog-mode text-mode) . rainbow-delimiters-mode))

(use-package doom-modeline
  :straight t
  :init
  (setq doom-modeline-icon nil
	doom-modeline-modal nil)
  (doom-modeline-mode 1))

;; prose
(use-package adaptive-wrap
  :straight t
  :hook
  (visual-line-mode . adaptive-wrap-prefix-mode))

(use-package pdf-tools
  :straight t
  :hook
  ((pdf-view-mode . pdf-view-themed-minor-mode)
   (pdf-view-mode
    . (lambda () (set (make-local-variable 'evil-normal-state-cursor) (list nil)))))
  :init
  (pdf-loader-install)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t
	pdf-view-use-imagemagick nil)
  (evil-define-key 'normal pdf-view-mode-map
    (kbd "zm") 'pdf-view-themed-minor-mode))

(use-package auctex
  :straight t
  :hook
  ((LaTeX-mode . eglot-ensure)
   (LaTeX-mode . visual-line-mode)
   (LaTeX-mode . flyspell-mode))
  :init
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (setq-default TeX-PDF-mode t)
  (setq TeX-parse-self t
	TeX-auto-save t
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

(use-package org-superstar
  :straight t
  :defer t)

(use-package org
  :straight t
  :defer t
  :hook
  ((org-mode . org-superstar-mode)
   (org-mode . visual-line-mode)
   (org-mode . flyspell-mode))
  :config
  (setq org-startup-indented t))

(use-package org-roam
  :straight t
  :defer t
  :config
  (setq org-roam-directory (file-truename "~/git/RoamNotes"))
  (org-roam-db-autosync-mode))

;; code
(use-package company-coq
  :straight t
  :defer t)

(use-package proof-general
  :straight t
  :hook
  (coq-mode . company-coq-mode)
  :init
  (setq proof-splash-enable nil))

(use-package tuareg
  :straight t
  :after ocp-indent
  :hook
  (tuareg-mode . eglot-ensure)
  :config
  (setq tuareg-opam-insinuate t)
  (tuareg-opam-update-env (tuareg-opam-current-compiler)))

(use-package ocp-indent
  :straight t
  :config
  (defun ocp-indent-buffer ()
    (interactive nil)
    (ocp-indent-region 1 (buffer-size))))

(use-package utop
  :straight t
  :defer t
  :hook
  (tuareg-mode . utop-minor-mode)
  :config
  (push '("*utop*" :stick t) popwin:special-display-config))

(use-package dune
  :straight t)

(use-package dune-format
  :straight t
  :hook
  (dune-mode . dune-format-on-save-mode))

(use-package haskell-mode
  :straight t
  :hook
  (haskell-mode . eglot-ensure)
  :config
  (push '("*haskell*" :stick t) popwin:special-display-config))

(use-package ats2-mode
  :straight (ats2-mode :type git :host github :repo "qcfu-bu/ATS2-emacs")
  :defer t)

;; keybinding
(spc-leader-def
  ;; general
  ""    nil
  "SPC" 'counsel-M-x
  "\\" 'toggle-input-method
  ;; help
  "hv" 'counsel-describe-variable
  "hf" 'counsel-describe-function
  "hs" 'counsel-describe-symbol
  "hb" 'counsel-descbinds
  "ht" 'counsel-load-theme
  ;; files
  "ff" 'counsel-find-file
  "fr" 'counsel-recentf
  "fl" 'counsel-find-library
  "fs" 'save-buffer
  ;; buffers
  "bb" 'counsel-switch-buffer
  "bi" 'counsel-imenu
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
  "wp" 'evil-window-prev
  "wn" 'evil-window-next
  "wd" 'evil-window-delete
  ;; projects
  "pp" 'counsel-projectile-switch-project
  "pf" 'counsel-projectile-find-file
  "pd" 'counsel-projectile-find-dir
  "pg" 'counsel-projectile-grep
  ;; org-roam
  "nf" 'org-roam-node-find
  "ng" 'org-roam-graph
  "ni" 'org-roam-node-insert
  "nc" 'org-roam-capture
  ;; toggles
  "tt" 'tab-bar-mode
  "tl" 'display-line-numbers-mode
  ;; terminal
  "ot" 'vterm-toggle
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
  "a" 'TeX-command-run-all
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
  "u" 'utop
  "=" 'ocp-indent-buffer)

;; miscellaneous
(add-to-list 'load-path "~/Git/TLL")
(require 'tll-mode)
