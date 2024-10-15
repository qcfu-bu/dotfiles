;; Prevent package.el loading packages prior to init-file loading.
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Frame appearance 
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

(defun configure-fonts ()
  (set-face-attribute 'default        nil :font "JetBrains Mono-11")
  (set-face-attribute 'fixed-pitch    nil :font "JetBrains Mono-11")
  (set-face-attribute 'variable-pitch nil :font "JetBrains Mono-11"))
(add-hook 'after-make-frame-functions (lambda (f) (configure-fonts)))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)
(setq ring-bell-function 'ignore)
(setq-default frame-title-format "GNU Emacs")
