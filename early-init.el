;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Increase thresholds, reset at end of init.el
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.8
      read-process-output-max (* 1024 1024))

;; Initial appearance
(setq ns-use-proxy-icon nil
      frame-title-format "Emacs\n"
      frame-resize-pixelwise t
      use-file-dialog nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode
      ring-bell-function 'ignore
      inhibit-compacting-font-caches t
      default-frame-alist
      '((height . 0.45)
        (width . 0.4)
        (left . 0.455)
        (top . 0.25)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)))

;; Hack startup message
(defun display-startup-echo-area-message ()
  (message ""))

;; Blend title bar
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Hide toolbar
(tool-bar-mode 0)

;; No line wrapping, no tabs
(setq-default truncate-lines t
	      indent-tabs-mode nil)

;; lsp-mode performance thing
(setenv "LSP_USE_PLISTS" "true")

;; end
(provide 'early-init)
