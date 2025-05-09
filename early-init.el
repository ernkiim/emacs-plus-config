;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Performance
(setq gc-cons-threshold (* 100 1000 1000)
      gc-cons-percentage 0.8)
(setq read-process-output-max (* 64 1000))

;; transparent title bar
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ; nil for dark text
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; no title bar text
(setq ns-use-proxy-icon nil)
(setq frame-title-format "\n")

;; fix bottom gap
(setq frame-resize-pixelwise t)


;; no tool bar
(tool-bar-mode 0)

;; no dialog box
(setq use-file-dialog nil)

;; no line wrap
(setq-default truncate-lines t)

;; i know what GNU is
(setq inhibit-startup-echo-area-message t)
(defun display-startup-echo-area-message ()
  (message ""))

;; lsp-mode performance thing
(setenv "LSP_USE_PLISTS" "true")

;; no scroll bar
(scroll-bar-mode 0)

(setq
 ;; no splash screen
 inhibit-startup-screen t
 ;; i know what scratch is
 initial-scratch-message nil
 ;; silence
 ring-bell-function 'ignore)


