;;; Init.el --- Initialization file for Emacs -*- lexical-binding: t;-*-

;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:


;;; CUSTOM


(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;; PREFERENCES

;; initial major mode to fundamental, want to defer prog-mode hook
(setq initial-major-mode 'fundamental-mode)

;; Save sessions history
(setq savehist-save-minibuffer-history 1)
(savehist-mode t)

;; Dim auxiliary buffers
(solaire-global-mode +1)

;; no line wrapping, yes horizontal scroll
(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)
;; disable scroll wheel zooming, needed for smooth scroll momentum
(keymap-global-unset "C-<wheel-up>")
(keymap-global-unset "C-<wheel-down>")

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)

;; VERTICO, CONSULT, ETC.
(use-package consult
  :bind
  (("M-g M-g" . consult-goto-line)
   ("M-y" . consult-yank-pop)
   ("C-c l" . consult-line)
   ("C-c b" . consult-buffer))
  :hook (completion-list-mode . consult-preview-at-point-mode))


;; Can't defer
(use-package marginalia
  :init
  (marginalia-mode))
 
(use-package orderless
  :custom
  (completion-styles '(basic
		       substring
		       initials
		       flex
		       orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


;; Appearance
;;(load-theme 'catppuccin :no-confirm)

(use-package mood-line)

;;; TERMINAL
(use-package vterm
  :bind
  (("M-s-t" . vterm)
   ("M-s-T" . vterm-other-window)))

;; Scroll half screen
(use-package golden-ratio-scroll-screen
  :defer
  :bind
  (("C-v" . 'golden-ratio-scroll-screen-down)
   ("M-v" . 'golden-ratio-scroll-screen-up)))




;;; LANGUAGES

;; LSP MODE
(use-package lsp-mode
  :hook prog-mode  
  :config
  (lsp
  (setq lsp-warn-no-matched-clients nil
        lsp-file-watch-threshold 5000)))


;; COMPANY
(use-package company
  :after lsp-mode
  :config
  (setq company-minimum-prefix-length 1
		company-selection-wrap-around t
		company-tooltip-align-annotations t
		company-tooltip-annotation-padding 2
		company-tooltip-limit 9
		company-show-quick-access 'left)
  :init
  (global-company-mode))

;; HASKELL
(use-package haskell-mode
  :mode "\\.hs$")

;;; make environment variables available (not path inj?)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;; AGDA
(load (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")) nil t)

;; LEAN 4
(use-package lean4-mode
  :commands lean4-mode
  :vc (:url "https://github.com/leanprover-community/lean4-mode.git"))


;; LATEX
(use-package auctex
  :commands latex-mode
  :config
  (setq TeX-auto-save t
        TeX-parse-self t)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :init
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t
        TeX-save-query nil)
  :config
  (pdf-tools-install))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(adwaita))
 '(custom-safe-themes
   '("7bea8c8136b95e40a3def71cc2953e29d2553078ba1730d8262f1dccc586fbab"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     default))
 '(line-number-mode nil)
 '(mood-line-format
   '((" " (mood-line-segment-modal) " "
      (or (mood-line-segment-buffer-status) " ") " "
      (mood-line-segment-buffer-name) "  " (mood-line-segment-anzu)
      "  " (mood-line-segment-multiple-cursors) "  " " " "")
     ((mood-line-segment-vc) "  " (mood-line-segment-major-mode) "  "
      (mood-line-segment-misc-info) "  " (mood-line-segment-checker)
      "  " (mood-line-segment-process) "  " " ")))
 '(mood-line-glyph-alist
   '((:checker-info . 8627) (:checker-issues . 8594)
     (:checker-good . 10003) (:checker-checking . 10227)
     (:checker-errored . 120) (:checker-interrupted . 61)
     (:vc-added . 43) (:vc-needs-merge . 10231)
     (:vc-needs-update . 8595) (:vc-conflict . 120) (:vc-good . 10003)
     (:buffer-narrowed . 9698) (:buffer-modified . 9679)
     (:buffer-read-only . 9632) (:frame-client . 57504)
     (:count-separator . 215)))
 '(mood-line-mode t)
 
 '(package-selected-packages
   '(auctex company consult exec-path-from-shell haskell-mode lean4-mode
            marginalia orderless pdf-tools solaire-mode vertico vterm))
 '(tool-bar-mode nil))
;; end
(setq gc-cons-threshold (* 2 1000 1000)
      gc-cons-percentage 0.5)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "MesloLGS Nerd Font Mono" :foundry "nil" :slant normal :weight regular :height 140 :width normal)))))


(provide 'init)
