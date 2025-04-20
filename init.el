;;; Init.el --- Initialization file for Emacs -*- lexical-binding: t;-*-

;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:

;;; CUSTOM
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("7bea8c8136b95e40a3def71cc2953e29d2553078ba1730d8262f1dccc586fbab"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     default))
 '(package-selected-packages
   '(auctex catppuccin-theme company consult doom-modeline
            exec-path-from-shell haskell-mode lean4-mode marginalia
            orderless pdf-tools vertico vterm))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "nil" :slant normal :weight regular :height 140 :width normal)))))


(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;; make environment variables available (not path inj?)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; PREFERENCES

;; initial major mode to fundamental, want to defer prog-mode hook
(setq initial-major-mode 'fundamental-mode)
;; yes smooth scroll
(pixel-scroll-precision-mode)
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

;;; VERTICO, CONSULT, ETC.
(use-package consult
  :bind (("C-c l" . consult-line)
         ("C-c b" . consult-buffer)))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :after vertico
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

;;; APPEARANCE
(load-theme 'catppuccin :no-confirm)

(use-package doom-modeline
  :init
  (doom-modeline-mode))


;;; TERMINAL
(use-package vterm
  :commands vterm)


;;; LANGUAGES

;; LSP MODE
(use-package lsp-mode
  :hook
  prog-mode
  :custom
  (lsp-completion-provider :nil)
  :config
  (setq lsp-warn-no-matched-clients nil
        lsp-file-watch-threshold 5000))


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

;; PYTHON



;; HASKELL
(use-package haskell-mode
  :mode "\\.hs$")

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


;; end
(setq gc-cons-threshold (* 2 1000 1000)
      gc-cons-percentage 0.5)

(provide 'init)
