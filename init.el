;;; Init.el --- Initialization file for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;; Ern Kim's Emacs startup file

;;; Code:

;; ---------- package, use-package ---------- ;;

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("elpa"  . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-defer t
        use-package-always-ensure t ;; Set to nil normally
        use-package-expand-minimally t
        use-package-compute-statistics t))

;; ---------- PATH injection ---------- ;;
;; Update here every time you change PATH
(setenv "PATH" "/Users/e/.cabal/bin:/Users/e/.ghcup/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/Library/TeX/texbin:/Users/e/Library/Application Support/Coursier/bin:/opt/homebrew/bin:$HOME/.local/bin:$HOME/.ghcup/bin:$HOME/.ghcup/ghc/9.6.7/bin:$HOME/.cabal/bin:/Library/TeX/texbin:")
(setq exec-path (split-string (getenv "PATH") path-separator))

;; Uncomment if programs still not being found, pretty slow to load
;; (use-package exec-path-from-shell
;;   :init
;;   (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize)))


;; ---------- Marginalia, Vertico, Consult ---------- ;;

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :bind
  (("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-y"     . consult-yank-pop)
   ("C-c l"   . consult-line)
   ("C-x b" . consult-buffer))
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package orderless
  :custom
  (completion-styles '(orderless
                       basic
		       substring
		       initials
		       flex))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package nerd-icons-completion
  :after marginalia
  :init
  (nerd-icons-completion-mode)
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


;; ---------- Terminal Emulator ---------- ;;

(use-package vterm
  :bind
  (("M-s-t" . vterm)
   ("M-s-T" . vterm-other-window)))

;; (use-package multi-vterm
;;   :after vterm
;;   )


;; ---------- Preferences ---------- ;;

(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)

;; buffer management
(keymap-global-set "s-w" 'delete-window)
(keymap-global-set "C-s-w" 'kill-buffer-and-window)


;; Scroll zoom conflicts with smooth scroll momentum
(keymap-global-unset "C-<wheel-up>")
(keymap-global-unset "C-<wheel-down>")

;; Save history
(savehist-mode 1)

;; Smooth Scroll
(pixel-scroll-precision-mode 1)

;; Mood-line is MUCH faster than doom
(use-package mood-line
  :init
  (mood-line-mode t)
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code))

;; Theme
(use-package dracula-theme)

;; ---------- Programming modes ---------- ;;


;; Haskell
(use-package haskell-mode
  :mode "\\.hs$")

;; Scala
(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))


;; Latex
(use-package auctex
  :commands latex-mode
  :config
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :after auctex
  :init
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t
        TeX-save-query nil)
  :config
  (pdf-tools-install))

;; ---------- Custom ---------- ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("4acfb4e3d5e86206c4c3a834f4a9356beb25dc04c48e4e364006eff5625606ab"
     default))
 '(mood-line-format
   '((" " (mood-line-segment-modal) " "
      (or (mood-line-segment-buffer-status) " ") " "
      (mood-line-segment-buffer-name) "  " (mood-line-segment-anzu)
      "  " (mood-line-segment-multiple-cursors) "  " " " "")
     ((mood-line-segment-vc) "  " (mood-line-segment-major-mode) "  "
      (mood-line-segment-misc-info) "  " (mood-line-segment-checker)
      "  " (mood-line-segment-process) "  " " ")))
 '(package-selected-packages
   '(auctex consult dracula-theme exec-path-from-shell flycheck
            haskell-mode marginalia mood-line nerd-icons-completion
            orderless pdf-tools sbt-mode scala-mode vertico vterm))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Meslo LG S" :foundry "nil" :slant normal :weight regular :height 140 :width normal)))))


;; ---------- End ---------- ;;

(setq gc-cons-threshold (* 1 1000 1000)
      gc-cons-percentage 0.5)

(provide 'init)

;;; init.el ends here
