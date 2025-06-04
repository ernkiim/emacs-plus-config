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
  (setq use-package-always-ensure nil ; Set to nil normally
        use-package-expand-minimally t
        use-package-compute-statistics t))


;; ---------- Preferences ---------- ;;

;; Much faster than doom-modeline
(use-package mood-line
  :init (mood-line-mode +1)
  :custom (mood-line-glyph-alist mood-line-glyphs-fira-code))

;; Runs on first frame of emacs or emacsclient

;; Misc. preferences
(use-package emacs
  :defer nil
  :bind (("M-o" . other-window)
         ("C-<wheel-up>"   . nil)  ; Momentum can trigger scroll wheel bindings
         ("C-<wheel-down>" . nil))
  :config
  ;; Hack startup message
  (defun display-startup-echo-area-message ()
    (message ""))
  :custom
  ;; Horizontal scrolling (in macos direction)
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-flip-direction t)
  ;; Defer prog hook
  (initial-major-mode 'fundamental-mode)
  ;; Silence
  (ring-bell-function 'ignore)
  ;; No line wrapping
  (truncate-lines t)
  ;; Indent with spaces
  (indent-tabs-mode nil)
  ;; Suppress annoying little confirm windows
  (use-dialog-box nil)
  ;; Hide startup mess
  (inhibit-startup-screen t)
  (initial-scratch-message "")
  ;; Suppress emacsclient "C-x 5 0" and "C-x #" reminders
  (server-client-instructions nil)
  ;; 'y' instead of 'yes'
  (use-short-answers t)
  ;; This is pretty neat
  (show-paren-context-when-offscreen 'overlay) ; Emacs 29
  :hook
  ((after-init . pixel-scroll-precision-mode)
   (after-init . (lambda () (define-key input-decode-map (kbd "C-i") (kbd "H-i"))))
   (server-after-make-frame . (lambda () (define-key input-decode-map (kbd "C-i") (kbd "H-i"))))))


(use-package dired
  ;; Navigation normally opens a new buffer for every file traversed, want to kill as we go
  :custom ((dired-kill-when-opening-new-dired-buffer t)))

(use-package savehist
  :hook after-init)

;; Theme
(use-package dracula-theme)


;; ---------- Vert&co (and corfu) ---------- ;;

(use-package marginalia
  :hook after-init)

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package consult
  :bind
  (("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-y"     . consult-yank-pop)
   ("s-f"     . consult-line)
   ("C-x b"   . consult-buffer)
   ("C-x C-b" . consult-buffer))
  :hook (completion-list-mode . consult-preview-at-point-mode))


(use-package vertico
  :hook after-init
  :custom (vertico-cycle t))

(use-package orderless
  :custom
  (completion-styles '(orderless
                       basic
		       substring
		       initials
		       flex))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Corfu completion

(use-package corfu
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . nil))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)) ; Popup completion info


;; ---------- Abo-abo ---------- ;;

(defconst homerow
  '(?a ?r ?s ?t ?n ?e ?i ?o) "colemak home row")

;; (use-package ace-window
;;   :custom
;;   (aw-keys homerow)
;;   (aw-dispatch-always nil)
;;   :bind ("M-o" . ace-window))
  


(use-package avy
  :load-path "~/.emacs.d/ernkiim-avy"
  :custom
  (avy-keys homerow)
  (avy-background t)
  (avy-dispatch-alist
   '((?x . avy-action-kill-move)
     (?X . avy-action-kill-stay)
     (?g . avy-action-teleport)
     (?m . avy-action-mark)
     (?c . avy-action-copy)
     (?y . avy-action-yank)
     (?Y . avy-action-yank-line)
     (?i . avy-action-ispell)
     (?z . avy-action-zap-to-char)))
  :custom-face
  (avy-background-face ((t :inherit shadow)))
  :bind
  (("H-i" . avy-goto-char-2) ; C-i
   :map isearch-mode-map ("H-i" . avy-isearch)))


;; ---------- Terminal Emulator ---------- ;;

(use-package vterm
  :bind
  (("s-t" . vterm)
   ("M-s-t" . vterm-other-window))
  :custom
  (vterm-clear-scrollback-when-clearing t))

;; (use-package multi-vterm
;;   :after vterm
;;   )

;; ---------- Programming modes ---------- ;;

;; LSP
(use-package eglot
  :hook ((scala-ts-mode . eglot-ensure)
         (haskell-ts-mode . eglot-ensure))
  :config  
  (add-to-list 'eglot-server-programs
               `((scala-mode scala-ts-mode)
                 . ,(alist-get 'scala-mode eglot-server-programs))) ; this lets eglot recognize scala somehow
  :custom
  (eglot-autoshutdown t))


;; Haskell
(use-package haskell-ts-mode
  :mode "\\.hs$"
  :custom
  (haskell-ts-font-lock-level 4)
  (haskell-ts-use-indent t)
  (haskell-ts-ghci "ghci")
  (haskell-ts-use-indent t)
  :config
  (add-to-list 'treesit-language-source-alist
   '(haskell . ("https://github.com/tree-sitter/tree-sitter-haskell" "v0.23.1")))
  (unless (treesit-grammar-location 'haskell)
   (treesit-install-language-grammar 'haskell)))


;; Agda 2.7.0.1
;; Recompile if reinstalling agda-mode
;; Remove warning suppress from custom when lexical binding fixed
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; Scala
(use-package scala-ts-mode
  :interpreter ("scala" . scala-ts-mode)
  :custom (treesit-font-lock-level 4))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package scala-repl
  :commands scala-repl-run)


;; Latex
(use-package auctex
  :commands latex-mode
  :config
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  :custom
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (TeX-source-correlate-start-server t)
  (TeX-save-query nil))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install)
  :custom
  ((pdf-view-resize-factor 1.1)
   (pdf-view-display-size 'fit-page)))


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
   '(((propertize " " 'display '(raise -0.25)) (mood-line-segment-modal)
      " " (or (mood-line-segment-buffer-status) " ")
      (propertize " " 'display '(raise 0.25))
      (mood-line-segment-buffer-name) "  " (mood-line-segment-anzu)
      "  " (mood-line-segment-multiple-cursors) "")
     ((mood-line-segment-vc) "  " (mood-line-segment-major-mode) "  "
      (mood-line-segment-misc-info) "  " (mood-line-segment-checker)
      "  " (mood-line-segment-process) " ")))
 '(package-selected-packages
   '(auctex consult corfu dracula-theme haskell-ts-mode marginalia
            mood-line nerd-icons-completion orderless pdf-tools
            sbt-mode scala-repl scala-ts-mode vertico vterm))
 '(tool-bar-mode nil)
 '(warning-suppress-types
   '((files missing-lexbind-cookie
            "~/.cabal/store/ghc-9.6.7/Agd-2.7.0.1-3c651abb/share/emacs-mode/agda2.el")
     (files missing-lexbind-cookie
            "~/.cabal/store/ghc-9.6.7/Agd-2.7.0.1-3c651abb/share/emacs-mode/agda2.el"))))

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ---------- End ---------- ;;

(setq gc-cons-threshold (* 1 1024 1024)
      gc-cons-percentage 0.5)

(provide 'init)
