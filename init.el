;;; Init.el --- Initialization file for Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Ern Kim's Emacs startup file

;;; Code:

;; ---------- package, use-package ---------- ;;

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
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

;; ;; Padding
(use-package spacious-padding
  :hook after-init
  :custom
  (spacious-padding-subtle-mode-line '(:mode-line-inactive "#282a36"))
  (spacious-padding-widths '( :internal-border-width 10
                              :header-line-width 4
                              :mode-line-width 4
                              :tab-width 4
                              :right-divider-width 20
                              :scroll-bar-width 8)))

;; Misc. preferences
(use-package emacs
  :config
  ;; Clear echo messages after delay
  (run-with-idle-timer 3 t (lambda () (message "")))
  ;; Hack startup message
  (defun display-startup-echo-area-message ()
    (message ""))
  ;; Delete files into trash bin (when using dired, etc.)
  (setq delete-by-moving-to-trash t)
  (if (eq system-type 'darwin)
      (setq trash-directory "~/.Trash"))	

  ;; Load spotify_player integration
  (load-file "~/.emacs.d/spotify-player.el")

  :bind
  (("M-o" . other-window)
   ("C-<wheel-up>"   . nil)  ; Momentum can trigger scroll wheel bindings
   ("C-<wheel-down>" . nil)
   ("C-." . scroll-up-line)
   ("C-," . scroll-down-line)
   ;; Spotify bindings
   ("C-c C-s"   . spotify)
   ("C-c C-SPC" . spotify-pause-resume)
   ("C-c C-n"   . spotify-next)
   ("C-c C-p"   . spotify-prev))

  :custom
  ;; Echo keystrokes as they are entered
  (echo-keystrokes 0.01)
  ;; Don't show C-h reminder text in echo box
  (echo-keystrokes-help nil)
  ;; Thin line cursor (instead of box)
  (cursor-type 'bar)
  ;; Horizontal scrolling (in macos direction)
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-flip-direction t)
  ;; Defer prog hook
  (initial-major-mode 'fundamental-mode)
  ;; Vterm on startup
  (initial-buffer-choice 'vterm)
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
  ((after-init . spotify-init)
   (after-init . savehist-mode)
   (after-init . pixel-scroll-precision-mode)
   (after-init . (lambda ()
                   (define-key input-decode-map
                               (kbd "C-i")
                               (kbd "H-i"))))
   (server-after-make-frame . (lambda ()
                                (define-key input-decode-map
                                            (kbd "C-i")
                                            (kbd "H-i"))))))

;; Hide mode line on scratch (also hides spacious-padding artifact)
(use-package hide-mode-line
  :hook vterm-mode)

;; Auto pair balanced expressions.
(use-package electric-pair-mode
  :hook after-init)

;; Scroll half page
(use-package view
  :bind
  (("C-v" . View-scroll-half-page-forward)
   ("M-v" . View-scroll-half-page-backward)))

;; Much faster than doom-modeline
(use-package mood-line                  
  :init
  (mood-line-mode +1)
  :custom (mood-line-glyph-alist mood-line-glyphs-fira-code))


;; ---------- Typing ---------- ;;

(use-package typit
  :defer t)

;; ---------- Dired ---------- ;;

(use-package dired
  :hook
  (dired-mode . dired-hide-details-mode)
  :custom
  ;; Keep only current file's dired buffer
  (dired-kill-when-opening-new-dired-buffer t))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))


;; ---------- Vert&co (and corfu) ---------- ;;

(use-package marginalia
  :hook after-init)

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode +1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package consult
  :bind
  (("s-l"     . consult-goto-line)
   ("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-y"     . consult-yank-pop)
   ("s-f"     . consult-line)
   ("C-x b"   . consult-buffer)
   ("C-x C-b" . consult-buffer))
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package vertico
  :hook after-init
  :custom (vertico-cycle t))

(use-package vertico-posframe
  :hook vertico-mode)

(use-package orderless
  :custom
  (completion-styles '(orderless
                       basic
		       substring
		       initials
		       flex))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("C-<return>" . corfu-insert)
              ("RET"        . nil))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)) ; Popup completion info



;; ---------- Abo-abo ---------- ;;

(defconst homerow
  '(?n ?t ?e ?s ?i ?r ?o ?a))

;; (use-package ace-window
;;   :custom
;;   (aw-keys homerow)
;;   (aw-dispatch-always nil)
;;   :bind ("M-o" . ace-window))


(use-package avy                        
  :config
  (add-to-list 'avy-orders-alist '(avy-goto-char-2 . avy-order-closest))

  (defun avy-mk-stay-action (pt fn)
    (save-excursion
      (goto-char pt)
      (funcall fn))
    (select-window
     (cdr
      (ring-ref avy-ring 0))))
  
  (defun avy-action-comment-line-stay (pt)
    (avy-mk-stay-action
     pt
     (lambda () (comment-line 1))))

  (defun avy-action-comment-defun-stay (pt)
    (avy-mk-stay-action
     pt
     (lambda ()
       (mark-defun)
       (comment-region (region-beginning) (region-end)))))
  
  :custom
  (avy-keys homerow)
  (avy-background t)
  (avy-single-candidate-jump nil)
  (avy-dispatch-alist
   '((?\; . avy-action-comment-line-stay)
     (?\: . avy-action-comment-defun-stay)
     (?x . avy-action-kill-move)
     (?X . avy-action-kill-stay)
     (?g . avy-action-teleport)
     (?m . avy-action-mark)
     (?c . avy-action-copy)
     (?y . avy-action-yank)
     (?Y . avy-action-yank-line)
     (?z . avy-action-zap-to-char)))
  :custom-face
  (avy-background-face ((t :inherit shadow)))
  :bind
  (("H-i" . avy-goto-char-2) ; C-i
   :map isearch-mode-map ("H-i" . avy-isearch)))


;; ---------- Terminal Emulator ---------- ;;

(use-package vterm
  :commands vterm-mode
  :bind
  (("s-t" . vterm)
   ("M-s-t" . vterm-other-window)
   (:map vterm-mode-map ("C-y" . vterm-yank)))
  :custom
  (vterm-clear-scrollback-when-clearing t))


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


;; ;; Agda 2.7.0.1
;; ;; Recompile if reinstalling agda-mode
;; (load-file (let ((coding-system-for-read 'utf-8))
;;              (shell-command-to-string "agda-mode locate")))

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
  (TeX-save-query nil)
  (TeX-electric-math '("\\(" . "\\)"))
  (LaTeX-electric-left-right-brace t))

(use-package pdf-tools
  :magic
  ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (add-to-list 'pdf-tools-enabled-modes 'pdf-view-themed-minor-mode)
  :custom
  (pdf-view-resize-factor 1.1)
  (pdf-view-display-size 'fit-page))


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
      "  " (mood-line-segment-multiple-cursors) "  ")
     ((mood-line-segment-vc) "  " (mood-line-segment-major-mode) "  "
      (mood-line-segment-misc-info) "  " (mood-line-segment-checker)
      "  " (mood-line-segment-process) "  " " ")))
 '(package-selected-packages
   '(auctex avy consult corfu dracula-theme haskell-ts-mode
            hide-mode-line marginalia mood-line nerd-icons-completion
            nerd-icons-dired orderless pdf-tools sbt-mode scala-repl
            scala-ts-mode solaire spacious-mode spacious-padding typit
            vertico vertico-posframe vertico-quick vterm))
 '(tool-bar-mode nil))

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
















































