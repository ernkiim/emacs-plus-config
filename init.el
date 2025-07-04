;;; Init.el --- Initialization file for Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Ern Kim's Emacs startup file

;;; Code:

;; ---------- package, use-package ---------- ;;

(require 'package)
(add-to-list 'package-archives '("gnu"    . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/"))
;; (add-to-list 'package-archived '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure nil ; Set to nil normally
        use-package-expand-minimally t
        use-package-compute-statistics t))


;; ---------- Preferences ---------- ;;

;; Misc. preferences
(use-package emacs
  :config
  ;; Clear echo messages after delay
  (run-with-idle-timer 3 t (lambda () (message "")))
  ;; Intercept startup message
  (defun display-startup-echo-area-message ()
    (message ""))
  ;; Delete files into trash bin (when using dired, etc.)
  (setq delete-by-moving-to-trash t)
  (when (eq system-type 'darwin)
      (setq trash-directory "~/.Trash"))

  (defvar first-call t)
  :bind
  (("M-o" . other-window)
   ("M-u" . up-list)
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
  ;; start on vterm buffer
  (initial-buffer-choice 'vterm)
  ;; Echo keystrokes as they are entered
  (echo-keystrokes 0.01)
  ;; Don't show C-h reminder text in echo box
  (echo-keystrokes-help nil)
  ;; Thin line cursor (instead of box)
  (cursor-type 'bar)
  ;; Horizontal scrolling (in macos direction)
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-flip-direction t)
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
  ;; 'y' instead of 'yes'
  (use-short-answers t)
  ;; This is pretty neat
  (show-paren-context-when-offscreen 'overlay) ; Emacs 29
  :hook
  ((after-init . (lambda ()     ; start dedicated "spotify" vterm buffer
                   (load "~/.emacs.d/spotify-player.el" nil t)
                   (spotify-init)))
   (after-init . pixel-scroll-precision-mode)
   (after-init . (lambda ()
                   "intercept C-i, decode to H-i instead of TAB"
                   (define-key input-decode-map
                               (kbd "C-i")
                               (kbd "H-i"))))))

;; Theme
(use-package dracula-theme)

;; Padding
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

;; Server-specific config
(use-package server
  :custom
  (server-client-instructions nil)
  :hook
  (server-after-make-frame . (lambda ()
                               "input-decode-map is session-local, have to redefine for each client"
                               (define-key input-decode-map
                                           (kbd "C-i")
                                           (kbd "H-i")))))


;; Auto pair balanced expressions.
(use-package electric-pair
  :hook after-init)

;; Much faster than doom-modeline
(use-package mood-line                  
  :init
  (mood-line-mode +1)
  :custom (mood-line-glyph-alist mood-line-glyphs-fira-code))


;; ---------- Org ---------- ;;

(use-package org
  :custom
  (org-log-done-time 'time)
  (org-return-follows-link t)
  :hook
  (org-mode . (org-indent-mode
               visual-line-mode)))

;; (use-package org
;;   :custom
;;   (org-agenda-files "~/org")
;;   (org-log-done 'time)
;;   (org-return-follows-link t)
;;   :mode ("\\.org\\'" . org-mode)
;;   :hook
;;   (org-mode . (org-indent-mode
;;                visual-line-mode)))

(use-package org-roam
  :ensure t)


;; ---------- Window ---------- ;;

(use-package winum
  :hook after-init
  :config
  (defun my-winum-assign ()
    "Manually assign number based on buffer"
    (cond
     ((equal (buffer-name) "spotify") 9)))
  
  (add-to-list 'winum-assign-functions #'my-winum-assign)
  :custom
  ;; Insert numbers manually in custom mode line
  (winum-auto-assign-0-to-minibuffer t)
  (winum-auto-setup-mode-line nil)
  :custom-face
  ;; Window number face
  (winum-face ((t (:inherit mood-line-unimportant))))
  :bind
  (("s-0" . winum-select-window-0-or-10)
   ("s-1" . winum-select-window-1)
   ("s-2" . winum-select-window-2)
   ("s-3" . winum-select-window-3)
   ("s-4" . winum-select-window-4)
   ("s-5" . winum-select-window-5)
   ("s-6" . winum-select-window-6)
   ("s-7" . winum-select-window-7)
   ("s-8" . winum-select-window-8)
   ("s-9" . winum-select-window-9)))

;; Hide mode line on vterm
(use-package hide-mode-line
  :hook
  (vterm-mode
   pdf-view-mode))


;; ---------- Magit ---------- ;;

(use-package magit
  :init
  ;; Use child clients for commit messages
  (use-package with-editor
    :defer t)
  :custom
  ;; Add magit-status, -dispatch, and -file-dispatch actions to global
  (magit-maybe-define-global-key-bindings recommended)
  ;; Don't refresh status buffer, only current buffer
  (magit-refresh-status-buffer nil)
  ;; Use system trash for discard and delete
  (magit-delete-by-moving-to-trash t)
  ;; Disable confirmation for some actions
  (magit-no-confirm '(set-and-push
                      discard ;; Careful if not delete by moving to trash
                      trash)))


;; ---------- Dired ---------- ;;

(use-package dired
  :hook
  (dired-mode . dired-hide-details-mode)
  :custom
  ;; Keep only current file's dired buffer
  (dired-kill-when-opening-new-dired-buffer t)
  ;; Show symlink targets in hide-details-mode
  (dired-hide-details-hide-symlink-targets nil)
  :bind
  (:map dired-mode-map ("^" . dired-up-directory)))

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
              ("M-SPC"      . corfu-insert-separator))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)) ; Popup completion info


;; ---------- Abo-abo ---------- ;;

(defconst homerow
  '(?n ?t ?e ?s ?i ?r ?o ?a)
  "Colemak home row in order of strong fingers")

(use-package avy                        
  :config
  ;; Let closer matches be shallower in selection tree
  (add-to-list 'avy-orders-alist '(avy-goto-char-2 . avy-order-closest))

  (defun avy-mk-stay-action (pt fn)
    "Return avy action which calls fn at pt without moving point"
    (save-excursion
      (goto-char pt)
      (funcall fn))
    (select-window
     (cdr
      (ring-ref avy-ring 0))))
  
  (defun avy-action-comment-line-stay (pt)
    "Comment line at pt without moving point"
    (avy-mk-stay-action
     pt
     (lambda () (comment-line 1))))

  (defun avy-action-comment-defun-stay (pt)
    "Comment defun at pt without moving point"
    (avy-mk-stay-action
     pt
     (lambda ()
       (mark-defun)
       (comment-region (region-beginning) (region-end)))))
  
  :custom
  ;; Use colemak home row
  (avy-keys homerow)
  ;; Set all non-matched text to background color when selecting
  (avy-background t)
  ;; Allow action on single match
  (avy-single-candidate-jump nil)
  (avy-dispatch-alist
   '((?\; . avy-action-comment-line-stay)  ; I added
     (?\: . avy-action-comment-defun-stay) ; I added
     (?x . avy-action-kill-move)
     (?X . avy-action-kill-stay)
     (?g . avy-action-teleport)
     (?m . avy-action-mark)
     (?c . avy-action-copy)
     (?y . avy-action-yank)
     (?Y . avy-action-yank-line)
     (?z . avy-action-zap-to-char)))
  :custom-face (avy-background-face ((t :inherit shadow))) ; Set background color for selecting
  :bind
  (("H-i" . avy-goto-char-2) ; C-i
   :map isearch-mode-map ("H-i" . avy-isearch)))


;; ---------- Terminal Emulator ---------- ;;

(use-package vterm
  :commands vterm-mode
  :bind
  (("s-T" . vterm)
   ("s-t" . vterm-other-window)
   (:map vterm-mode-map ("C-y" . vterm-yank)))
  :custom
  (vterm-clear-scrollback-when-clearing t) ; Completely clear vterm on C-l 
  (vterm-min-window-width 60))


;; ---------- LaTeX ---------- ;;

;; More features than built-in tex-mode
(use-package auctex
  :commands LaTeX-mode
  :config
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  :custom
  (TeX-view-program-selection '((output-pdf "PDF Tools"))) ; View in pdf-tools
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))) ; Sync
  (TeX-source-correlate-start-server t) 
  (TeX-save-query nil) ; Don't save query when running command list
  (TeX-electric-math '("\\(" . "\\)")) ; Insert '\(\)' for '$$' automatically
  (LaTeX-electric-left-right-brace t)) ; Balance braces automatically

;; Treat environments, delimiters as balanced expressions for navigation
;; FANTASTIC
(use-package tex-parens
  :hook LaTeX-mode
  :bind (:map TeX-mode-map ("M-u" . tex-parens-up-list)))

;; Pdf viewer
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (add-to-list 'pdf-tools-enabled-modes 'pdf-view-themed-minor-mode)
  :custom
  (pdf-view-resize-factor 1.1)
  (pdf-view-display-size 'fit-page))

(use-package saveplace-pdf-view
  :after (:any doc-view pdf-tools)
  :bind (:map pdf-view-mode-map ("s a" . save-place-mode))
  :demand t)


;; ---------- Programming modes ---------- ;;

;; LSP
(use-package eglot
  :hook ((scala-ts-mode haskell-ts-mode) . eglot-ensure)
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
;; Recompile if reinstalling agda-mode
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


;; ---------- Custom ---------- ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("2d74de1cc32d00b20b347f2d0037b945a4158004f99877630afc034a674e3ab7"
     default))
 '(mood-line-format
   '((" " (mood-line-segment-modal) " "
      (or (mood-line-segment-buffer-status) " ") " "
      (mood-line-segment-buffer-name) "  " (mood-line-segment-anzu)
      "  " (mood-line-segment-multiple-cursors) "  ")
     ((mood-line-segment-vc) "  " (mood-line-segment-major-mode) "  "
      (mood-line-segment-misc-info) "  " (mood-line-segment-checker)
      "  " (mood-line-segment-process) " "
      (format winum-format (winum-get-number-string)))))
 '(package-selected-packages
   '(auctex avy consult corfu dracula-theme haskell-ts-mode
            hide-mode-line magit marginalia mood-line
            nerd-icons-completion nerd-icons-dired orderless org-roam
            pdf-tools saveplace-pdf-view sbt-mode scala-repl
            scala-ts-mode solaire spacious-mode spacious-padding
            tex-parens vertico vertico-posframe vertico-quick vterm
            winum))
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
















































