;;; Init.el --- Initialization file for Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Ern Kim's Emacs startup file

;;; Code:

;; ---------- package, use-package ---------- ;;

(require 'package)
(add-to-list 'package-archives '("gnu"    . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/elisp")
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure nil ; Set to nil normally
        use-package-expand-minimally t
        use-package-compute-statistics t))

(if (daemonp)
    (setq use-package-always-demand t))

;; ---------- Preferences ---------- ;;

;; Misc. preferences
(use-package emacs
  :config
  ;; Theme
  (use-package dracula-theme)
  ;; Intercept startup message
  (defun display-startup-echo-area-message ()
    (message ""))

  (defun delete-visited-file (buffer-name)
  "Delete the file visited by the buffer named BUFFER-NAME."
  (interactive "bDelete file visited by buffer ")
  (let* ((buffer (get-buffer buffer-name))
         (filename (buffer-file-name buffer)))
    (when buffer
      (when (and filename
                 (file-exists-p filename))
        (delete-file filename))
      (kill-buffer buffer))))
  
  ;; Delete files into trash bin (when using dired, etc.)
  (setq delete-by-moving-to-trash t)
  (when (eq system-type 'darwin)
    (setq trash-directory "~/.Trash"))
  :bind
  (("M-o" . other-window)
   ("M-u" . up-list)
   ("C-<wheel-up>"   . nil)  ; Momentum can trigger scroll wheel bindings
   ("C-<wheel-down>" . nil)
   ("C-." . scroll-up-line)
   ("C-," . scroll-down-line))

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
  ((after-init . pixel-scroll-precision-mode)
   (after-init . (lambda ()
                   "intercept C-i, decode to H-i instead of TAB"
                   (define-key input-decode-map
                               (kbd "C-i")
                               (kbd "H-i"))))))

;; Spotify cli integration
(use-package spotify-player
  :ensure nil
  :hook
  (after-init . spotify-init)
  :bind
  (("C-c C-s"   . spotify)
   ("C-c C-SPC" . spotify-pause-resume)
   ("C-c C-n"   . spotify-next)
   ("C-c C-p"   . spotify-prev)))


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
(use-package elec-pair
  :hook (after-init . electric-pair-mode))

;; Much faster than doom-modeline
(use-package mood-line                  
  :hook after-init
  :custom
  
  (mood-line-glyph-alist mood-line-glyphs-fira-code))


;; ---------- Org ---------- ;;

(use-package org
  :custom
  ;; Let RET act on links
  (org-return-follows-link t)
  ;; Need to set LaTeX preview generation program manually
  (org-latex-create-formula-image-program 'dvisvgm)
  :hook
  ;; IDE-like smart indentation
  (org-mode . org-indent-mode)
  ;; Wrap lines preserving words, let line-sensitive commands work visually
  (org-mode . visual-line-mode)
  ;; Displays LaTeX math symbols using unicode
  (org-mode . org-toggle-pretty-entities))

(use-package org-roam
  :config
  ;; Update the database automatically
  (org-roam-db-autosync-mode)
  
  (defun org-roam-tag-toggle (tag)
    "Toggle TAG on the node at point"
    (interactive
     (list (let ((crm-separator "[ 	]*:[ 	]*"))
             (completing-read "Tag: " (org-roam-tag-completions)))))
    (let ((current-tags (split-string (or (cadr (assoc "FILETAGS"
                                                       (org-collect-keywords '("filetags"))))
                                          "")
                                      ":" 'omit-nulls)))
      (if (member tag current-tags)
          (org-roam-tag-remove (list tag))
        (org-roam-tag-add (list tag)))))
  :custom
  ;; Set the directory for all roam nodes (default "~/org-roam")
  (org-roam-directory (file-truename "~/org-roam"))
  ;; Set the subdirectory for daily captures
  (org-roam-dailies-directory "daily/") ; default
  ;; Set the template for daily captures 
  (org-roam-dailies-capture-templates ; default
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))))
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n r" . org-roam-node-random)
   ("C-c n c" . org-roam-dailies-capture-today)
   ("C-c n d" . org-roam-dailies-goto-today)
   ("C-c n u" . org-roam-ui-open)
   :map org-mode-map
   ("C-c n t" . org-roam-tag-toggle)
   ("C-c n l" . org-roam-buffer-toggle)))

(use-package org-roam-agenda
  :ensure nil
  :hook
  ;; Add and remove 'todo' filetag automatically when saving
  (before-save . org-roam-agenda-update-todo-tag)
  :bind
  (("C-c a" . org-roam-agenda-open)
   ("C-c t" . org-roam-agenda-todo-list)))

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
  :config
  ;; Use child clients for commit messages
  (use-package with-editor)
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
  :bind (:map dired-mode-map ("^" . dired-up-directory)))

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
  :hook
  (after-init . global-corfu-mode)
  (corfu-mode . corfu-history-mode)
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
  :bind (:map corfu-map ("M-SPC" . corfu-insert-separator)))


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
    (avy-mk-stay-action pt
                        (lambda ()
                          (mark-defun)
                          (comment-region (region-beginning) (region-end)))))

  (defun avy-action-kill-whole-line (pt)
    "Kill text"
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))
  :custom
  ;; Use colemak home row
  (avy-keys homerow)
  ;; Set all non-matched text to background color when selecting
  (avy-background t)
  ;; Allow action on single match
  (avy-single-candidate-jump nil)
  ;; Bindings
  (avy-dispatch-alist
   '((?\; . avy-action-comment-line-stay)  
     (?\: . avy-action-comment-defun-stay)
     (?k . avy-action-kill-stay)
     (?K . avy-action-kill-whole-line)
     (?w . avy-action-copy)
     (?W . avy-action-copy-whole-line)
     (?y . avy-action-yank)
     (?Y . avy-action-yank-whole-line)
     (?p . avy-action-teleport)
     (?P . avy-action-teleport-whole-line)
     (?z . avy-action-zap-to-char)
     (?m . avy-action-mark)
     (? . avy-action-mark-to-char)))
  :custom-face (avy-background-face ((t :inherit shadow))) ; Set background color for selecting
  :bind
  (("H-i" . avy-goto-char-2) ; C-i
   :map isearch-mode-map
   ("H-i" . avy-isearch)))


;; ---------- Terminal Emulator ---------- ;;

(use-package vterm
  :commands vterm-mode
  :bind
  (("s-T" . vterm)
   ("s-t" . vterm-other-window)
   :map vterm-mode-map
   ("C-y" . vterm-yank))
  :custom
  (vterm-clear-scrollback-when-clearing t) ; Completely clear vterm on C-l 
  (vterm-min-window-width 60))


;; ---------- LaTeX ---------- ;;

;; More features than built-in tex-mode
(use-package auctex
  :commands LaTeX-mode
  :hook
  ;; Revert PDF viewing buffer every time we recompile
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
  ;; Org-like heading-aware folding and navigation
  (LaTeX-mode . outline-minor-mode)
  ;; Unicode display of symbols
  (LaTex-mode . prettify-symbols-mode)
  :config
  ;; Treat environments, delimiters as balanced expressions for navigation, FANTASTIC
  (use-package tex-parens
    :hook LaTeX-mode)
  :custom
  (TeX-view-program-selection '((output-pdf "PDF Tools"))) ; View in pdf-tools
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))) ; Sync
  (TeX-source-correlate-start-server t) 
  (TeX-save-query nil) ; Don't save query when running command list
  (TeX-electric-math '("\\(" . "\\)")) ; Insert '\(\)' for '$$' automatically
  (LaTeX-electric-left-right-brace t))



;; Pdf viewer
(use-package pdf-tools
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (add-to-list 'pdf-tools-enabled-modes 'pdf-view-themed-minor-mode)
  :custom
  (pdf-view-resize-factor 1.1)
  (pdf-view-display-size 'fit-page))


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

(use-package consult-hoogle
  :after haskell-ts-mode)

;; Agda 2.8.0
;; Recompile if reinstalling agda-mode
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda --emacs-mode locate")))
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
      "  " (mood-line-segment-multiple-cursors) "  "
      (format-mode-line "%l:%c" 'mood-line-unimportant) " ")
     ((mood-line-segment-vc) "  " (mood-line-segment-major-mode) "  "
      (mood-line-segment-misc-info) "  " (mood-line-segment-checker)
      "  " (mood-line-segment-process) "  "
      (format winum-format (winum-get-number-string)))))
 '(org-preview-latex-default-process 'dvisvgm nil nil "Customized with use-package org")
 '(package-selected-packages
   '(auctex avy consult consult-hoogle corfu dracula-theme esup
            haskell-ts-mode hide-mode-line magit marginalia mood-line
            nerd-icons-completion nerd-icons-dired orderless org-roam
            org-roam-ui pdf-tools saveplace-pdf-view sbt-mode
            scala-repl scala-ts-mode solaire spacious-mode
            spacious-padding tex-parens vertico vertico-posframe
            vertico-quick vterm winum))
 '(tool-bar-mode nil))

;; ---------- End ---------- ;;

(setq gc-cons-threshold (* 1 1024 1024)
      gc-cons-percentage 0.5)

(provide 'init)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
