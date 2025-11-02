;;; init.el --- Initialization file for Emacs -*- lexical-binding: t; -*-

;; TODO: Lazytab

;;; Bootstrapping

;; Straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	(or (bound-and-true-p straight-base-dir)
	    user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use-package configuration macro
(straight-use-package 'use-package)

;; Configure use-package
(use-package use-package
  :straight nil
  :custom
  ;; Debug
  (use-package-verbose init-file-debug)
  (use-package-compute-statistics init-file-debug)
  ;; Always try to install
  (straight-use-package-by-default t)
  ;; Daemon eagerly loads packages, defer otherwise
  (use-package-always-defer (not (daemonp)))
  (use-package-always-demand (daemonp))
  ;; Let imenu see use-package forms
  (use-package-enable-imenu-support t))


;;; General configuration

;; Misc. options
(use-package emacs
  :straight (:type built-in)
  :bind (("C-M-y" . up-list)
	 ("C-M-w" . delete-pair)
	 ("M-o" . other-window)
	 ("C-<wheel-up>" . nil) ; Smooth scroll can trigger these bindings
	 ("C-<wheel-down>" . nil))
  :config
  ;; Separate 'C-i' from 'TAB' binding by sending 'C-i' to 'H-i'
  (define-key input-decode-map
	      (kbd "C-i")
	      (kbd "H-i"))
  ;; Store custom-set-variables in separate file
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file 'noerror t)
  :custom
  ;; Thin bar cursor
  (cursor-type 'bar)
  ;; Make cursor flash instead of blinking
  (blink-cursor-blinks 1)
  (blink-cursor-alist '((bar . box)))
  ;; Answer y/n insteand of yes/no
  (use-short-answers t)
  ;; Echo keystrokes fast
  (echo-keystrokes 0.01)
  ;; Don't show C-h reminder text
  (echo-keystrokes-help nil)
  ;; Minibuffers when another minibuffer is already open
  (enable-recursive-minibuffers t)
  ;; Undo/redo limits
  (undo-limit (* 13 160000))
  (undo-strong-limit (* 13 240000))
  (undo-outer-limit (* 13 24000000))
  ;; Disable bell
  (visible-bell nil)
  (ring-bell-function 'ignore)
  ;; Position underlines at the descent line instead of the base line
  (x-underline-at-descent-line t)
  ;; Use system trash
  (delete-by-moving-to-trash t)
  ;; Disable truncation of printed s-expressions in the message buffer
  (eval-expression-print-length nil)
  (eval-expression-print-level nil)
  ;; Prevent crashes
  (window-resize-pixelwise nil)
  ;; Prevent lag
  (redisplay-skip-fontification-on-input t)
  ;; No line wrap
  (truncate-lines t)
  ;; Make sentences work normally
  (sentence-end-double-space nil)
  ;; Smooth scrolling
  (pixel-scroll-precision-mode t)
  ;; Horizontal scrolling
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-flip-direction t)
  (hscroll-margin 2)
  (hscroll-step 1)
  ;; Keep the cursor out of the read-only portions of the minibuffer
  (minibuffer-prompt-properties '(read-only t
				  intangible t
				  cursor-intangible t
				  face minibuffer-prompt))
  :hook (;; Keep the cursor out of the read-only portions of the minibuffer
	 (minibuffer-setup . cursor-intangible-mode)
	 ;; Separate C-i from TAB binding (have to redefine for each client)
	 ((after-init server-after-make-frame) . (lambda ()
						   (define-key input-decode-map
							       (kbd "C-i")
							       (kbd "H-i"))))))

;; File system
(use-package files
  :straight (:type built-in)
  :custom
  ;; Actually delete remote files
  (remote-file-name-inhibit-delete-by-moving-to-trash t)
  ;; Can ignore this warning
  (find-file-suppress-same-file-warnings t)
  ;; Resolve symlinks
  (find-file-visit-truename t)
  (vc-follow-symlinks t))


;;; Appearance

;; Theme
(use-package gruvbox-theme
  :demand t
  :custom (custom-enabled-themes '(gruvbox-dark-medium)))

;; Minimal mode line
(use-package mood-line
  :init (mood-line-mode +1)
  :custom-face
  ;; mood-line-unimportant is hard to see with gruvbox
  (mood-line-unimportant ((t (:inherit background))))
  :custom
  ;; Pretty symbols
  (mood-line-glyph-alist mood-line-glyphs-fira-code)
  ;; Format and shown items
  (mood-line-format
   (mood-line-defformat
    :left
    (((or (mood-line-segment-buffer-status) " ") . " ")
     ((mood-line-segment-buffer-name)            . "  ")
     ((propertize (mood-line-segment-cursor-position)
		  'face
		  '(:inherit mood-line-unimportant))
      . "  "))
    :right
    (((mood-line-segment-misc-info)  . "  ")
     ((mood-line-segment-vc)         . "  ")
     ((mood-line-segment-major-mode) . "  ")
     ((mood-line-segment-checker)    . "  ")
     (propertize (winum-get-number-string)
		 'face
		 '(:inherit mood-line-unimportant))))))


;;; UX

;; Delimiter highlighting
(use-package paren
  :straight (:type built-in)
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-context-when-offscreen 'overlay) ; Emacs >= 29
  (show-paren-when-point-in-periphery t))

;; Delimiter insertion
(use-package elec-pair
  :straight (:type built-in)
  :init (electric-pair-mode +1)
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Save position in files
(use-package saveplace
  :straight (:type built-in)
  :init (save-place-mode +1)
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-limit 600))

;; Save history
(use-package savehist
  :straight (:type built-in)
  :init (savehist-mode +1)
  :custom
  ;; Increase saved history
  (history-length 300)
  (savehist-additional-variables '(kill-ring                         ; clipboard
				   register-alist                    ; macros
				   mark-ring global-mark-ring        ; marks
				   search-ring regexp-search-ring))) ; searches

;; Auto revert
(use-package autorevert
  :straight (:type built-in)
  :init (global-auto-revert-mode +1)
  :custom
  ;; No revert query
  (revert-without-query (list "."))
  ;; Don't stop
  (auto-revert-stop-on-user-input nil)
  ;; Print message on revert
  (auto-revert-verbose t)
  ;; Revert other buffers (e.g. Dired)
  (global-auto-revert-non-file-buffers t)
  (global-auto-revert-ignore-modes '(Buffer-menu-mode)))

;; Window switching
(use-package winum
  :init (winum-mode +1)
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
   ("s-9" . winum-select-window-9))
  :custom
  ;; Insert window number manually in mood-line
  (winum-auto-setup-mode-line nil)
  ;; Minibuffer gets number '0'
  (winum-auto-assign-0-to-minibuffer t))

;; Better undo
(use-package undo-fu
  :bind (("s-z" . undo-fu-only-undo)
	 ("s-Z" . undo-fu-only-redo)))

;; Fast navigation and shortcut actions
;; TODO: Better actions
(use-package avy
  :bind (("H-i" . avy-goto-char-2) ; C-i
	 :map isearch-mode-map
	 ("H-i" . avy-isearch))
  :custom-face (avy-background-face ((t :inherit shadow)))
  :custom
  ;; Use colemak home row
  (avy-keys '(?n ?t ?e ?s ?i ?r ?o ?a))
  ;; Dim other text when selecting match
  (avy-background t)
  ;; Allow shortcuts on single match
  (avy-single-candidate-jump nil)
  :config
  ;; Let closer matches be shallower in selection tree
  (add-to-list 'avy-orders-alist '(avy-goto-char-2 . avy-order-closest)))


;;; Vert&co

;; List completing-read completions vertically
(use-package vertico
  :init (vertico-mode +1)
  :config
  ;; Show completions in centered frame
  (straight-use-package 'vertico-posframe)
  (vertico-posframe-mode +1)
  :custom
  ;; Cycle through completions
  (vertico-cycle t))

;; Completing-read commands
(use-package consult
  :bind
  (("s-l"     . consult-goto-line)
   ("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-y"     . consult-yank-pop)
   ("s-f"     . consult-line)
   ("C-x b"   . consult-buffer)
   ("C-x C-b" . consult-buffer))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  (use-package consult-todo
    :bind ("s-d" . consult-todo)))

;; Order-insensitive keyword search
(use-package orderless
  :custom
  (completion-styles '(orderless
		       basic
		       substring
		       initials
		       flex))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Annotations in minibuffer
(use-package marginalia
  :init (marginalia-mode +1))


;;; IDE features

;; Compilation
(use-package compile
  :straight (:type built-in)
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error))

;; Interpreters in buffer
(use-package comint
  :straight (:type built-in)
  :custom
  (comint-prompt-read-only t)
  (comint-buffer-maximum-size 2048))

;; Version control integration
(use-package vc-git
  :straight (:type built-in)
  :custom
  (vc-git-print-log-follow t)
  (vc-make-backup-files nil)
  (vc-git-diff-switches '("--histogram")))

;; Git interface
(use-package magit
  :config
  ;; Use emacs client to edit commit messages
  (straight-use-package 'with-editor)
  ;; Show todo items in magit buffer
  (use-package magit-todos
    :config (magit-todos-mode +1)
    :custom
    (magit-todos-branch-list nil))
  :custom
  ;; Hardcode git path
  (magit-git-executable "/opt/homebrew/bin/git")
  ;; Add magit-status, -dispatch, and -file-dispatch actions to global
  (magit-maybe-define-global-key-bindings recommended)
  ;; Don't refresh status buffer, only current buffer
  (magit-refresh-status-buffer nil)
  ;; Use system trash for discard and delete
  (magit-delete-by-moving-to-trash t)
  ;; Disable confirmation for some actions
  (magit-no-confirm '(set-and-push
		      discard ; Careful if not delete-by-moving-to-trash
		      trash)))

;; Fast terminal emulator
(use-package vterm
  :bind (("s-T" . vterm)
	 ("s-t" . vterm-other-window)
	 :map vterm-mode-map
	 ("C-y" . vterm-yank))
  :custom
  ;; Completely clear terminal on 'C-l'
  (vterm-clear-scrollback-when-clearing t))


;;; Language major modes


;; Markdown
(use-package markdown-ts-mode
  :straight (:type built-in)
  :hook (markdown-ts-mode . visual-line-mode))

(use-package org
  :straight (:type built-in)
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . org-toggle-pretty-entities)
   (org-mode . org-cdlatex-mode)))

(use-package org-roam)


;; Agda 2.8.0
(use-package agda2
  :straight nil
  :load-path
  (lambda ()
    (file-name-directory (shell-command-to-string "agda --emacs-mode locate")))
  :custom
  ;; Highlight the expression being type-checked
  (agda2-highlight-level 'interactive))

;; Lean4
(use-package nael
  :straight (nael :repo "https://codeberg.org/mekeor/nael"
		  :files ("nael/" :defaults))
  :hook ((nael-mode . abbrev-mode)
	 (nael-mode . eglot-ensure)))

;; LaTeX
(use-package auctex
  :hook ((LaTeX-mode . outline-minor-mode)
	 (LaTeX-mode . visual-line-mode))
  :custom
  ;; Save style information when saving buffer
  (TeX-auto-save t)
  ;; Parse file after loading
  (TeX-parse-self t)
  ;; Use pdf-tools to view output pdf
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (TeX-source-correlate-start-server t)
  ;; No save query when running command list
  (TeX-save-query nil)
  :config
  (use-package latex
    :straight (:type built-in)
    :bind (:map LaTeX-mode-map ("C-M-w" . tex-parens-delete-pair)))

  ;; Fast LaTeX entry
  (use-package cdlatex
    :hook
    (LaTeX-mode
     (cdlatex-tab . LaTeX-indent-line))
    :straight (cdlatex :type git :host github :repo "cdominik/cdlatex"
		       ;; Use my fork
		       :fork (:host github
				    :repo "ernkiim/cdlatex"))

    :custom
    ;; Turn off labels
    (cdlatex-insert-auto-labels-in-env-templates nil)
    ;; Use electric pairs
    (cdlatex-takeover-parenthesis nil))

  ;; Navigate environments as balanced delims
  (use-package tex-parens
    :hook LaTeX-mode)

  ;; Revert pdf after compile
  (add-hook 'TeX-after-compilation-finished-functions
	    #'TeX-revert-document-buffer))



;; Haskell
(use-package haskell-mode)

;; Scala
(use-package scala-mode)

;;; Misc. major modes

;; Directory editing and navigation
(use-package dired
  :straight (:type built-in)
  :config (straight-use-package 'dired-x)
  :hook
  (;; Hide permissions vector, owner etc.
   (dired-mode . dired-hide-details-mode)
   ;; Hide unwanted file names
   (dired-mode . dired-omit-mode))
  :custom
  ;; Don't hide symlink targets
  (dired-hide-details-hide-symlink-targets nil)
  ;; Don't list '.', '..' (separate from omit)
  (dired-listing-switches "-l -A -h -v --group-directories-first")
  ;; Omit '.DS_STORE' and '.localized'
  (dired-omit-files (rx (or (seq bol ".DS_STORE" eol)
			    (seq bol ".localized" eol))))
  ;; Omit silently
  (dired-omit-verbose nil))

;; PDF interaction
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; Initialize
  (pdf-tools-install :no-query)
  ;; Save place in pdf buffer
  (straight-use-package 'saveplace-pdf-view))

;; Music/media
(use-package ready-player
  :config (ready-player-mode +1)
  :custom
  ;; Don't hide mode line
  (ready-player-hide-modeline nil)
  ;; Don't show sponsor message
  (ready-player-ask-for-project-sustainability nil))


;;; Cleanup

(provide 'init)

;; Local variables:
;; coding: utf-8
;; End:
;;; init.el ends here
