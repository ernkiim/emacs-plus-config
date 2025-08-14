;;; init.el --- Initialization file for Emacs -*- lexical-binding: t; -*-

;; TODO: Lazytab
;; TODO: Use monospace font
;; TODO: Back to LSP-bridge

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
  ;; Answer y/n insteand of yes/no
  (use-short-answers t)
  ;; Echo keystrokes fast
  (echo-keystrokes 0.01)
  ;; Don't show C-h reminder text
  (echo-keystrokes-help nil)
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
  ;; Insert delims in pairs
  (electric-pair-mode t)
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
  :config (load-theme 'gruvbox))

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

;; Imenu
(use-package imenu
  :straight (:type built-in)
  :custom
  ;; Rescan when imenu invoked
  (imenu-auto-rescan t)
  ;; Prevent truncation of long function names in 'imenu' listings
  (imenu-max-item-length 160))

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

;; Whitespace removal
(use-package stripspace
  :hook ((prog-mode text-mode conf-mode) . stripspace-local-mode))

;; Better undo
(use-package undo-fu
  :bind (("s-z" . undo-fu-only-undo)
	 ("s-Z" . undo-fu-only-redo)))

;; Fast navigation and shortcut actions
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
  ;; Shortcuts
  (avy-dispatch-alist
   '((?\; . avy-action-comment-whole-line)
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
  :config
  ;; Let closer matches be shallower in selection tree
  (add-to-list 'avy-orders-alist '(avy-goto-char-2 . avy-order-closest))

  (defun avy-action-comment-whole-line (pt)
    "Comment item at pt"
    (save-excursion
      (goto-char pt)
      (comment-line 1))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

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
    (goto-char pt)))


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

;; Native LSP client
(use-package eglot
  :straight (:type built-in)
  :hook (((python-mode
	   python-ts-mode
	   haskell-mode
	   haskell-ts-mode)
	  . eglot-ensure))
  :custom
  ;; Improves performance
  (eglot-events-buffer-size 0)
  ;; Removes margin indications that shift line height
  (eglot-code-action-indications '(eldoc-hint))
  :config
  ;; Python
  (add-to-list 'eglot-server-programs
	       '((python-mode python-ts-mode) "basedpyright"))
  (add-to-list 'eglot-server-programs
	       '((haskell-mode haskell-ts-mode) "haskell-language-server-wrapper" "--lsp")))

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

;; Automatically install treesit modes
(use-package treesit-auto
  :custom
  ;; Prompt to install new grammar
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode +1))

;; Completion interface
(use-package corfu
  :init (global-corfu-mode +1)
  :config
  ;; Remember recent completions
  (corfu-history-mode +1)
  ;; Nerd icons
  (straight-use-package 'nerd-icons-corfu)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :custom
  ;; Auto show completions
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.2)
  ;; Cycle through completions
  (corfu-cycle t)
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable ispell
  (text-mode-ispell-word-completion nil)
  :bind (:map corfu-map ("M-SPC" . corfu-insert-separator)))

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

;; Agda 2.8.0
(use-package agda2
  :straight nil
  :load-path
  (lambda ()
    (file-name-directory (shell-command-to-string "agda --emacs-mode locate")))
  :custom
  ;; Highlight the expression being type-checked
  (agda2-highlight-level 'interactive))

;; LaTeX
(use-package auctex
  :hook
  (;; Org-like heading-aware folding and navigation
   (LaTeX-mode . outline-minor-mode)
   ;; In-line unicode preview symbol macros
   (LaTeX-mode . prettify-symbols-mode))
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

  ;; Navigate environments as balanced delims
  (use-package tex-parens
    :hook TeX-mode
    :bind (("C-M-w" . tex-parens-delete-pair)))

  ;; Fast LaTeX entry
  (use-package cdlatex
    :hook LaTeX-mode
    :straight (cdlatex :type git :host github :repo "cdominik/cdlatex"
		       ;; Use my fork
		       :fork (:host github
				    :repo "ernkiim/cdlatex"))
    :custom
    ;; Turn off labels
    (cdlatex-insert-auto-labels-in-env-templates nil))

  ;; Revert pdf after compile
  (add-hook 'TeX-after-compilation-finished-functions
	    #'TeX-revert-document-buffer))

;; Haskell
(use-package haskell-ts-mode
  :custom
  (haskell-ts-font-lock-level 4)
  (haskell-ts-use-indent t)
  (haskell-ts-ghci "ghci")
  (haskell-ts-use-indent t))



;;; Misc. major modes

;; Directory editing and navigation
(use-package dired
  :straight (:type built-in)
  :hook
  (;; Hide permissions vector, owner etc.
   (dired-mode . dired-hide-details-mode)
   ;; Hide unwanted file names
   (dired-mode . dired-omit-mode))
  :custom
  ;; Don't hide symlink targets
  (dired-hide-details-hide-symlink-targets nil)
  ;; Omit ., .. and .DS_Store
  (dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|\\.DS_Store"))

;; PDF interaction
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install :no-query))


;;; Cleanup

(provide 'init)

;; Local variables:
;; coding: utf-8
;; End:
;;; init.el ends here
