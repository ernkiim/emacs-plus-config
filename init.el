;;; init.el --- Initialization file for Emacs -*- lexical-binding: t; -*-

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

;; Straight.el bootstrap
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

;; Use-package
(straight-use-package 'use-package)

(use-package use-package
  :custom
  ;; Debug
  (use-package-verbose init-file-debug)
  (use-package-compute-statistics t); init-file-debug)
  ;; Always try to install
  (straight-use-package-by-default t)
  ;; Daemon eagerly loads packages, defer otherwise
  (use-package-always-defer (not (daemonp)))
  (use-package-always-demand (daemonp))
  ;; Let imenu see use-package forms
  (use-package-enable-imenu-support t))


;;; Built-in packages

;; Misc.
(use-package emacs
  :straight (:type built-in)
  :bind (("M-u" . up-list)
	 ("C-<wheel-up>" . nil) ; Smooth scroll can trigger these bindings
	 ("C-<wheel-down>" . nil))
  :init
  ;; Separate 'C-i' from 'TAB' binding by sending 'C-i' to 'H-i'
  (define-key input-decode-map
	      (kbd "C-i")
	      (kbd "H-i"))
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
  ;; Horizontal scrolling
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-flip-direction t)
  (hscroll-margin 2)
  (hscroll-step 1)
  ;; Enable nested minibuffers
  (enable-recursive-minibuffers t)
  ;; Keep the cursor out of the read-only portions of the minibuffer
  (minibuffer-prompt-properties '(read-only t
				  intangible t
				  cursor-intangible t
				  face minibuffer-prompt))
  :hook (;; Keep the cursor out of the read-only portions of the minibuffer
	 (minibuffer-setup . cursor-intangible-mode)))

;; Server-specific config
(use-package server
  :hook ((server-after-make-frame . (lambda ()
				      "input-decode-map is session-local; must redefine for each client"
				      (define-key input-decode-map
						  (kbd "C-i")
						  (kbd "H-i"))))))

;; Smooth scrolling
(use-package pixel-scroll
  :straight (:type built-in)
  :hook ((after-init . pixel-scroll-precision-mode)))

;; Frame display
(use-package frame
  :straight (:type built-in)
  :hook ((after-init . window-divider-mode))
  :custom
  (window-divider-default-bottom-width 1)
  (window-divider-default-places t)
  (window-divider-default-right-width 1))

;; Files
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

;; Compilation
(use-package compile
  :straight (:type built-in)
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error))

(use-package comp-run
  :straight (:type built-in)
  :custom
  ;; Ask the user whether to terminate asynchronous compilations on exit.
  ;; This prevents native compilation from leaving temporary files in /tmp.
  (native-comp-query-on-exit t))

;; Comint
(use-package comint
  :straight (:type built-in)
  :custom
  (comint-prompt-read-only t)
  (comint-buffer-maximum-size 2048))

;; Auto insert paired delims
(use-package elec-pair
  :straight (:type built-in)
  :hook ((prog-mode . electric-pair-mode)))

;; Delimiter highlighting
(use-package paren
  :straight (:type built-in)
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-context-when-offscreen 'overlay) ; Emacs >= 29
  (show-paren-when-point-in-periphery t))

;; Version control
(use-package vc-git
  :straight (:type built-in)
  :custom
  (vc-git-print-log-follow t)
  (vc-make-backup-files nil)
  (vc-git-diff-switches '("--histogram")))

;; Save position in files
(use-package saveplace
  :straight (:type built-in)
  :hook (after-init . save-place-mode)
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-limit 600))

;; Save history
(use-package savehist
  :straight (:type built-in)
  :hook after-init
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
  :hook (after-init . global-auto-revert-mode)
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


;;; Third-party packages

;; Window switching
(use-package winum
  :hook after-init
  :custom
  ;; Insert window number manually in mood-line
  (winum-auto-setup-mode-line nil))

;; Minimal mode line
(use-package mood-line
  :hook after-init
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
  ;; Minibuffer gets number '0'
  (winum-auto-assign-0-to-minibuffer t)
  ;; Pretty symbols
  (mood-line-glyph-alist mood-line-glyphs-fira-code)
  ;; Format
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

;; Hide mode line in some modes
(use-package hide-mode-line
  :hook
  (vterm-mode
   pdf-view-mode))

;; Magit git porcelain
(use-package magit
  :config (use-package with-editor)
  :custom
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

;; Snippets
(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :config
  (straight-use-package 'yasnippet-snippets)
  :custom
  ;; Indent first line of snippet
  (yas-also-auto-indent-first-line t)
  (yas-also-indent-empty-lines t)
  ;; Setting this to t causes issues with undo
  (yas-snippet-revival nil)
  ;; Do not wrap region when expanding snippets
  (yas-wrap-around-region nil))

;; LSP client
;; Fast third-party alternative to eglot
(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :hook prog-mode)

;; Corfu completion ui
(use-package corfu
  :hook ((prog-mode vterm-mode) . global-corfu-mode)
  :config
  (corfu-history-mode +1)
  :custom
  ;; Auto show completions
  (corfu-auto t)
  (corfu-auto-prefix 2)
  ;; Cycle through completions
  (corfu-cycle t)
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable ispell
  (text-mode-ispell-word-completion nil)
  :bind (:map corfu-map ("M-SPC" . corfu-insert-separator)))

;; Cape provides more capfs
(use-package cape
  :bind ("C-c p" . cape-prefix-map)
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; Whitespace removal
(use-package stripspace
  :hook ((prog-mode text-mode conf-mode) . stripspace-local-mode))

;; Better undo
(use-package undo-fu
  :bind (("s-z" . undo-fu-only-undo)
	 ("s-Z" . undo-fu-only-redo)))

;; Vert&co
(use-package vertico
  :hook after-init
  :config
  (straight-use-package 'vertico-posframe)
  (vertico-posframe-mode +1)
  :custom
  ;; Cycle through completions
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

(use-package marginalia
  :hook after-init
  :commands (marginalia-mode marginalia-cycle))

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

;; Terminal emulator
(use-package vterm
  :bind (("s-T" . vterm)
	 ("s-t" . vterm-other-window)
	 :map vterm-mode-map
	 ("C-y" . vterm-yank))
  :custom
  ;; Completely clear terminal on 'C-l'
  (vterm-clear-scrollback-when-clearing t))


;;; Programming modes

;; General treesitter modes
(use-package treesit-auto
  :custom
  ;; Prompt to install new treesitter grammar
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode +1))

;; Agda 2.8.0

;; (defun agda2-load-path ()
;;   (let ((coding-system-for-read 'utf-8))
;;     (file-name-directory (shell-command-to-string "agda --emacs-mode locate"))))

(use-package agda2
  :straight nil
  :load-path
  "/Users/e/.cabal/store/ghc-9.6.7/Agd-2.8.0-4e938804/share/emacs-mode/")


(provide 'init)

;;; init.el ends here
