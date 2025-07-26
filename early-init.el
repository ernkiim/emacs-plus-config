;;; early-init.el --- Loads before UI elements -*- lexical-binding: t; -*-

(defun setq-until (var val hook &optional default)
  "Set VAR equal to VAL temporarily. Use setq-default if DEFAULT
Reset VAR to its value at time of calling on HOOK"
  (let ((prev (eval var)))
    (defun reset ()
      (if default
	  (setq-default var val)
	(setq var val))
      (remove-hook hook 'reset)))
  (setq var val)
  (add-hook hook 'reset))

(defun setq-until-startup (var val)
  (setq-until var val 'emacs-startup-hook t))

;; Debug
(setq debug-on-error init-file-debug)

;;; Performance

;; Defer garbage collection
(setq-until-startup 'gc-cons-threshold most-positive-fixnum)
(setq-until-startup 'gc-cons-percentage 0.6)

;; Disable special file handlers during initialization
(setq-until-startup 'file-name-handler-alist nil)

;; Disable mode line (mood-line will replace it)
(setq-default mode-line-format nil)

;; Increase chunk size
(setq read-process-output-max (* 2 1024 1024))

;; Prefer newer compiled files
(setq load-prefer-newer t)

;; Native compile
(if (and (featurep 'native-compile)
	 (fboundp 'native-comp-available-p)
	 (native-comp-available-p))
    (setq native-comp-deferred-compilation t
	  native-comp-jit-compilation t
	  ;; package-native-compile t
	  )
  (setq features (delq 'native-compile features)))

;; NON-daemon optimizations
(when (and (not (daemonp)) (not noninteractive))
  ;; Debug
  (when (not init-file-debug)
    ;; Don't redraw until something actually happens
    (setq-until 'inhibit-redisplay t 'post-command-hook t)

    ;; Don't display messages during startup
    (setq-until 'inhibit-message t 'post-command-hook t))

  ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  ;; No second pass of case-insensitive search over auto-mode-alist.
  (setq auto-mode-case-fold nil)

  ;; Disable bidirectional text scanning for a modest performance boost.
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; Give up some bidirectional functionality for slightly faster re-display.
  (setq bidi-inhibit-bpa t)

  ;; Remove "For information about GNU Emacs..." message at startup
  (advice-add 'display-startup-echo-area-message :override #'ignore)

  ;; Suppress the vanilla startup screen completely. We've disabled it with
  ;; `inhibit-startup-screen', but it would still initialize anyway.
  (advice-add 'display-startup-screen :override #'ignore)

  ;; The initial buffer is created during startup even in non-interactive
  ;; sessions, and its major mode is fully initialized. Modes like `text-mode',
  ;; `org-mode', or even the default `lisp-interaction-mode' load extra packages
  ;; and run hooks, which can slow down startup.
  ;;
  ;; Using `fundamental-mode' for the initial buffer to avoid unnecessary
  ;; startup overhead.
  (setq initial-major-mode 'fundamental-mode
        initial-scratch-message nil)

  (unless init-file-debug
    ;; Unset command line options irrelevant to the current OS. These options
    ;; are still processed by `command-line-1` but have no effect.
    (unless (eq system-type 'darwin)
      (setq command-line-ns-option-alist nil))
    (unless (memq initial-window-system '(x pgtk))
      (setq command-line-x-option-alist nil))))


;;; Suppress warnings/messages

;; Suppress compile warnings
(setq native-comp-warning-on-missing-source init-file-debug
      native-comp-async-report-warnings-errors (or init-file-debug 'silent)
      native-comp-verbose (if init-file-debug 1 0)
      jka-comp-verbose init-file-debug
      byte-compile-warnings init-file-debug
      byte-compile-verbose init-file-debug)

;; Suppress warnings
(setq warning-minimum-level (if init-file-debug :warning :error))


;;; UI

;; Auto focus new frames
(add-hook 'after-make-frame-functions 'select-frame-set-input-focus)

;; Resizing the Emacs frame can be costly when changing the font. Disable this
;; to improve startup times with fonts larger than the system default.
(setq frame-resize-pixelwise t)

;; Without this, Emacs will try to resize itself to a specific column size
(setq frame-inhibit-implied-resize t)

;; Default frame
(setq-default default-frame-alist '(;; Frame position
				    (height . 0.45)
				    (width  . 0.4)
				    (left   . 0.5)
				    (top    . 0.3)
				    ;; Menu bar
				    (menu-bar-lines . 0)
				    ;; Font
				    (font . "Liga SFMono Nerd Font-15")))

;; Frame title
(setq frame-title-format "Emacs\n")

;; Reduce noise at startup. An empty scratch buffer (or the
;; dashboard) is more than enough, and faster to display.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message nil
      initial-buffer-choice nil
      inhibit-startup-buffer-menu t
      inhibit-x-resources t
      initial-scratch-message nil
      server-client-instructions nil)


;; Disable GUI
(setq use-file-dialog nil
      use-dialog-box nil)

;; Disable tooltips
(tooltip-mode -1)

;; Disable scroll bars
(setq default-frame-scroll-bars 'right)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)

;; Disable tool bar
(push '(tool-bar-lines . 0) default-frame-alist)
(setq tool-bar-mode nil)

;; MacOS specific UI
(when (eq system-type 'darwin)
  ;; Transparent titlebar
  (push '(ns-appearance . light) default-frame-alist)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  ;; No icon in titlebar
  (setq ns-use-proxy-icon nil))

;;; Misc.

;; Language environment
(set-language-environment "UTF-8")
(setq default-input-method nil)

;; Don't load packages before init
(setq package-enable-at-startup nil)

;; Security
(setq gnutls-verify-error t  ; Prompts user if there are certificate issues
      tls-checktrust t  ; Ensure SSL/TLS connections undergo trust verification
      gnutls-min-prime-bits 3072)  ; Stronger GnuTLS encryption


(provide 'early-init)

;;; early-init.el ends here
