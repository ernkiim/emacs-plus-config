;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Performance
(setq inhibit-compacting-font-caches t
      gc-cons-threshold (* 100 1024 1024) ; Increase thresholds, reset at end of init.el
      gc-cons-percentage 0.8
      read-process-output-max (* 1024 1024))
                    
;; Default appearance
(setq-default package-native-compile t
              ns-use-proxy-icon nil
              tool-bar-mode nil
              frame-title-format "Emacs\n"
              frame-resize-pixelwise t
              frame-inhibit-implied-resize nil
              default-frame-alist
              '((height . 0.45)
                (width . 0.4)
                (left . 0.5)
                (top . 0.3)
                (vertical-scroll-bars . nil)
                (horizontal-scroll-bars . nil)
                (menu-bar-lines . 0)
                (font . "Meslo LG S-15")))

;; MacOS specific appearance
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-appearance . 'dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Abnormal hook, passes created frame as argument
;; Adding in early-init so that the functions run on initial frame
(add-hook 'after-make-frame-functions 'select-frame-set-input-focus)

;; end
(provide 'early-init)
