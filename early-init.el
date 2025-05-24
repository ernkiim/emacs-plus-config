;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Increase thresholds, reset at end of init.el
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.8
      read-process-output-max (* 1024 1024))

;; Initial appearance
(setq-default ns-use-proxy-icon nil
              tool-bar-mode nil
              frame-title-format "Emacs\n"
              frame-resize-pixelwise t
              default-frame-alist
              '((height . 0.45)
                (width . 0.4)
                (left . 0.455)
                (top . 0.25)
                (vertical-scroll-bars . nil)
                (horizontal-scroll-bars . nil)
                (tool-bar-lines . 0)
                (menu-bar-lines . 0)
                (ns-appearance . dark)
                (ns-transparent-titlebar . t)))

;; end
(provide 'early-init)
