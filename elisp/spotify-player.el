;;; spotify-player.el --- cli integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; spotify_player cli vterm integration


(define-minor-mode no-q-mode
  "unbind 'q' to prevent accidental quit"
  :keymap '(("q" . (lambda () (interactive)))))

(defun spotify-init ()
  "Start spotify_player in dedicated vterm"
  (interactive)
  (save-current-buffer
    (set-buffer (get-buffer-create "spotify"))
    (vterm-mode)
    (vterm-send-string "spotify_player")
    (vterm-send-return)
    (no-q-mode)))

(defun spotify ()
  "Create and/or switch to spotify_player buffer"
  (interactive)
  (when (not (get-buffer "spotify"))
    (spotify-init)
    (message "connecting ...")
    (sleep-for 1.5)
    (message ""))
  (switch-to-buffer "spotify"))

(defun spotify-pause-resume ()
  "send 'space' char to spotify buffer if it exists"
  (interactive)
  (save-current-buffer
    (set-buffer "spotify")
    (vterm-send-space)))

(defun spotify-next ()
  "send 'n' char to spotify buffer if it exists"
  (interactive)
  (save-current-buffer
    (set-buffer "spotify")
    (vterm-send "n")))

(defun spotify-prev ()
  "send 'p' cnar to spotify buffer if it exists"
  (interactive)
  (save-current-buffer
    (set-buffer "spotify")
    (vterm-send "p")))


(provide 'spotify-player)
