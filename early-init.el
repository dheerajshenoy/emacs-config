;; -*- coding: utf-8; lexical-binding: t -*-

(setopt package-enable-at-startup nil)

(add-to-list 'load-path (expand-file-name "gcmh" user-emacs-directory))
(require 'gcmh)
(gcmh-mode 1)

;; (add-to-list 'default-frame-alist '(background-color . "#000000"))

(set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
(set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff" :box 'unspecified)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(undecorated . t) default-frame-alist)
(tooltip-mode -1)

(setopt inhibit-startup-message   t
        frame-resize-pixelwise    t  ; fine resize
        package-native-compile    t) ; native compile packages

;; Increase Read/Process Performance
(setq read-process-output-max (* 1024 1024))  ;; 1MB

;; Unset file-name-handler-alist
;; During startup, Emacs doesn't require specific file handlers for every file it opens or loads; thus, we should unset this list to optimize the startup process.

(defvar dj-emacs--vc-handled-backends vc-handled-backends)
(defvar dj-emacs--file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist dj-emacs--file-name-handler-alist
                  vc-handled-backends dj-emacs--vc-handled-backends)))

(set-fringe-mode 10)
(set-face-attribute 'default nil
                    :family "JetBrainsMono Nerd Font Mono"
                    :height 130)

(add-to-list 'default-frame-alist '(undecorated . t))
;; TODO: Fix for dashboard images (add-to-list 'default-frame-alist '(alpha-background . 80))
