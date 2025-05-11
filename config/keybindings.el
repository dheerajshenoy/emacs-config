;; -*- lexical-binding: t; -*-

;;; Text Scale

(use-package emacs
  :ensure nil
  :demand t
  :bind
  (:map global-map
        ("C-=" . text-scale-increase)
        ("C--" . text-scale-decrease)
        ("C-<wheel-up>" . text-scale-increase)
        ("C-<wheel-down>" . text-scale-decrease)

;;; Misc
        ("C-x C-c" . nil)
        ("C-x C-z" . nil)
        ("C-x C-r" . recentf-open)
        ("C-x C-l" . visual-line-mode)
        ("C-x C-b" . ibuffer)
        ("C-h h" . nil)
        ("C-z" . goto-last-change)
        ("C-S-z" . goto-last-change-reverse)
        ("M-o" . other-window)
        ("C-<backspace>" . dj/backward-kill-word)
        ("C-h K" . describe-keymap)
        ("C-h F" . apropos-function)
        ("C-h V" . apropos-variable)
        ("C-h L" . apropos-library)

        ;; Avy

        ("C-:" . avy-goto-char)
        ("M-g l" . avy-goto-line)
        ("M-g w" . avy-goto-word-1)

        ;; Eshell
        ("C-c e" . eshell)
  ))

(provide 'keybindings)
