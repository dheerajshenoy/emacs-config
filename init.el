;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "custom-elisp" user-emacs-directory))

(require 'dj-bytecompile)
(require 'dj-python)
(require 'dj-misc)
(require 'startup)
(require 'packages)
(require 'options)
(require 'keybindings)
(require 'modeline)
(require 'feeds)
