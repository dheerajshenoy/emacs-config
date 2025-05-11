;; -*- lexical-binding: t; -*-

;;; PACKAGES for dj's emacs

(require 'package)
(setq package-archives
      '(("MELPA"        . "https://melpa.org/packages/")
        ("GNU ELPA"     . "https://elpa.gnu.org/packages/")))
(package-initialize)
(setq use-package-always-ensure t)

;;;; Minions

(use-package minions
  :config
  (minions-mode))

;;;; Recentf

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 25)
  (recentf-save-file-modes nil)
  (recentf-keep nil)
  (recentf-initialize-file-name-history nil)
  (recentf-filename-handlers nil)
  (recentf-show-file-shortcuts-flag nil))

;;;; Manpages

(use-package man
  :ensure nil
  :commands (man)
  :config
  (setq Man-notify-method 'pushy)) ; does not obey `display-buffer-alist'

;;;; Proced

;;;; `proced' (process monitor, similar to `top')
(use-package proced
  :ensure nil
  :commands (proced)
  :config
  (setq proced-auto-update-flag 'visible) ; Emacs 30 supports more the `visible' value
  (setq proced-enable-color-flag t) ; Emacs 29
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user))

;;;; Savehist

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (history-length 100)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t))

;;;; Vertical completion layout (vertico)

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-scroll-margin 5)
  (setq vertico-count 10)
  (setq vertico-resize t)
  (setq vertico-cycle t)

  (with-eval-after-load 'rfn-eshadow
    ;; This works with `file-name-shadow-mode' enabled.  When you are in
    ;; a sub-directory and use, say, `find-file' to go to your home '~/'
    ;; or root '/' directory, Vertico will clear the old path to keep
    ;; only your current input.
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)))

;;;; Spacious Padding

(use-package spacious-padding
  :defer t)

;;;; Multiple Cursors

(use-package multiple-cursors
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;;;; Writeroom Mode

(use-package writeroom-mode
  :defer t
  :bind ("C-x C-w" . writeroom-mode))

;;;; prescient

(use-package prescient
  :defer t)

;;;; Detailed completion annotations (marginalia.el)

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0)) ; absolute time

;;;; Orderless

(use-package orderless
  :defer t
  :custom
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;;;; Doom Themes

(use-package doom-themes
  :defer t)

;;;; Ef Themes

(use-package ef-themes
  :defer t)

(load-theme 'ef-dark)
;;;; Org Bullets

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;;;; Corfu (in-buffer completion popup)

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-preview-current nil)
  (corfu-min-width 20)
  (corfu-quit-no-match nil)
  (corfu-separator ?\s)
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  :config
  (setq-default eglot-workspace-configuration
    '((:pylsp .
        ((useLibraryCodeForTypes . t)))))
  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;;;; Cape

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  :bind ("C-c p" . cape-prefix-map))

;;;; Embark

(use-package embark
  :bind (("C-." . embark-act)))

;;;; Treesitter

(require 'treesit)

(customize-set-variable 'treesit-font-lock-level 4)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        ;; (c-mode . c-ts-mode)
        (rust-mode . rust-ts-mode)
        ;; (c++-mode . c++-ts-mode)
        (css-mode . css-ts-mode)
        (lua-mode . lua-ts-mode)
        (elisp-mode . elisp-ts-mode)
        (go-mode . go-ts-mode)
        (html-mode . html-ts-mode)
        (js-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (conf-toml-mode . toml-ts-mode)
        (cmake-mode . cmake-ts-mode)
        (tsx-mode . tsx-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (yaml-mode . yaml-ts-mode)))

;;;; Dired

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("z" . dj/dired-xdg-open))
  :custom
  (dired-listing-switches "-lah")
  (dired-recursive-deleted 'always)
  (dired-recursive-copies 'always)
  (dired-dwim-target t)
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask)
  (dired-kill-when-opening-new-dired-buffer t)
  (auto-revert-verbose nil)
  (dired-create-destination-dirs-on-trailing-dirsep t))

;;;; Magit

(use-package magit
  :defer t
  :commands (magit-status))

;;;; Which Key

(use-package which-key
  :ensure nil
  :config
  (setopt which-key-idle-delay 0.2)
  (which-key-mode))

;;;; Yasnippets

(use-package yasnippet
  :defer t
  :config
  (yas-global-mode)
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :defer t
  :after (yasnippet))

;;;; Highlight TODO

(use-package hl-todo
  :config
  (global-hl-todo-mode))

;;;; Ripgrep

(use-package ripgrep
  :if (executable-find "rg")
  :defer t)

;;;; Winner

(use-package winner
  :ensure nil
  :defer t
  :custom
  (winner-boring-buffers
   '("*Completions*"
     "*Compile-Log*"
     "*inferior-lisp*"
     "*Fuzzy Completions*"
     "*Apropos*"
     "*Help*"
     "*cvs*"
     "*Buffer List*"
     "*Ibuffer*"
     "*esh command on file*"))
  :config
  (winner-mode 1))

;;;; IBuffer

(use-package ibuffer
  :ensure nil
  :custom
  (ibuffer-expert t))

;;;; Compile

(use-package compile
  :ensure nil
  :defer t
  :hook (compilation-filter . colorize-compilation-buffer)
  :custom
  (ansi-color-for-compilation-mode t)
  (compilation-always-kill t) ; kill compilation process before starting another
  (compilation-ask-about-save nil) ; save all buffers on `compile'
  (compilation-scroll-output t))

;;;; EWW

(use-package eww
  :ensure nil
  :defer t
  :config
  (setq eww-auto-rename-buffer 'title))

;;;; TOC ORG

(use-package toc-org
  :after (org markdown-mode)
  :init
  (add-to-list 'org-tag-alist '("TOC" . ?T))
  :hook (org-mode . toc-org-enable)
  :hook (markdown-mode . toc-org-enable))

;;;; CMake Mode

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" . cmake-mode)
  :defer t
  :config
  (setq cmake-tab-width 4))

(use-package cmake-ts-mode
  :ensure nil
  :mode ("CMakeLists\\.txt\\'" . cmake-mode)
  :config
  (setq cmake-ts-mode-indent-offset 4))

;;;; Markdown Mode

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-fontify-code-blocks-natively t))

;;;; PKGBUILD Mode

(use-package pkgbuild-mode
  :mode ("PKGBUILD" . pkgbuild-mode)
  :custom
  (pkgbuild-update-sums-on-save nil)
  (pkgbuild-ask-about-save nil))

;;;; Python Mode

(defun dj/python-mode-settings ()
  "Settings for python major mode"
  (setq python-indent-offset 4
        tab-width 4
        indent-tabs-mode nil))



(use-package python
  :defer t
  :init
  (setopt python-indent-guess-indent-offset-verbose nil)
  (add-hook 'python-mode-local-vars-hook #'lsp)
  :config
  (python-indent-guess-indent-offset-verbose nil)
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  :hook (python-mode . dj/python-mode-settings))

(use-package python-ts-mode
  :ensure nil
  :hook (python-ts-mode . dj/python-mode-settings))

;;;; Sudo Edit

(use-package sudo-edit
  :defer t
  :commands (sudo-edit))

;;;; AucTex

(use-package auctex
  :defer t
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  ;; to use pdfview with auctex
  (TeX-view-program-selection '((output-pdf "pdf-tools"))
                              TeX-source-correlate-start-server t)
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :hook
  (LaTeX-mode . (lambda ()
                  (turn-on-reftex)
                  (setq reftex-plug-into-AUCTeX t)
                  (reftex-isearch-minor-mode)
                  (setq TeX-PDF-mode t)
                  (setq TeX-source-correlate-method 'synctex)
                  (setq TeX-source-correlate-start-server t))))

;;;; EGLOT

(use-package eglot
  :defer t
  :ensure nil
  :config
  (eglot-report-progress nil)
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp")))
  :custom
  (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider :inlayHintProvider :hoverProvider)))

(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))

;;;; Eglot Booster

(use-package eglot-booster
  :defer t
  :ensure nil
  :after eglot
  :init
  (unless (package-installed-p 'eglot-booster)
    (package-vc-install "https://github.com/jdtsmith/eglot-booster")))

;;;; All the Icons

(use-package all-the-icons
  :defer t
  :if (display-graphic-p))

;;;; Makefile Mode

(use-package makefile-mode
  :ensure nil
  :mode ("Makefile" . makefile-mode))

;;;; Meson Mode

(use-package meson-mode
  :mode ("meson.build" . meson-mode)
  :config
  (setq meson-indent-basic 4))

;;;; Rust Mode

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode))

;;;; Lua Mode

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode))

;;;; Org Mode

(use-package org
  :defer t
  :ensure nil
  :config
  require 'org-tempo
  (setopt org-startup-indented t
          org-directory "~/Gits/Notes/"
          org-ellipsis " 󱞣"
          org-edit-src-content-indentation 4
          org-redisplay-inline-images t
          org-display-inline-images t
          org-startup-with-inline-images "inlineimages"
          org-adapt-indentation nil
          org-confirm-babel-evaluate nil
          org-src-window-setup 'current-window
          org-pretty-entities t
          org-enforce-todo-dependencies t
          org-enforce-todo-checkbox-dependencies t
          org-fontify-whole-heading-line t
          org-fontify-done-headline t
          org-fontify-quote-and-verse-blocks t
          org-startup-indented t
          org-startup-align-all-tables t
          org-use-property-inheritance t
          org-list-allow-alphabetical t
          org-M-RET-may-split-line nil
          org-src-window-setup 'split-window-below
          org-src-fontify-natively t
          org-src-preserve-indentation t
          org-src-tab-acts-natively t
          org-log-done 'time
          org-html-validation-link nil
          org-export-with-toc t
          org-hide-leading-stars t))

;;;; ISearch

(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  (lazy-highlight-initial-delay 0.5)
  (search-whitespace-regexp ".*?"))

;;;; Parentheses (show-paren-mode)
(use-package paren
  :ensure nil
  :hook (prog-mode . show-paren-local-mode)
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'overlay))


;;;; Eldoc (Emacs live documentation feedback)
(use-package eldoc
  :ensure nil
  :hook (prog-mode . eldoc-mode)
  :config
  (setopt eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-message-function #'message)) ; don't use mode line for M-x eval-expression, etc.

;;;; Avy
(use-package avy
  :defer t)

;;;; Org Capture
(use-package org-capture
  :ensure nil
  :config
  (setopt org-capture-templates
          '(
            ;;; Notes

            ("n" "Notes")

            ("ns" "Study these things" entry
             (file+headline "study.org" "Study These Things")
             "* %^{What concept would you like to study: %?}")

            ("nc" "Note using clipboard" entry
             (file+headline "note.org" "Notes")
             "* %?\n\n%(nth -1 kill-ring)")

            ("nr" "Note using highlight region" entry
             (file+headline "note.org" "Notes")
             "* %^{What is the Note}\n%i")


            ;;; Meetings

            ("m" "Meetings" entry
             (file+headline "meetings.org" "Meetings")
             "* %^{Whom ?}\nSCHEDULED: %^t\nADDED: %U")

            ;;; Events

            ("e" "Events" entry
             (file+headline "events.org" "Events")
             "* %^{Name of the event}\nSCHEDULED: %^t\nADDED: %U")))

  :bind ("C-c c" . org-capture))

;;;; csv-mode
(use-package csv-mode
  :ensure t
  :commands (csv-align-mode))

;;;; `outline' (`outline-mode' and `outline-minor-mode')
(use-package outline
  :ensure nil
  :bind
  ("<f9>" . outline-minor-mode)
  :config
  (setq outline-minor-mode-highlight nil) ; emacs28
  (setq outline-minor-mode-cycle t) ; emacs28
  (setq outline-minor-mode-use-buttons nil) ; emacs29---bless you for the nil option!
  (setq outline-minor-mode-use-margins nil)) ; as above

;;;; `shr' (Simple HTML Renderer)

(use-package shr
  :ensure nil
  :defer t
  :config
  (setq shr-use-colors nil)             ; t is bad for accessibility
  (setq shr-use-fonts nil)              ; t is superfluous, given `variable-pitch-mode'
  (setq shr-max-image-proportion 0.6)
  (setq shr-image-animate nil)          ; No GIFs, thank you!
  (setq shr-width fill-column)
  (setq shr-max-width fill-column)
  (setq shr-discard-aria-hidden t)
  (setq shr-fill-text nil)              ; Emacs 31
  (setq shr-cookie-policy nil))

;;;; `url-cookie'
(use-package url-cookie
  :ensure nil
  :commands (url-cookie-list)
  :config
  (setq url-cookie-untrusted-urls '(".*")))

;;;; Pyvenv

(use-package pyvenv
  :defer t)

(use-package ibuffer-vc)


(provide 'packages)
