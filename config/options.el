;; -*- lexical-binding: t; -*-

;; OPTIONS for dj's emacs

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent ; Emacs 28 with native compilation
        native-comp-async-jobs-number 8
        native-compile-prune-cache t)) ; Emacs 29

(setopt warning-minimum-level :emergency
        make-backup-files nil
        vc-make-backup-files nil
        backup-inhibited t
        create-lockfiles nil)

(setopt mouse-wheel-follow-mouse 't
        history-length 300 ;; Save what you enter into minibuffer prompts
        load-prefer-newer t)

(setopt tab-always-indent 'complete)

;; Disable the damn thing by making it disposable.
(setq custom-file (make-temp-file "emacs-custom-"))

(setq-default indent-tabs-mode nil)

(when (file-exists-p custom-file)
  (load custom-file))


(setopt read-file-name-completion-ignore-case t
        use-dialog-box nil
        read-buffer-completion-ignore-case t
        global-auto-revert-non-file-buffers t
        completion-ignore-case t
        delete-by-moving-to-trash t)
;; Replace selection on insert

(setq-default initial-scratch-message nil)
(setq-default sentence-end-double-space nil)
(add-hook 'before-save-hook 'whitespace-cleanup) ;; cleanup whitespace on save

(setopt scroll-conservatively 1000
        use-short-answers t
        undo-limit 100000000
        auto-save-default t)

(setopt user-full-name       "Dheeraj Vittal Shenoy"
        user-real-login-name "Dheeraj Vittal Shenoy"
        user-login-name      "dheerajshenoy"
        user-mail-address    "dheerajshenoy22@gmail.com")

(setopt dired-mouse-drag-files                   t
        mouse-drag-and-drop-region-cross-program t
        confirm-kill-emacs 'y-or-n-p ;; Ask before killing emacs
        confirm-kill-processes nil) ;; Automatically kill all active processes when closing Emacs

(setq-default fringes-outside-margins t)

;; Vertical Scroll
(setopt scroll-step 1
        scroll-margin 1
        scroll-conservatively 101
        scroll-up-aggressively 0.01
        scroll-down-aggressively 0.01
        auto-window-vscroll nil
        mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil
        hscroll-step 1
        hscroll-margin 1
        custom-safe-themes t)

(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default js-switch-indent-offset 4)

(c-set-offset 'comment-intro 0)
(c-set-offset 'innamespace 0)
(c-set-offset 'case-label '+)
(c-set-offset 'access-label 0)
(c-set-offset (quote cpp-macro) 0 nil)

(defun dj/prog-mode-settings ()
  (setopt display-line-numbers-type 'visual)
  (display-line-numbers-mode))

(add-hook 'prog-mode-hook #'dj/prog-mode-settings)

(setopt idle-update-delay 1.0
        help-window-select t
        kill-do-not-save-duplicates t)

(setq initial-scratch-message
      ";; Welcome to Emacs!\n;; This buffer is your sandbox.\n\n")

(repeat-mode)
(save-place-mode)
(show-paren-mode)
(pixel-scroll-precision-mode)
(global-auto-revert-mode) ;; Revert Dired and other buffers
(delete-selection-mode)
(global-subword-mode)

(setopt completion-styles '(prescient orderless))

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
;; No second pass of case-insensitive search over auto-mode-alist.
(setopt auto-mode-case-fold nil)

(setopt read-answer-short t)
(setopt find-file-visit-truename t
        vc-follow-symlinks t)

;; Move point to top/bottom of buffer before signaling a scrolling error.
(setopt scroll-error-top-bottom t)

;; Keep screen position if scroll command moved it vertically out of the window.
(setopt scroll-preserve-screen-position t)

;; If `scroll-conservatively' is set above 100, the window is never
;; automatically recentered, which decreases the time spend recentering.
(setopt scroll-conservatively 101)

;; Number of lines of margin at the top and bottom of a window.
(setopt scroll-margin 0)

;; Horizontal scrolling
(setopt hscroll-margin 2
        hscroll-step 1)

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setopt highlight-nonselected-windows nil)

(setq-default left-fringe-width  0)
(setq-default right-fringe-width 0)

;; Eliminate delay before highlighting search matches
(setopt lazy-highlight-initial-delay 0)

;; Makes Emacs omit the load average information from the mode line.
(setopt display-time-default-load-average nil)

;; In Emacs 30 and newer, disable Ispell completion to avoid annotation errors
;; when no `ispell' dictionary is set.
(setopt text-mode-ispell-word-completion nil)

;; (setopt split-width-threshold 0
;;         split-height-threshold nil)

; I never really need to match letters case-sensitively in the minibuffer

(setq completion-ignore-case t)
(setopt read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t)
(setq-default case-fold-search t)   ; For general regexp

;;;; Window Placement Control (display-buffer-alist)

(setopt display-buffer-alist
        '(("\\*Python\\*"
           (display-buffer-in-side-window)
           (side . right)
           (slot . 0)
           (window-width . 0.4))

          ("\\*Calculator\\*"
           (display-buffer-in-side-window)
           (side . right)
           (slot . 1)
           (window-width . 0.4))))




(provide 'options)
