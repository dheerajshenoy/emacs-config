;; -*- lexical-binding: t; -*-

;; Define a modeline component that just shows the buffer percentage
(defvar-local dj-modeline-position-in-buffer
  '(:eval (propertize "%p" 'face 'mode-line-percent-position))
  "Modeline construct showing the position in the buffer as a percentage.")

;; Define VC branch display
(defvar-local dj-modeline-vc
  '(:eval
    (when (and vc-mode buffer-file-name)
      (let ((backend (vc-backend buffer-file-name)))
        (when backend
          (format "[%s]" (substring vc-mode (+ (length (symbol-name backend)) 2)))))))
  "Modeline construct showing the current VC (version control) branch.")

(defvar-local dj-modeline-python-virtualenv
  '(:eval
    (when (derived-mode-p 'python-mode)
      (let ((venv python-shell-virtualenv-path))
        (when venv
          (format "[%s]" (file-name-nondirectory (directory-file-name venv))))))))

(dolist (construct '(dj-modeline-position-in-buffer
                     dj-modeline-python-virtualenv
                     dj-modeline-vc)))

(use-package dj-modeline
  :ensure nil
  :custom
  (mode-line-compact nil)
  (mode-line-right-align-edge 'right-margin)
  (line-number-mode nil)
  :init
  (setq-default mode-line-format
                (list
                 " "
                 ;; Show buffer name
                 '(:eval (propertize "%b" 'face 'mode-line-buffer-id))
                 ;; Show modified status
                 '(:eval (when (buffer-modified-p) (propertize " *" 'face 'error)))
                 "  "
                 dj-modeline-python-virtualenv
                 "  "
                 'keycast-mode-line " "
                 ;; The rest of the space
                 'mode-line-format-right-align

                 'mode-line-modes
                 ;; Your custom buffer position percentage
                 dj-modeline-position-in-buffer
                 "  "
                 dj-modeline-vc
                 )))

;;; Keycast mode
(use-package keycast
  :after dj-modeline
  :commands (keycast-mode-line-mode keycast-header-line-mode keycast-tab-bar-mode keycast-log-mode)
  :config
  (keycast-mode-line-format "%2s%k%c%R")
  (setq keycast-mode-line-insert-after 'mode-line-modes)
  (keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (keycast-mode-line-remove-tail-elements nil)
  :config
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '( mouse-event-p mouse-movement-p mwheel-scroll handle-select-window
                    mouse-set-point mouse-drag-region))
    (add-to-list 'keycast-substitute-alist `(,event nil))))

(provide 'modeline)
