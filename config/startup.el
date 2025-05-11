;; -*- lexical-binding: t;-*-

(let ((config-dir (concat user-emacs-directory "config")))
  (dolist (file (directory-files config-dir t "\\.el$"))
    (unless (string-match-p "startup\\.el$" file) ;; skip startup.el
      (dj/my-load-and-byte-compile file))))

(let ((config-dir (concat user-emacs-directory "custom-elisp")))
  (dolist (file (directory-files config-dir t "\\.el$"))
      (dj/my-load-and-byte-compile file)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(provide 'startup)
