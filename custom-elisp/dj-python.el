;;; dj-python.el -*- lexical-binding: t; -*-

;;;###autoload
(defun dj/python-install-package ()
  "Install a Python package using pip in the current virtualenv."
  (interactive)
  (if (derived-mode-p 'python-mode)
      (let* ((venv python-shell-virtualenv-root)
             (pip (when venv (expand-file-name "bin/pip" venv)))
             (package (read-string "Python Package(s) to install: ")))
        (if (and pip (file-exists-p pip))
            (let ((command (format "%s install %s" pip package)))
              (compilation-start command
                                 'comint-mode
                                 (lambda (_) "*pip install*")))
          (message "No valid virtualenv or pip found.")))
    (message "Not in a Python buffer.")))

(provide 'dj-python)
