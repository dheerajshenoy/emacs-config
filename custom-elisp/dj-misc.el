;; dj-misc.el -*- lexical-binding: t; -*-

;;; Miscellaneous functions
;;;###autoload
(defun dj/backward-kill-word ()
  "Remove all whitespace if the character behind the cursor is whitespace, otherwise remove a word."
  (interactive)
  (if (looking-back "[ \n]")
      ;; delete horizontal space before us and then check to see if we
      ;; are looking at a newline
      (progn (delete-horizontal-space 't)
             (while (looking-back "[ \n]")
               (backward-delete-char 1)))
    ;; otherwise, just do the normal kill word.
    (backward-kill-word 1)))

;;;###autoload
(defun dj/get-latest-clipboard-content ()
  "Returns a STR containing the latest clipboard content"
  (nth -1 kill-ring))

;;;###autoload
(defun dj/dired-xdg-open ()
  "Open the current file or directory with xdg-open."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (start-process "xdg-open" nil "xdg-open" file)))

(provide 'dj-misc)
