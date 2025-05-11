;; -*- lexical-binding: t; -*-

(defun dj/my-byte-compile-if-needed (file)
  "Byte-compile FILE if the .elc file is missing or older than the .el file."
  (let ((elc-file (concat (file-name-sans-extension file) ".elc")))
    (when (or (not (file-exists-p elc-file))
              (file-newer-than-file-p file elc-file))
      (message "Byte-compiling %s..." file)
      (byte-compile-file file))))

(defun dj/my-load-and-byte-compile (file)
  "Byte-compile FILE if needed, then load it."
  (dj/my-byte-compile-if-needed file)
  (load (file-name-sans-extension file)))

(provide 'dj-bytecompile)
