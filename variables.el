(defvar rangi/gen-dir (file-name-as-directory (expand-file-name ".gen" user-emacs-directory))
  "Directory to put files that generated by Emacs")

(defvar rangi/startup-file "/ramsey/Dropbox/org/personal/index.org"
  "File to open when emacs is started")

(defvar rangi/theme 'sanityinc-tomorrow-bright
  "Default theme")

(require 'local-variables nil t)

(provide 'variables)
