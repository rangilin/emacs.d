(defvar rangi/gen-dir (file-name-as-directory (expand-file-name ".gen" user-emacs-directory))
  "Directory to put files that generated by Emacs")


(defvar rangi/theme 'sanityinc-tomorrow-eighties
  "Default theme")

(defvar rangi/startup-file "/ramsey/Dropbox/personal/main.org"
  "File to open when emacs is started")

;; ------------------------------ elfeed
(defvar rangi/elfeed-directory "/ramsey/Dropbox/rss/"
  "Directory where contains DB data for elfeed")

;; ------------------------------ aspell
(defvar rangi/dictionary-dir "/ramsey/Dropbox/files/aspell"
  "Directory for dictionary files")

(defvar rangi/personal-dictionary-en
  (expand-file-name ".aspell.en.pws" rangi/dictionary-dir)
  "Personal English aspell dictionary ")

(require 'local-variables nil t)

(provide 'variables)
