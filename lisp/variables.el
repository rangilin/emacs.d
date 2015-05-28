(defvar rangi-gen-dir
  (file-name-as-directory (expand-file-name ".gen" user-emacs-directory))
  "Directory to put files that generated by Emacs")

(defvar rangi-theme 'darktooth "Theme to load by default")

(defvar rangi-startup-file "~/Dropbox/personal/main.org"
  "File to open when emacs is started")

;; ------------------------------ aspell related
(defvar rangi-dictionary-dir "~/Dropbox/files/aspell"
  "Directory for dictionary files")

(defvar rangi-personal-dictionary-en
  (expand-file-name ".aspell.en.pws" rangi-dictionary-dir)
  "Personal English aspell dictionary ")

(provide 'variables)
