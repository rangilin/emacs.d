(defconst rangi-generated-files-directory
  (file-name-as-directory (expand-file-name "gen" user-emacs-directory))
  "Path of directory where we put file that generated automatically by packages or Emacs itself")


;; install into separate directories for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version) rangi-generated-files-directory)))
  (setq package-user-dir versioned-package-dir)
  (setq package-gnupghome-dir (expand-file-name "gnupg" package-user-dir)))
