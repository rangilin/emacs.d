(defconst rangi-emacs-cache-directory
  (file-name-as-directory (expand-file-name ".cache" user-emacs-directory))
  "Path of directory where we put file that generated automatically by packages or Emacs itself")

;; install into separate directories for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version) rangi-emacs-cache-directory)))
  (setq package-user-dir versioned-package-dir)
  (setq package-gnupghome-dir (expand-file-name "gnupg" package-user-dir)))


;; package will be loaded later
(setq package-enable-at-startup nil)

(startup-redirect-eln-cache rangi-emacs-cache-directory)
