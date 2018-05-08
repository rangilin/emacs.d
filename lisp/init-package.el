(require 'package)

;;; install into separate directories for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version) rangi-generated-files-directory)))
  (setq package-user-dir versioned-package-dir))


;; offical melpa mirror repository, just in case we need it
;; (add-to-list 'package-archives '("melpa" . "https://www.mirrorservice.org/sites/melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/") t)
(package-initialize)


(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(setq package-enable-at-startup nil)
(package-initialize)


;; remind me to update package on 1st & 15th every month on start up
(defun rangi-package-update-reminder ()
  (interactive)
  (when (or (string= (format-time-string "%d") "01")
            (string= (format-time-string "%d") "15"))
            (when (y-or-n-p "It has been a while since last reminder, do you want to check package update ?")
              (package-list-packages))))

(add-hook 'emacs-startup-hook 'rangi-package-update-reminder)


(provide 'init-package)
