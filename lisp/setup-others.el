(require 'use-package)
(require 'variables)

;; 1.2.0 http://elpa.gnu.org/packages/csv-mode.html
;; no idea why 1.5.0 in marmalade not works
(use-package csv-mode
  :load-path "site-lisp/csv-mode"
  :mode ("\\.csv$" . csv-mode))

(use-package sh-script
  :init
  (progn
    (custom-set-variables '(sh-basic-offset tab-width))))

(use-package webmacro-mode
  :load-path "site-lisp/webmacro-mode"
  :mode ("\\.wm[m]?$" . webmacro-mode))

(use-package smex
  :bind ("M-x" . smex))

(use-package projectile
  :diminish projectile-mode
  :init
  (progn
    (projectile-global-mode)
    (setq-default projectile-switch-project-action 'projectile-dired)
    (setq-default projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" rangi/gen-dir))
    (projectile-load-known-projects)))

(use-package feature-mode
  :mode ("\\.feature$" . feature-mode))

(use-package apache-mode
  :mode (("\\.htaccess\\'"   . apache-mode)
         ("httpd\\.conf\\'"  . apache-mode)
         ("srm\\.conf\\'"    . apache-mode)
         ("access\\.conf\\'" . apache-mode)
         ("sites-\\(available\\|enabled\\)/" . apache-mode)))

;; ------------------------------ dired
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-listing-switches "-aBhl  --group-directories-first")

;; ------------------------------ apropos
;; use apropos instead of apropos-command
(bind-key "a" 'apropos help-map)


;; ------------------------------ startup file
(defun rangi--open-startup-file ()
  (interactive)
  (if (file-exists-p rangi/startup-file)
    (find-file rangi/startup-file)
    (message (format "File %s does not exist" rangi/startup-file))))

(bind-key "C-c <home>" 'rangi--open-startup-file)


(provide 'setup-others)
