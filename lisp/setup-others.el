(require 'use-package)

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

;; -------------------------------------------------- dired
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-listing-switches "-aBhl  --group-directories-first")

(provide 'setup-others)
