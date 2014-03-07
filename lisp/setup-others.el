(require 'use-package)

;; 1.2.0 http://elpa.gnu.org/packages/csv-mode.html
;; no idea why 1.5.0 in marmalade not works
(use-package csv-mode
  :load-path "site-lisp/csv-mode"
  :mode ("\\.csv$" . csv-mode))

(use-package moz
  :load-path "site-lisp/moz"
  :init
  (progn
    (defun rl/moz-start () (moz-minor-mode 1))
    (defun rl/moz-reload-firefox ()
      "Reload current tab of firefox if moz minor mode is enabled"
      (interactive)
      (if (and (boundp 'moz-minor-mode) moz-minor-mode)
          (process-send-string (inferior-moz-process) "BrowserReload()\n")))
    (defun rl/moz-hook-reload-after-save ()
      "Add hook to reload firefox on save"
      (interactive)
      (add-hook 'after-save-hook 'rl/moz-reload-firefox))
    (add-hook 'html-mode-hook 'rl/moz-hook-reload-after-save)
    (add-hook 'css-mode-hook 'rl/moz-hook-reload-after-save)
    (bind-key "C-c r" 'rl/moz-reload-firefox)))

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
