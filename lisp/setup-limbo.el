;;;; place for setups that I don't use anymore
(require 'use-package)

;; ------------------------------ moz
;; (use-package moz
;;   :load-path "site-lisp/moz"
;;   :init
;;   (progn
;;     (defun rangi/moz-start () (moz-minor-mode 1))
;;     (defun rangi/moz-reload-firefox ()
;;       "Reload current tab of firefox if moz minor mode is enabled"
;;       (interactive)
;;       (if (and (boundp 'moz-minor-mode) moz-minor-mode)
;;           (process-send-string (inferior-moz-process) "BrowserReload()\n")))
;;     (defun rangi/moz-hook-reload-after-save ()
;;       "Add hook to reload firefox on save"
;;       (interactive)
;;       (add-hook 'after-save-hook 'rangi/moz-reload-firefox))
;;     (add-hook 'html-mode-hook 'rangi/moz-hook-reload-after-save)
;;     (add-hook 'css-mode-hook 'rangi/moz-hook-reload-after-save)
;;     (bind-key "C-c r" 'rangi/moz-reload-firefox)))

(provide 'setup-limbo)
