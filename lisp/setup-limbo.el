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

;; ------------------------------ workaround emacsclient theme bug
;; (defun rangi/reload-theme (&rest frame)
;;   (when window-system
;;     (let ((theme rangi-theme))
;;       (message "Reloading theme %s" theme)
;;       (load-theme theme))
;;
;;     ;; some customization for sanityinc-tomorrow-eighties theme
;;     (set-face-attribute 'mode-line nil
;;                         :box '(:line-width 1 :color "#EEE")
;;                         :foreground "white"
;;                         :background (face-attribute 'default :background))
;;     (set-face-attribute 'mode-line-inactive nil :background (face-attribute 'default :background))
;;     (set-face-background 'fringe (face-attribute 'default :background))))
;;
;; (defadvice server-create-window-system-frame
;;   (after reload-theme-on-frame-created ())
;;   "Reload theme when a frame is created"
;;
;;   (rangi/reload-theme))
;;
;; (ad-activate 'server-create-window-system-frame)
;; (add-hook 'after-make-frame-functions 'rangi/reload-theme t)

;; ------------------------------ default frame size to maximized
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'setup-limbo)
