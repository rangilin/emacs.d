;;; init-misc.el --- Misc initialization file -*- lexical-binding: t -*-

;; package to edit beancount file
(use-package beancount
  :load-path "site-lisp/beancount-mode"
  :mode ("\\.beancount\\'" . beancount-mode))



(provide 'init-misc)

;; ;; show hotkey when input incomplete command
;; (use-package which-key
;;   :delight
;;   :config
;;   (setq which-key-side-window-max-width 0.5)
;;   (setq which-key-max-description-length 50)
;;   (setq which-key-show-early-on-C-h t)
;;   (setq which-key-idle-delay 3600)
;;   (setq which-key-idle-secondary-delay 0.05)
;;   (which-key-setup-side-window-bottom)
;;   (which-key-mode))


;; ;; control Emacs program
;; (defun rangi-new-emacs (&optional prefix)
;;   "Create a new Emacs instance, mostly for testing purpose"
;;   (interactive "p")
;;   (let ((name "Emacs")
;;         (command "emacs")
;;         (arg (cond ((equal prefix 4) "--debug-init")
;;                    ((equal prefix 16) "-Q")
;;                    (t nil))))
;;     (if arg
;;         (start-process name nil (executable-find command) arg)
;;       (start-process name nil (executable-find command)))))

;; (global-set-key (kbd "C-c e n") 'rangi-new-emacs)
;; (global-set-key (kbd "C-c e r") 'restart-emacs)



;; ;; csv mode
;; (use-package csv-mode
;;   :mode "\\.csv\\'"
;;   :config
;;   (setq csv-separators '("," ";" "|" " " "\t")))

;; (use-package ag
;;   :bind (("C-c s s" . counsel-ag) ; use counsel for better ux
;;          ("C-c s S" . ag)
;;          ("C-c s f" . ag-dired))
;;   :config
;;   (setq ag-highlight-search t))


;; (use-package editorconfig
;;   :delight editorconfig-mode
;;   :config
;;   (editorconfig-mode 1))


;; (use-package yaml-mode
;;   :mode (("\\.yaml\\'" . yaml-mode)
;;          ("\\.yml\\'" . yaml-mode)
;;          ("\\.ansible-lint\\'" . yaml-mode)))
