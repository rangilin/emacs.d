(use-package flymake
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)
         ("C-c l e" . rangi-flymake-show-buffer-diagnostics))
  :config
  (defun rangi-flymake-show-buffer-diagnostics ()
    (interactive)
    (let ((split-height-threshold 1))
      (flymake-show-buffer-diagnostics)
      (other-window 1))))



(use-package company
  :delight
  :bind
  ("M-." . company-complete)
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-annotation-padding 3)
  (setq company-tooltip-minimum-width 80)
  (setq company-tooltip-maximum-width 120)
  (setq company-format-margin-function 'company-text-icons-margin)
  (setq company-text-face-extra-attributes '(:weight bold :slant italic))
  (setq company-show-quick-access t)


  (global-company-mode))


(use-package eldoc
  :delight eldoc-mode)

(use-package eglot
  :bind (:map eglot-mode-map
	            ("C-c l h" . eldoc)
	            ("C-c l f" . eglot-format)
	            ("C-c l r" . eglot-rename))
  :hook ((eglot-managed-mod . rangi-set-eglot-managed-mode))
  :config
  (setq eglot-autoshutdown t))


;; Make eglot/eldoc works better with flymake diagnostics.
;; https://github.com/joaotavora/eglot/discussions/898
(defun rangi-set-eglot-managed-mode ()
  (setq eldoc-documentation-functions
        (cons #'flymake-eldoc-function (remove #'flymake-eldoc-function eldoc-documentation-functions)))
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose))



(provide 'init-prog)
