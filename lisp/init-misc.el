;; show hotkey when input incomplete command
(use-package which-key
  :delight
  :config
  (setq which-key-side-window-max-width 0.5)
  (setq which-key-max-description-length 50)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 3600)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-setup-side-window-bottom)
  (which-key-mode))


;; csv mode
(use-package csv-mode
  :mode "\\.csv\\'"
  :config
  (setq csv-separators '("," ";" "|" " " "\t")))

(use-package ag
  :bind (("C-c s s" . counsel-ag) ; use counsel for better ux
         ("C-c s S" . ag)
         ("C-c s f" . ag-dired))
  :config
  (setq ag-highlight-search t))


(use-package editorconfig
  :delight editorconfig-mode
  :config
  (editorconfig-mode 1))


(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)
         ("\\.ansible-lint\\'" . yaml-mode)))


(provide 'init-misc)
