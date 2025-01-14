;; show hotkey when input incomplete command
(use-package which-key
  :delight
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))


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
