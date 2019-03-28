(use-package company
  :delight
  :config
  (global-company-mode)
  :bind
  ("M-." . company-complete)
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)))



(provide 'init-company)
