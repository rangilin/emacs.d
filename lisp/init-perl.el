(defvar rangi-cperl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'cperl-perldoc-at-point)
    (define-key map "D" 'cperl-perldoc)
    map)
  "My keymap in cperl mode")

(use-package cperl-mode
  :mode "\\.pl\\'"
  :interpreter "perl"
  :bind-keymap (("s-m" . rangi-cperl-mode-map)
                ("C-c m" . rangi-cperl-mode-map))
  :config
  (setq cperl-invalid-face nil)
  :hook ((cperl-mode . eglot-ensure)))

(provide 'init-perl)
