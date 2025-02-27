(use-package rust-mode
  :mode (("\\.rs\\'" . rust-mode))
  :hook ((rust-mode . eglot-ensure))
  :config
  (setq rust-format-on-save t)
  (setq rust-mode-treesitter-derive t)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((rust-ts-mode rust-mode) .
                   ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))))


(provide 'init-rust)
