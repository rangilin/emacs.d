(use-package rust-mode
  :mode (("\\.rs\\'" . rust-mode))
  :hook ((rust-mode . prettify-symbols-mode)
         (rust-mode . eglot-ensure))
  :config
  (setq rust-format-on-save t)
  (setq rust-mode-treesitter-derive t))

(add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

(provide 'init-rust)
