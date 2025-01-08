(use-package go-mode
  :mode "\\.go\\'"
  :interpreter "go"
  :hook ((go-mode . eglot-ensure)))

(provide 'init-go)
