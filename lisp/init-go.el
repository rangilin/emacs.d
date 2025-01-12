(use-package go-mode
  :mode "\\.go\\'"
  :interpreter "go"
  :hook ((go-mode . eglot-ensure)
         (before-save . gofmt-before-save)))

(provide 'init-go)
