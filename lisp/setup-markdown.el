(require 'use-package)

(use-package markdown-mode
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode))
  :config
  (progn
    (setq-default markdown-coding-system 'utf-8)
    (setq-default markdown-content-type "text/html")))

(provide 'setup-markdown)
