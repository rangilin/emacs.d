(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  (add-hook 'markdown-mode-hook (lambda () (turn-on-flyspell))))


(provide 'init-markdown)
