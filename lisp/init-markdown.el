(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  (add-hook 'markdown-mode-hook (lambda () (turn-on-flyspell)))
  (setq markdown-command
        (concat "/usr/local/bin/pandoc -s"
                " --quiet"
                " -c ~/Documents/pandoc/pandoc.css"
                " -f markdown"
                " -t html")))


(provide 'init-markdown)
