(require 'use-package)
(require 'variables)

(use-package flyspell
  :diminish ""
  :bind
  (("C-M-$" . flyspell-buffer)
   ("C-$" . flyspell-check-previous-highlighted-word))
  :init
  (progn
    (setq ispell-dictionary "english")
    (setq ispell-personal-dictionary (expand-file-name ".aspell.en.pws" rangi/dictionary-dir) )

    (setq flyspell-issue-message-flag nil)

    (add-hook 'markdown-mode-hook 'flyspell-mode)
    (add-hook 'monky-log-edit-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (progn
    (bind-key "C-;" nil flyspell-mode-map))) ; reserved for ace jump

(provide 'setup-flyspell)
