(require 'use-package)
(require 'variables)

(use-package flyspell
  :diminish ""
  :bind
  (("C-M-$" . flyspell-buffer)
   ("C-$" . flyspell-check-previous-highlighted-word))
  :init
  (progn
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("-C" "--sug-mode=ultra" "--run-together-limit=5"))
    (setq ispell-dictionary "english")
    (setq ispell-personal-dictionary rangi/personal-dictionary-en)

    (setq flyspell-issue-message-flag nil)

    (add-hook 'markdown-mode-hook 'flyspell-mode)
    (add-hook 'monky-log-edit-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode))
  :config
  (progn
    (bind-key "C-;" nil flyspell-mode-map))) ; reserved for ace jump

(provide 'setup-spell)
