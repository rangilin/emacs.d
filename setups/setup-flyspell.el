(require 'use-package)

;; (use-package flyspell
;;   :diminish ""
;;   :bind
;;   (("C-M-$" . flyspell-buffer)
;;    ("C-$" . flyspell-check-previous-highlighted-word))
;;   :init
;;   (progn
;;     (setq ispell-dictionary "english")
;;     (add-hook 'markdown-mode-hook 'flyspell-mode)
;;     (add-hook 'monky-log-edit-mode-hook 'flyspell-mode)
;;     (add-hook 'prog-mode-hook 'flyspell-prog-mode))
;;   :config
;;   (progn
;;     (bind-key "C-;" nil flyspell-mode-map))) ; reserved for ace jump

(provide 'setup-flyspell)
