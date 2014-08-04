(require 'use-package)
(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.[gj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :init
  (progn
    (defun rangi/web-mode-hook ()
      (setq indent-tabs-mode t))
    (add-hook 'web-mode-hook 'rangi/web-mode-hook))

  :config
  (progn
    (autopair-mode -1) ;; use web-mode's auto pairing
    (setq-default web-mode-markup-indent-offset tab-width)
    (setq-default web-mode-css-indent-offset tab-width)
    (setq-default web-mode-code-indent-offset tab-width)))

(provide 'setup-web-mode)
