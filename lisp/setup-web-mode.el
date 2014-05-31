(require 'use-package)
(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[gj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :config
  (progn
    (autopair-mode -1) ;; use web-mode's auto pairing
    (setq-default web-mode-html-offset tab-width)
    (setq-default web-mode-css-offset tab-width)
    (setq-default web-mode-script-offset tab-width)))

(provide 'setup-web-mode)
