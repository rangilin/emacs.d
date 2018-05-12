;;;; Web mode
(require-package 'web-mode)

(require 'web-mode)

;; open with web-mode in these files
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))


;; change indent offset
(setq-default web-mode-markup-indent-offset 4)
(setq-default web-mode-css-indent-offset 4)
(setq-default web-mode-code-indent-offset 4)


;; highlight
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)




(provide 'init-web-mode)
