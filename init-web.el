(require 'init-elpa)

(require-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(provide 'init-web)
