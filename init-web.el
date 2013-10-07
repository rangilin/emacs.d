(require 'init-elpa)

(require-package 'simple-httpd)
(require-package 'impatient-mode)

(require-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(provide 'init-web)
