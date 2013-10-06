(require 'init-elpa)
(require-package 'js2-mode)

;; use js2-mode in *.js
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; shorter mode name
(eval-after-load 'js2-mode
  (add-hook 'js2-mode-hook '(lambda () (setq mode-name "js2"))))

(provide 'init-javascript)
