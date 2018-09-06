;;;; JS2 mode
(require-package 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))





;;;; Web mode
(require-package 'web-mode)

(require 'web-mode)

;; open with web-mode in these files
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; change indent offset
(setq-default web-mode-markup-indent-offset 4)
(setq-default web-mode-css-indent-offset 4)
(setq-default web-mode-code-indent-offset 4)

;; highlight
(setq web-mode-enable-current-element-highlight t)





;;;; web beautifier
(require-package 'web-beautify)

(eval-after-load 'css-mode
  '(add-hook 'css-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))


(provide 'init-web)
