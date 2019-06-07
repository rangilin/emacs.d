;; web-mode
(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("\\.html.erb\\'" . web-mode))
  :config
  (setq-default web-mode-markup-indent-offset 4)
  (setq-default web-mode-css-indent-offset 4)
  (setq-default web-mode-code-indent-offset 4)

  ;; highlight
  (setq web-mode-enable-current-element-highlight t))


;; js2-mode
(use-package js2-mode
  :delight "JS2"
  :mode "\\.js\\'")


;; URL package
(use-package url
  :ensure nil
  :config
  ;; set up user directory for url package
  (setq url-configuration-directory (expand-file-name "url/" rangi-generated-files-directory))
  (unless (file-exists-p url-configuration-directory) (make-directory url-configuration-directory)))


;; REST API client
(use-package restclient)

(provide 'init-web)
