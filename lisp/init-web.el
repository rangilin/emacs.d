;; web-mode
(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.gohtml\\'" . web-mode)
         ("\\.erb\\'" . web-mode))
  :config
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)

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
  (setq url-configuration-directory (expand-file-name "url/" rangi-emacs-cache-directory))
  (unless (file-exists-p url-configuration-directory) (make-directory url-configuration-directory)))


;; REST API client
(use-package restclient
  :mode ("\\.restclient\\'" . restclient-mode))


;; Nginx
(use-package nginx-mode)

;; Apache
(use-package apache-mode)


(provide 'init-web)
