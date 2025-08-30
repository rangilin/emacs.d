;;; init-misc.el --- Misc initialization file -*- lexical-binding: t -*-

;; package to edit beancount file
(use-package beancount
  :load-path "site-lisp/beancount-mode"
  :mode ("\\.beancount\\'" . beancount-mode))


;; enable repeat mode
(use-package repeat
  :config
  (setq set-mark-command-repeat-pop t)
  (repeat-mode))


;; docker
(use-package dockerfile-mode
  :ensure t
  :pin nongnu
  :mode "Dockerfile\\'")


;; yaml
(use-package yaml-mode
  :ensure t
  :pin nongnu
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)
         ("\\.ansible-lint\\'" . yaml-mode)))


;; MacOS
(setq mac-command-modifier 'super)
(setq mac-right-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)
(setq-default locate-command "mdfind")


;; URL
(defun rangi-browse-duckduckgo ()
  (interactive)
  (rangi-browse-url "Search DuckDuckGo: " "https://duckduckgo.com/?q=%s"))

(defun rangi-open-dictionary ()
  (interactive)
  (rangi-open-url "Search Dictionary: " "dict://%s"))

(bind-key "C-c o o" 'browse-url-at-point)
(bind-key "C-c o <mouse-1>" 'browse-url-at-mouse)
(bind-key "C-c o s" 'rangi-browse-duckduckgo)
(bind-key "C-c o d" 'rangi-open-dictionary)


;; csv mode
(use-package csv-mode
  :pin gnu
  :ensure t
  :mode "\\.csv\\'"
  :config
  (setq csv-separators '("," ";" "|" " " "\t")))


(provide 'init-misc)
