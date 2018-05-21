;;;; PHP mode
(require-package 'php-mode)

(with-eval-after-load 'php-mode

  ;; current
  (define-key php-mode-map (kbd "C-c m c c") 'php-current-class)
  (define-key php-mode-map (kbd "C-c m c n") 'php-current-namespace))




;;;; Apache mode
(require-package 'apache-mode)


(provide 'init-php)
