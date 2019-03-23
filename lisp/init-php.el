;;;; PHP mode
(require-package 'php-mode)

(with-eval-after-load 'php-mode

  ;; current
  (define-key php-mode-map (kbd "C-c m c c") 'php-current-class)
  (define-key php-mode-map (kbd "C-c m c n") 'php-current-namespace))



;;;; Psysh

(require-package 'psysh)

(defvar psysh-doc-buffer-color 'only-emacs)




;;;; Apache mode
(require-package 'apache-mode)


(provide 'init-php)
