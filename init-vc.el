;; Source version control related configuration

;; -------------------------------------------------- git
(require-package 'magit)
(require 'magit)
(eval-after-load "magit"
  ;; override so magit don't highlight
  '(defun magit-highlight-section ()))

;; -------------------------------------------------- mercurial
(require-package 'monky)
(require 'monky)
(setq monky-process-type 'cmdserver)

(provide 'init-vc)
