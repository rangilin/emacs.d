(require 'use-package)

(use-package guide-key
  :diminish guide-key-mode
  :init
  (progn
    (defun rangi/guide-key-for-org-mode ()
      (guide-key/add-local-highlight-command-regexp "org-"))
    (add-hook 'org-mode-hook 'rangi/guide-key-for-org-mode)

    (setq guide-key/idle-delay 2)
    (setq guide-key/recursive-key-sequence-flag t)
    (setq guide-key/guide-key-sequence '("C-c" "C-x"))
    (guide-key-mode 1)))

(provide 'setup-guide-key)
