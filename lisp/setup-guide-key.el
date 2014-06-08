(require 'use-package)

(use-package guide-key
  :init
  (progn
    ;; ------------------------------ org-mode
    (defun rangi/guide-key-for-org-mode ()
      (guide-key/add-local-guide-key-sequence "C-c")
      (guide-key/add-local-guide-key-sequence "C-c C-x")
      (guide-key/add-local-highlight-command-regexp "org-"))
    (add-hook 'org-mode-hook 'rangi/guide-key-for-org-mode)

    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
    (guide-key-mode 1)))

(provide 'setup-guide-key)
