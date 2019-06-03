(use-package terraform-mode
  :mode "\\.tf\\'"
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

  (defun rangi-terraform-format-current-dir()
    "Run terraform fmt on current dir"
    (interactive)
    (if (eq major-mode 'terraform-mode)
        (save-excursion
          (shell-command "terraform fmt" nil "*Terraform Error Buffer*"))
      (error "Refuse to format current buffer because it is not in `terraform-mode'"))))


(use-package conf-mode
  :mode "credentials\\'")


(provide 'init-devop)
