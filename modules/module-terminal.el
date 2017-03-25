(defun rl-init-module-terminal ()
  (rl--set-up-exec-path))


(defun rl--set-up-exec-path ()
  (use-package exec-path-from-shell
    :ensure t
    :config
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))))



(provide 'module-terminal)
