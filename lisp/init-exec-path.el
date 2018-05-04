(require-package 'exec-path-from-shell)

(with-eval-after-load 'exec-path-from-shell
  (dolist (var '("LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))

(exec-path-from-shell-initialize)

(provide 'init-exec-path)
