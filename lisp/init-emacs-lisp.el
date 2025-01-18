(use-package elisp-mode
  :ensure nil
  :delight
  (emacs-lisp-mode "Elisp" :major))


(use-package eldoc
  :delight eldoc-mode)


(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(bind-key "C-c C-e" 'eval-and-replace)

(provide 'init-emacs-lisp)
