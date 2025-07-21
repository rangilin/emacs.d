(use-package elisp-mode
  :ensure nil
  :delight
  (emacs-lisp-mode "Elisp" :major)
  :hook ((emacs-lisp-mode . prettify-symbols-mode)))


(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; keybinding
(bind-key "C-c C-e" 'eval-and-replace)

;; preserve keybinding in other modes
(define-key org-mode-map (kbd "C-c C-e") nil)


(provide 'init-emacs-lisp)
