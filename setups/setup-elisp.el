(require 'use-package)

(use-package emacs-lisp-mode
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)))
  :mode ("Cask" . emacs-lisp-mode))

(use-package eldoc
  :diminish "")

;; ------------------------------ eval and replace the sexp
;; http://emacsredux.com/blog/2013/06/21/eval-and-replace/
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(bind-key "C-c e" 'eval-and-replace)

;; ------------------------------ ielm
(use-package ielm
  :init (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))


(provide 'setup-elisp)
