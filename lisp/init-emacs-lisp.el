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

;; new Emacs
(defun rangi-new-emacs (&optional prefix)
  "Create a new Emacs instance, mostly for testing purpose"
  (interactive "p")
  (let ((name "Emacs")
        (command "/Applications/Emacs.app/Contents/MacOS/Emacs")
        (arg (cond ((equal prefix 4) "--debug-init")
                   ((equal prefix 16) "-Q")
                   (t ""))))
    (start-process name nil (executable-find command) arg)))

(global-set-key (kbd "C-c e n") 'rangi-new-emacs)

(provide 'init-emacs-lisp)
