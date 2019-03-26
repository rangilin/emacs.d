(use-package elisp-mode
  :ensure nil
  :delight
  (emacs-lisp-mode "Elisp" :major))


(use-package eldoc
  :delight eldoc-mode)


;; restart Emacs
(defun rangi-new-emacs (&optional prefix)
  "Create a new Emacs instance, mostly for testing purpose"
  (interactive "p")
  (let ((name "Emacs")
        (command "/Applications/Emacs.app/Contents/MacOS/Emacs")
        (arg (cond ((equal prefix 4) "--debug-init")
                   ((equal prefix 16) "-Q")
                   (t ""))))
    (start-process name nil (executable-find command) arg)))

(global-set-key (kbd "C-c e r") 'rangi-new-emacs)

(provide 'init-emacs-lisp)
