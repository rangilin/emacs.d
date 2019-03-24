(use-package elisp-mode
  :ensure nil
  :delight
  (emacs-lisp-mode "Elisp" :major))


(use-package eldoc
  :delight eldoc-mode)


(defun rangi-new-emacs ()
  "Create a new emacs instance, mostly for testing purpose."
  (interactive)
  (start-process "Emacs" nil (executable-find "/Applications/Emacs.app/Contents/MacOS/Emacs")))


(provide 'init-emacs-lisp)
