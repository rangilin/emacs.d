(require 'functions)
(require 'use-package)

(use-package "eshell"
  :init
  (progn
    (use-package "em-smart")

    (defmacro with-face (str &rest properties)
      `(propertize ,str 'face (list ,@properties)))

    (defun rangi-eshell-prompt ()
      (concat
       (propertize (format-time-string "[%H:%M:%S]" (current-time))
                   'face font-lock-keyword-face)
       (propertize (or (ignore-errors
                         (format " [%s] " (vc-responsible-backend default-directory))) " ")
                   'face font-lock-keyword-face)
       (propertize (concat user-login-name "@" system-name)
                   'face font-lock-constant-face)
       " "
       (propertize (abbreviate-file-name (eshell/pwd))
                   'face font-lock-constant-face)
       " \n"
       (if (= (user-uid) 0)
             (with-face " #" :foreground "red")
         " $")
       " "))

    (setq-default eshell-prompt-function 'rangi-eshell-prompt)
    (setq-default eshell-where-to-jump 'begin)
    (setq-default eshell-review-quick-commands nil)
    (setq-default eshell-smart-space-goes-to-end t)))

(provide 'setup-eshell)
