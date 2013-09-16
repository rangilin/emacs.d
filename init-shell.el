(require 'em-dirs)

;; exec-path-from-shell
(require-package 'exec-path-from-shell)
(eval-after-load 'exec-path-from-shell
  (exec-path-from-shell-copy-envs '("LANG" "LC_CTYPE")))

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)


;; mimic my shell prompt in eshell
(defvar eshell-prompt-function
      (lambda ()
        (concat (eshell-user-name) "@" (system-name) ": "
                (abbreviate-file-name (eshell/pwd))
                (if (= (user-uid) 0) "\n# " "\n$"))))

(provide 'init-shell)
