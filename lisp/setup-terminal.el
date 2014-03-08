(require 'use-package)

;; ------------------------------ term
(use-package multi-term
  :bind (("C-c t" . multi-term))
  :config
  (progn

    ;; ------------------------------ workaround view-lossage issues
    ;; clear recorded keystroke on enter is pressed
    ;; avoid view-lossage to display password
    (defadvice term-send-raw (after clear-recorded-key activate)
      (if (string= (kbd "RET") (this-command-keys))
          (clear-this-command-keys)))

    ;; ------------------------------ hook for setup term mode
    (defun rl/setup-term-mode ()
      (yas-minor-mode -1)) ; yas tab not works well with term
    (add-hook 'term-mode-hook 'rl/setup-term-mode)

    (defun rl/toggle-term-mode ()
      "Toggle between `term-line-mode' and `term-char-mode'"
      (interactive)
      (if (term-in-line-mode)
          (term-char-mode)
          (term-line-mode)))

    (setq-default multi-term-program "/bin/bash")
    (setq-default multi-term-program-switches "--login")
    (setq-default term-buffer-maximum-size 10000)
    (setq-default term-unbind-key-list
                  `("C-z" "C-x" "C-h" "C-c" "C-y" "<ESC>"))
    (setq-default term-bind-key-alist
                  '(("C-c C-c" . term-interrupt-subjob)
                    ("C-m" . term-send-raw)
                    ("M-v" . term-send-raw)
                    ("M-f" . term-send-forward-word)
                    ("M-b" . term-send-backward-word)
                    ("M-p" . term-send-up)
                    ("M-n" . term-send-down)
                    ("M-d" . term-send-forward-kill-word)
                    ("C-M-h" . term-send-backward-kill-word)
                    ("M-," . term-send-input)
                    ("M-t" . rl/toggle-term-mode)
                    ("M-o" . other-window)
                    ("C-y" . term-paste)))
    (bind-key "M-t" 'rl/toggle-term-mode term-mode-map)))

;; ------------------------------ exec path
(use-package exec-path-from-shell
  :init
  (progn
    (setq-default exec-path-from-shell-variables
                  '("PATH" "MANPATH" "GOROOT" "GOPATH"))
    (exec-path-from-shell-initialize)))

;; ------------------------------ comint
(use-package comint
  :init
  (setq-default comint-scroll-to-bottom-on-output 'all))

(provide 'setup-terminal)
