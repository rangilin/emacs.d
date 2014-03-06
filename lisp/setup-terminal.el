(require 'use-package)

(use-package multi-term
  :bind (("C-c t" . multi-term))
  :config
  (progn
    ;; clear recorded keystroke on enter is pressed
    ;; avoid view-lossage to display password
    (defadvice term-send-raw (after clear-recorded-key activate)
      (if (string= (kbd "RET") (this-command-keys))
          (clear-this-command-keys)))

    (defun rl/setup-term-mode ()
                                        ; yas tab not works well with term
      (yas-minor-mode -1))
    (add-hook 'term-mode-hook 'rl/setup-term-mode)

    (defun rl/toggle-term-mode ()
      "Toggle between `term-line-mode' and `term-char-mode', also
enable `read-only-mode' in `term-line-mode' so I won't accidentally
execute something I don't want"
      (interactive)
      (if (term-in-line-mode)
          (progn
            (read-only-mode -1)
            (term-char-mode))
        (progn
          (term-line-mode)
          (read-only-mode 1))))

    (setq-default multi-term-program "/bin/bash")
    (setq-default term-buffer-maximum-size 10000)
    (setq-default term-unbind-key-list
                  `("C-z" "C-x" "C-h" "C-c" "C-y" "<ESC>"))
    (setq-default term-bind-key-alist
                  '(("C-c C-c" . term-interrupt-subjob)
                    ("C-m" . term-send-raw)
                    ("M-f" . term-send-forward-word)
                    ("M-b" . term-send-backward-word)
                    ("M-p" . term-send-up)
                    ("M-n" . term-send-down)
                    ("M-d" . term-send-forward-kill-word)
                    ("C-M-h" . term-send-backward-kill-word)
                    ("M-," . term-send-input)
                    ("M-t" . rl/toggle-term-mode)
                    ("M-o" . other-window)
                    ("C-y" . term-paste)
                    ))

    (bind-key "M-t" 'rl/toggle-term-mode term-mode-map)
    ))

;; ------------------------------ exec path
(use-package exec-path-from-shell
  :init
  (progn
    (setq-default exec-path-from-shell-variables
                  '("PATH" "MANPATH" "GOROOT" "GOPATH"))
    (exec-path-from-shell-initialize)))


(require 'comint)
(setq-default comint-scroll-to-bottom-on-output 'all)

(provide 'setup-terminal)
