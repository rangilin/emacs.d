(add-to-list 'load-path user-emacs-directory)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'defun)
(require 'config)
(require 'use-package)

(require 'keybinding)

(use-package ace-jump-mode
  :bind (("C-;" . ace-jump-mode)))

(use-package browse-kill-ring
  :bind (("C-S-y" . browse-kill-ring)))

(use-package color-moccur
  :bind ("C-o" . moccur))

(use-package dabbrev-highlight
  :load-path "site-lisp/dabbrev-highlight")

(use-package duplicate-thing
  :bind ("C-c d" . duplicate-thing))

(use-package eshell
  :bind ("C-S-z" . eshell))

(use-package exec-path-from-shell
  :init
  (progn
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs
     '("LANG" "LC_CTYPE" "JAVA_HOME" "CATALINA_HOME" "CATALINA_PID" "M2_HOME"))))

(use-package expand-region
  :bind (("C-'" . er/expand-region)
         ("C-\"" . er/contract-region)))

(use-package flx-ido
  :init (flx-ido-mode 1)
  :config
  (progn
    ;; disable ido faces so can see flx highlights
    (setq ido-use-faces nil)))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (progn
    (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
    (setq-default flycheck-emacs-lisp-load-path load-path)))

(use-package flycheck-cask
  :init (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    (defun ibuffer-ido-find-file ()
      "Like `ido-find-file', but default to the directory of the buffer at point."
      (interactive
       (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                  (if (buffer-live-p buf)
                                      (with-current-buffer buf
                                        default-directory)
                                    default-directory))))
         (ido-find-file-in-dir default-directory))))
    (bind-key "C-x C-f" 'ibuffer-ido-find-file ibuffer-mode-map)))

(use-package ido
  :init (ido-mode 1)
  :config
  (progn
    (setq ido-enable-flex-matching t)
    (setq ido-use-virtual-buffers t)
    (setq ido-create-new-buffer 'always)
    (setq ido-case-fold t)
    (setq ido-everywhere t)
    (defadvice ido-find-file (after find-file-sudo activate)
      "Find file as root if necessary."
      (unless (and buffer-file-name
                   (file-writable-p buffer-file-name))
        (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))))

(use-package ido-ubiquitous
  :init (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :init (ido-vertical-mode))

(use-package js-mode
  :mode ("\\.json$" . js-mode))

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :init
  (progn
    (add-hook 'js2-mode-hook '(lambda () (setq mode-name "js2")))))

(use-package linum
  :init
  (progn
    (defun linum-on ()
      (unless (or (minibufferp)
                  (member major-mode '(eshell-mode text-mode dired-mode))
                  (string-match "*" (buffer-name)))
        (linum-mode 1)))
    (global-linum-mode 1)))

(use-package magit)

(use-package misc
  :bind (("M-Z" . zap-to-char)
         ("M-z" . zap-up-to-char)))

(use-package monky
  :config
  (progn
    (setq monky-process-type 'cmdserver)))

(use-package move-text
  :bind (("M-P" . rangi-move-text-up)
         ("M-N" . rangi-move-text-down)))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-c C->" . mc/mark-more-like-this-extended)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package nxml-mode
  :mode ("\\.zul$" . nxml-mode)
  :config
  (progn
    (bind-key "M-h" nil nxml-mode-map)
    (setq nxml-slash-auto-complete-flag t)))

(use-package ruby-mode
  :init
  (progn
    (bind-key "RET" 'reindent-then-newline-and-indent ruby-mode-map)
    (bind-key "TAB" 'indent-for-tab-command ruby-mode-map)
    (use-package rvm)
    (use-package rspec-mode
      :config
      (progn
        (defvar rspec-use-rvm t)))
  :config
  (progn
    (custom-set-variables
     '(ruby-insert-encoding-magic-comment nil)))))

(use-package ruby-mode
  :init
  (progn
    (use-package rvm
      :init (rvm-use-default))
    (use-package yari))
  :config
  (progn
    (setq ruby-deep-indent-paren nil)
    (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)))

(use-package shell
  :bind ("C-z" . shell))

(use-package smartparens
  :init
  (progn
    (use-package smartparens-config)
    (use-package smartparens-ruby)
    (use-package smartparens-html)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
  :config
  (progn
    (setq smartparens-strict-mode t)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
    ))

(use-package smex
  :bind ("C-x C-m" . smex))

(use-package undo-tree
  :init
  (progn
    (bind-key "C-/" nil undo-tree-map)
    (bind-key "C-?" nil undo-tree-map)
    (global-undo-tree-mode 1)))

(use-package web-mode
  :mode ("\\.html$" . web-mode)
  :config
  (progn
    (bind-key "C-/" (lambda ()
                      (interactive)
                      (web-mode-comment-or-uncomment)
                      (next-logical-line)) web-mode-map)))

(use-package webmacro-mode
  :load-path "site-lisp/webmacro-mode"
  :mode ("\\.wm[m]?$" . webmacro-mode))

(use-package windmove
  :config (windmove-default-keybindings 'shift))

(use-package yasnippet
  :init
  (progn
    (let ((snippets-dir (f-expand "snippets" user-emacs-directory)))
      (yas/load-directory snippets-dir)
      (setq-default yas/snippet-dirs snippets-dir))
    (yas-global-mode 1)
    (setq-default yas/prompt-functions '(yas/ido-prompt))))

;; -------------------------------------------------- local
(require 'local nil t)
;; -------------------------------------------------- load customization
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
