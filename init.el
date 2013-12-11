(add-to-list 'load-path user-emacs-directory)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'defun)
(require 'config)
(require 'use-package)

(require 'keybinding)

(use-package ace-jump-mode
  :bind (("C-;" . ace-jump-mode)))

(use-package anzu
  :init (global-anzu-mode +1)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))

(use-package browse-kill-ring
  :bind (("C-S-y" . browse-kill-ring)))

(use-package color-moccur
  :bind ("C-o" . moccur))

(use-package dabbrev-highlight
  :load-path "site-lisp/dabbrev-highlight")

(use-package duplicate-thing
  :bind ("C-c d" . duplicate-thing))

(use-package emacs-lisp-mode
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)))
  :mode ("Cask" . emacs-lisp-mode))

(use-package exec-path-from-shell
  :init
  (progn
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs
     '("LANG" "LC_CTYPE" "JAVA_HOME" "CATALINA_HOME" "CATALINA_PID" "M2_HOME"))))

(use-package expand-region
  :bind (("C-'" . er/expand-region)
         ("C-\"" . er/contract-region)))

(use-package fill-column-indicator
  :init
  (progn
    (setq-default fci-rule-use-dashes t)
    (setq-default fci-rule-width 5)
    (add-hook 'prog-mode-hook 'fci-mode)))

(use-package flycheck
  :config
  (progn
    (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
    (add-hook 'after-init-hook 'global-flycheck-mode)))

(use-package flycheck-cask
  :init (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))

(use-package flyspell
  :bind
  (("C-M-$" . flyspell-buffer)
   ("C-$" . flyspell-check-previous-highlighted-word))
  :init
  (progn
    (setq ispell-dictionary "english")
    (add-hook 'markdown-mode-hook 'flyspell-mode)
    (add-hook 'monky-log-edit-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (progn
    (bind-key "C-;" nil flyspell-mode-map))) ; reserved for ace jump

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
    (use-package ido-ubiquitous
      :init (ido-ubiquitous-mode 1))
    (use-package ido-vertical-mode
      :init (ido-vertical-mode))
    (use-package flx-ido
      :init (flx-ido-mode 1)
      :config
      (progn
        ;; disable ido faces so can see flx highlights
        (setq ido-use-faces nil)))
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


(use-package ielm
  :init (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(use-package js-mode
  :mode ("\\.json$" . js-mode))

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :config
  (progn
    (setq-default js2-basic-offset 2)
    (setq-default js2-bounce-indent-p t)))

(use-package linum
  :init
  (progn
    (defun linum-on ()
      (unless (or (minibufferp)
                  (member major-mode '(shell-mode eshell-mode text-mode dired-mode))
                  (string-match "*" (buffer-name)))
        (linum-mode 1)))
    (global-linum-mode 1)))

(use-package magit)

(use-package markdown-mode
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)))

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

(use-package moz
  :load-path "site-lisp/moz"
  :init
  (progn
    (defun rl/moz-start () (moz-minor-mode 1))
    (defun rl/moz-reload-firefox ()
      "Reload current tab of firefox if moz minor mode is enabled"
      (interactive)
      (if (and (boundp 'moz-minor-mode) moz-minor-mode)
          (process-send-string (inferior-moz-process) "BrowserReload()\n")))
    (defun rl/moz-hook-reload-after-save ()
      "Add hook to reload firefox on save"
      (interactive)
      (add-hook 'after-save-hook 'rl/moz-reload-firefox))
    (add-hook 'html-mode-hook 'rl/moz-hook-reload-after-save)
    (add-hook 'css-mode-hook 'rl/moz-hook-reload-after-save)
    (bind-key "C-c r" 'rl/moz-reload-firefox)))

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

(use-package org
  :config
  (progn
    (setq-default org-special-ctrl-a/e t)))

(use-package projectile
  :init
  (progn
    (projectile-global-mode)
    (setq-default projectile-switch-project-action 'projectile-dired)))

(use-package ruby-mode
  :init
  (progn
    (bind-key "RET" 'reindent-then-newline-and-indent ruby-mode-map)
    (bind-key "TAB" 'indent-for-tab-command ruby-mode-map)
    (use-package rvm
      :config
      (progn
        (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)))
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
    (use-package yari)
    (use-package rhtml-mode
      :mode (("\\.html\\.erb$" . rhtml-mode)))
    (use-package rinari
      :init (global-rinari-mode 1)
      :config (setq ruby-insert-encoding-magic-comment nil))
    (use-package rspec-mode
      :config
      (progn
        (setq-default rspec-use-rvm t)
        (setq-default rspec-use-rake-flag nil))))
  :config
  (progn
    (setq ruby-deep-indent-paren nil)
    (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)))

(use-package sql
  :init
  (progn
    (use-package sql-indent
      :init
      (progn
        (setq-default sql-indent-offset tab-width)))
    ))

(use-package shell
  :config
  (progn
    (add-hook 'shell-mode-hook 'shell-window-resize-hook)
    (bind-key "C-c C-z" 'shell comint-mode-map)))

(use-package smart-mode-line
  :init
  (progn
    (setq sml/theme 'dark)
    (sml/setup)))

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
    (sp-local-pair 'nxml-mode "<" nil :actions :rem)))

(use-package smex
  :bind ("C-x C-m" . smex))

(use-package term
  :config (add-hook 'term-mode-hook '(lambda () (yas-minor-mode -1))))

(use-package undo-tree
  :init
  (progn
    (bind-key "C-/" nil undo-tree-map)
    (bind-key "C-?" nil undo-tree-map)
    (global-undo-tree-mode 1)))

(use-package webmacro-mode
  :load-path "site-lisp/webmacro-mode"
  :mode ("\\.wm[m]?$" . webmacro-mode))

(use-package windmove
  :config (windmove-default-keybindings 'shift))

(use-package yasnippet
  :init
  (progn
    (let ((snippets-dir (f-expand "snippets" user-emacs-directory)))
      (yas-load-directory snippets-dir)
      (setq-default yas/snippet-dirs snippets-dir))
    (yas-global-mode 1)
    (setq-default yas/prompt-functions '(yas/ido-prompt))))

(use-package zencoding-mode
  :init
  (progn
    (bind-key "C-j" nil zencoding-mode-keymap)
    (add-hook 'html-mode-hook 'zencoding-mode)
    (add-hook 'nxml-mode-hook 'zencoding-mode)))

;; -------------------------------------------------- local
(require 'local nil t)
;; -------------------------------------------------- load customization
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
