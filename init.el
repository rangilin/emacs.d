(add-to-list 'load-path user-emacs-directory)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'defun)
(require 'config)
(require 'use-package)
(require 'keybinding)

(use-package duplicate-thing
  :bind ("C-c d" . duplicate-thing))

(use-package undo-tree
  :init
  (progn
    (bind-key "C-/" nil undo-tree-map)
    (bind-key "C-?" nil undo-tree-map)
    (global-undo-tree-mode 1)))

(use-package move-text
  :bind (("M-P" . rangi-move-text-up)
         ("M-N" . rangi-move-text-down)))

(use-package expand-region
  :bind (("C-'" . er/expand-region)
         ("C-\"" . er/contract-region)))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-c C->" . mc/mark-more-like-this-extended)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package browse-kill-ring
  :bind (("C-S-y" . browse-kill-ring)))

(use-package dabbrev-highlight
  :load-path "site-lisp/dabbrev-highlight")

(use-package ace-jump-mode
  :bind (("C-;" . ace-jump-mode)))

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

(use-package flx-ido
  :init (flx-ido-mode 1)
  :config
  (progn
    ;; disable ido faces so can see flx highlights
    (setq ido-use-faces nil)))

(use-package ido-vertical-mode
  :init (ido-vertical-mode))

(use-package ido-ubiquitous
  :init (ido-ubiquitous-mode 1))

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

(use-package exec-path-from-shell
  :init
  (progn
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs
     '("LANG" "LC_CTYPE" "JAVA_HOME" "CATALINA_HOME" "CATALINA_PID" "M2_HOME"))))

(use-package eshell
  :init
  (progn
    (defvar eshell-prompt-function
      (lambda ()
        (concat (eshell-user-name) "@" (system-name) ": "
                (abbreviate-file-name (eshell/pwd))
                (if (= (user-uid) 0) "\n# " "\n$ ")))))
  :bind ("C-S-z" . eshell))

(use-package shell
  :bind ("C-z" . shell))

(use-package magit)

(use-package monky
  :config
  (progn
    (setq monky-process-type 'cmdserver)))

(use-package web-mode
  :config
  (progn
    (bind-key "C-/" (lambda ()
                      (interactive)
                      (web-mode-comment-or-uncomment)
                      (next-logical-line)) web-mode-map))
  :mode ("\\.html$" . web-mode))

(use-package webmacro-mode
  :load-path "site-lisp/webmacro-mode"
  :mode ("\\.wm[m]?$" . webmacro-mode))

(setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
(use-package yasnippet
  :init (yas-global-mode 1))

(use-package autopair
  :init (autopair-global-mode))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (progn
    (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
    (setq-default flycheck-emacs-lisp-load-path load-path)))

(use-package misc
  :bind (("M-Z" . zap-to-char)
         ("M-z" . zap-up-to-char)))

(use-package color-moccur
  :bind ("C-o" . moccur))

(use-package nxml-mode
  :mode ("\\.zul$" . nxml-mode)
  :config
  (progn
    (setq nxml-slash-auto-complete-flag t)))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package smex
  :bind ("C-x C-m" . smex))


;; -------------------------------------------------- local
(require 'local nil t)
;; -------------------------------------------------- load customization
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
