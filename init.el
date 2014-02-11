(add-to-list 'load-path user-emacs-directory)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'defun)
(require 'font)
(require 'config)
(require 'use-package)

(require 'keybinding)

(use-package ace-jump-mode
  :bind (("C-;" . ace-jump-mode)))

(use-package anzu
  :diminish ""
  :init (global-anzu-mode +1)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))

(use-package browse-kill-ring
  :bind (("C-S-y" . browse-kill-ring)))

(use-package buffer-move
  :bind (("M-S-<up>" . buf-move-up)
         ("M-S-<down>" . buf-move-down)
         ("M-S-<right>" . buf-move-right)
         ("M-S-<left>" . buf-move-left)))

(use-package color-moccur
  :config
  (progn
    (defun moccur-view-file ()
      "my version that will recenter when select an item"
      (if (string= moccur-before-buffer-name moccur-buffer-name)
          (moccur-color-check-view)
        (if moccur-current-line-overlays
            (progn
              (delete-overlay moccur-current-line-overlays)
              (setq moccur-overlays nil)))
        (moccur-color-view))

      (switch-to-buffer-other-window
       (get-buffer moccur-buffer-name))
      (goto-line (string-to-number moccur-line))
      (if (re-search-forward moccur-regexp-color (line-end-position) t)
          ()
        (goto-line (string-to-number moccur-line)))

      ;; color
      (moccur-color-current-line)
      (recenter)

      (setq moccur-before-buffer-name moccur-buffer-name)
      (switch-to-buffer-other-window moccur-mocur-buffer)))
  :bind ("C-o" . moccur))

(use-package css-mode
  :mode ("\\.css$" . css-mode)
  :init
  (progn
    (setq-default css-indent-offset tab-width)))

;; 1.2.0 http://elpa.gnu.org/packages/csv-mode.html
;; no idea why 1.5.0 in marmalade not works
(use-package csv-mode
  :load-path "site-lisp/csv-mode"
  :mode ("\\.csv$" . csv-mode))

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

(use-package eldoc
  :diminish "")

(use-package expand-region
  :bind (("C-'" . er/expand-region)
         ("C-\"" . er/contract-region)))

(use-package fill-column-indicator
  :init
  (progn
    (setq-default fci-rule-column 80)
    (setq-default fci-rule-width 2)
    (add-hook 'prog-mode-hook 'fci-mode)))

(use-package flycheck
  :config
  (progn
    (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
    (add-hook 'after-init-hook 'global-flycheck-mode)))

(use-package flycheck-cask
  :init (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))

;; (use-package flyspell
;;   :diminish ""
;;   :bind
;;   (("C-M-$" . flyspell-buffer)
;;    ("C-$" . flyspell-check-previous-highlighted-word))
;;   :init
;;   (progn
;;     (setq ispell-dictionary "english")
;;     (add-hook 'markdown-mode-hook 'flyspell-mode)
;;     (add-hook 'monky-log-edit-mode-hook 'flyspell-mode)
;;     (add-hook 'prog-mode-hook 'flyspell-prog-mode))
;;   :config
;;   (progn
;;     (bind-key "C-;" nil flyspell-mode-map))) ; reserved for ace jump

(use-package fullscreen-mode
  :init (fullscreen-mode 1))

(use-package gnuplot-mode)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    (define-ibuffer-column readable-size
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 1000) (format "%7.1fK" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size)))))
    (setq-default ibuffer-formats
          '((mark modified read-only " "
                  (name 18 18 :left :elide)
                  " "
                  (readable-size 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  filename-and-process)))
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

(eval-after-load "isearch"
  '(progn
    (defadvice
      isearch-repeat-forward
      (after isearch-repeat-forward-recenter activate)
      (recenter))
    (defadvice
      isearch-repeat-backward
      (after isearch-repeat-backward-recenter activate)
      (recenter))))

(use-package ielm
  :init (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(use-package js
  :mode (("\\.json$" . js-mode)))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
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

(use-package magit
  :init
  (progn
    (defun rl/disable-magit-item-highlight ()
      (face-remap-add-relative 'magit-item-highlight '()))
    (add-hook 'magit-status-mode-hook 'rl/disable-magit-item-highlight)))

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
  :bind (("M-P" . rl/move-text-up)
         ("M-N" . rl/move-text-down)))

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

(use-package nxml-mode
  :mode
  (("\\.zul$" . nxml-mode)
   ("\\.xml$" . nxml-mode))
  :config
  (progn
    (bind-key "M-h" nil nxml-mode-map)
    (setq-default nxml-child-indent tab-width)
    (setq-default nxml-outline-child-indent tab-width)
    (setq nxml-slash-auto-complete-flag t)))

(use-package org
  :mode ("\\.org$" . org-mode)
  :init
  (progn
    (use-package org-plot
      :bind (("C-M-g" . org-plot/gnuplot))))
  :config
  (progn
    (setq-default org-directory "/ramsey/Dropbox/org")
    (setq-default org-special-ctrl-a/e t)))

(use-package projectile
  :init
  (progn
    (projectile-global-mode)
    (setq-default projectile-switch-project-action 'projectile-dired)))

(use-package randomize-region
  :load-path "site-lisp/randomize-region")

(use-package ruby-mode
  :mode (("\\.rake$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode))
  :init
  (progn
    (use-package rvm
      ;; :init (rvm-use-default)
      :config
      (progn
        (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)))
    (use-package rhtml-mode
      :mode (("\\.html\\.erb$" . rhtml-mode)))
    (use-package yari)
    (use-package rspec-mode
      :config
      (progn
        (defvar rspec-use-rvm t)))
    (use-package rinari
      :init (global-rinari-mode 1)
      :config (setq ruby-insert-encoding-magic-comment nil)))
  :config
  (progn
    (bind-key "RET" 'reindent-then-newline-and-indent ruby-mode-map)
    (bind-key "TAB" 'indent-for-tab-command ruby-mode-map)
    (setq ruby-deep-indent-paren nil)
    (custom-set-variables '(ruby-insert-encoding-magic-comment nil))))

(use-package sql
  :init
  (progn
    (use-package sql-indent
      :init
      (progn
        (setq-default sql-indent-offset tab-width)))
    ))

(use-package sh-script
  :init
  (progn
    (custom-set-variables '(sh-basic-offset tab-width))))

(use-package smartparens
  :diminish ""
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

(use-package transpose-frame
  :load-path "site-lisp/transpose-frame")

(use-package undo-tree
  :diminish ""
  :init
  (progn
    (bind-key "C-/" nil undo-tree-map)
    (bind-key "C-?" nil undo-tree-map)
    (global-undo-tree-mode 1)))

(use-package webmacro-mode
  :load-path "site-lisp/webmacro-mode"
  :mode ("\\.wm[m]?$" . webmacro-mode))

(use-package windmove
  :config (windmove-default-keybindings 'meta))

(use-package yasnippet
  :diminish 'yas/minor-mode
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

;; -------------------------------------------------- load customization
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; -------------------------------------------------- local
(require 'local nil t)

;; -------------------------------------------------- after
(kill-buffer "*Compile-Log*")
