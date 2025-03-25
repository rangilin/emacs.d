;;; init-prog.el --- Generic programming related configurations -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous programming settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; show trailing whitespace in prog-mode
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; show document with eldoc
(use-package eldoc
  :diminish eldoc-mode)



;;;;;;;;;;;;;;;;;;;
;; Source Contrl ;;
;;;;;;;;;;;;;;;;;;;

(use-package vc
  :config
  ;; only check git
  (setq vc-handled-backends `(Git)))



;; use magit
(use-package magit
  :ensure t
  :pin nongnu
  :bind
  (("C-c v g" . magit-status)
   ("C-c v G" . magit-dispatch-popup))

  :init
  (use-package transient
    :ensure t
    :pin nongnu
    :config
    (setq transient-levels-file (expand-file-name "transient-levels.el" rangi-emacs-cache-directory))
    (setq transient-values-file (expand-file-name "transient-values.el" rangi-emacs-cache-directory))
    (setq transient-history-file (expand-file-name "transient-history.el" rangi-emacs-cache-directory)))

  (use-package with-editor
    :ensure t
    :pin nongnu)

  :config
  (setq-default magit-diff-refine-hunk t))



;;;;;;;;;;;;;;
;; Web Mode ;;
;;;;;;;;;;;;;;

;; web-mode
(use-package web-mode
  :ensure t
  :pin nongnu
  :mode (("\\.html\\'" . web-mode)
         ("\\.gohtml\\'" . web-mode)
         ("\\.erb\\'" . web-mode))
  :hook (web-mode . rangi-set-web-mode)
  :config
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)

  (defun rangi-set-web-mode ()
    (electric-pair-local-mode -1))

  ;; highlight
  (setq web-mode-enable-current-element-highlight t))



;;;;;;;;;;;;;;;;;
;; Error Check ;;
;;;;;;;;;;;;;;;;;

(use-package flymake
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)
         ("C-c l e" . rangi-flymake-show-buffer-diagnostics))
  :config
  (defun rangi-flymake-show-buffer-diagnostics ()
    (interactive)
    (let ((split-height-threshold 1))
      (flymake-show-buffer-diagnostics)
      (other-window 1))))



(use-package eglot
  :bind (:map eglot-mode-map
	            ("C-c l h" . eldoc)
	            ("C-c l f" . eglot-format)
	            ("C-c l r" . eglot-rename))
  :hook ((eglot-managed-mode . rangi-set-eglot-managed-mode))
  :config

  ;; Make eglot/eldoc works better with flymake diagnostics.
  ;; https://github.com/joaotavora/eglot/discussions/898
  (defun rangi-set-eglot-managed-mode ()
    (setq eldoc-documentation-functions
          (cons #'flymake-eldoc-function (remove #'flymake-eldoc-function eldoc-documentation-functions)))
    (setq eldoc-documentation-strategy #'eldoc-documentation-compose))

  (setq eglot-autoshutdown t))



;;;;;;;;;;;;;;;;;;;;
;; Autocompleting ;;
;;;;;;;;;;;;;;;;;;;;

(use-package company
  :diminish
  :ensure t
  :pin gnu
  :bind
  ("M-." . company-complete)
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-format-margin-function 'company-text-icons-margin)
  (setq company-text-face-extra-attributes '(:weight bold :slant italic))
  (global-company-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous programming modes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rust
(use-package rust-mode
  :ensure t
  :pin nongnu
  :mode (("\\.rs\\'" . rust-mode))
  :hook ((rust-mode . eglot-ensure))
  :config
  (setq rust-format-on-save t)
  (setq rust-mode-treesitter-derive t)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((rust-ts-mode rust-mode) .
                   ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))))



(provide 'init-prog)
