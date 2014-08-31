(require 'use-package)

;; ------------------------------ trailing whitespace
;; show trailing whitespace in following mode
(defun rangi/show-trailing-whitespace ()
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'rangi/show-trailing-whitespace)

;; hide trailing whitespace in following mode
(defun rangi/hide-trailing-whitespace ()
  (setq show-trailing-whitespace nil))
(add-hook 'minibuffer-inactive-mode-hook 'rangi/hide-trailing-whitespace)

;; clean trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ------------------------------ ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn

    ;; don't display empty filter groups
    (setq-default ibuffer-show-empty-filter-groups nil)

    ;; my filter groups
    (setq-default ibuffer-saved-filter-groups
                  `(("Default"
                     ("Dired" (mode . dired-mode))
                     ("Terminal" (or (mode . term-mode)
                                     (mode . shell-mode)
                                     (mode . eshell-mode)))
                     ("Help" (or (mode . Man-mode)
                                 (mode . woman-mode)
                                 (mode . info-mode)
                                 (mode . help-mode)))
                     ("Emacs" (or (name . "*Messages*")))
                     ("Temporary" (name . "\*.*\*"))
                     )))

    ;; define a column that display buffer size in readable format
    (define-ibuffer-column readable-size
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 1000) (format "%7.1fK" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size)))))

    ;; setup ibuffer format
    (setq-default ibuffer-formats
                  '((mark modified read-only
                          " " (name 24 24 :left :elide)
                          " " (readable-size 9 -1 :right)
                          " " (mode 16 16 :left :elide)
                          " " filename-and-process)))


    (defun ibuffer-ido-find-file ()
      "Like `ido-find-file', but default to the directory of the buffer at point."
      (interactive
       (let ((default-directory
               (let ((buf (ibuffer-current-buffer)))
                 (if (buffer-live-p buf)
                     (with-current-buffer buf default-directory)
                   default-directory))))
         (ido-find-file-in-dir default-directory))))
    (bind-key "C-x C-f" 'ibuffer-ido-find-file ibuffer-mode-map)

    (defun rangi/ibuffer-mode-hook ()
      (ibuffer-switch-to-saved-filter-groups "Default"))
    (add-hook 'ibuffer-mode-hook 'rangi/ibuffer-mode-hook)

    ))

;; ------------------------------ switch buffer
(bind-key "M-]" 'next-buffer)
(bind-key "M-[" 'previous-buffer)

;; ------------------------------ buffer move
(use-package buffer-move
  :bind (("M-S-<up>" . buf-move-up)
         ("M-S-<down>" . buf-move-down)
         ("M-S-<right>" . buf-move-right)
         ("M-S-<left>" . buf-move-left)))

;; ------------------------------ buffer auto revert
(global-auto-revert-mode 1)
(setq-default global-auto-revert-non-file-buffers t)
(setq-default auto-revert-verbose nil)

;; ------------------------------ default scratch buffer mode
(setq initial-major-mode 'text-mode)

(provide 'setup-buffer)
