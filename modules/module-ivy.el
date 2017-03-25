(defun rl-init-module-ivy ()
  (rl--set-up-ivy)
  (rl--set-up-counsel)
  (rl--set-up-swiper))


(defun rl--set-up-ivy ()
  (use-package ivy
    :ensure t
    :init
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "%d/%d ")
    :config
    (ivy-mode 1)
    (bind-key "C-r" 'ivy-resume)))


(defun rl--set-up-counsel ()
  (use-package counsel
    :ensure t
    :config
    (bind-key "M-x" 'counsel-M-x)
    (bind-key "C-x C-f" 'counsel-find-file)
    (bind-key "C-x C-r" 'counsel-recentf)
    (bind-key "<f1> f" 'counsel-describe-function)
    (bind-key "<f1> v" 'counsel-describe-variable)
    (bind-key "<f1> l" 'counsel-find-library)
    (bind-key "C-c g" 'counsel-git)
    (bind-key "C-c j" 'counsel-git-grep)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))


(defun rl--set-up-swiper ()
  (use-package swiper
    :ensure t
    :config
    (bind-key "C-s" 'swiper)))



(provide 'module-ivy)
