;; install counsel will also install ivy & swiper as dependencies
(require-package 'counsel)


;; Ivy
(require 'ivy)
(ivy-mode 1)
(diminish 'ivy-mode)

(setq enable-recursive-minibuffers t)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "%d/%d ")



;; use ivy for all kinds of stuff
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)


(provide 'init-ivy)
