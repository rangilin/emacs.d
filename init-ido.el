(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)
(setq ido-max-directory-size 100000)

;; sudo edit if no permission
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; -------------------------------------------------- smex
(require-package 'smex)
(setq smex-prompt-string "Smex: ")

;; -------------------------------------------------- flx ido
(require-package 'flx-ido)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; -------------------------------------------------- ido everywhere
;(require-package 'ido-ubiquitous)
;(ido-ubiquitous-mode 1)

;; -------------------------------------------------- ido vertical
(require-package 'ido-vertical-mode)
(require 'ido-vertical-mode)
(ido-vertical-mode)


(provide 'init-ido)
