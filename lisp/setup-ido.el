(require 'use-package)

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
    (setq ido-file-history (expand-file-name ".ido.last" rangi/gen-dir))
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    (setq ido-case-fold t)
    (setq ido-everywhere t)

    ;; make ido find file try sudo automatically
    (defadvice ido-find-file (after find-file-sudo activate)
      "Find file as root if necessary."
      (unless (and buffer-file-name
                   (file-writable-p buffer-file-name))
        (find-alternate-file (concat "/sudo::" buffer-file-name))))

    (defadvice ido-find-file-other-window (after find-file-other-window-sudo activate)
      "Find file as root if necessary."
      (unless (and buffer-file-name
                   (file-writable-p buffer-file-name))
        (find-alternate-file (concat "/sudo::" buffer-file-name))))

    (defadvice ido-find-file-other-frame (after find-file-other-frame-sudo activate)
      "Find file as root if necessary."
      (unless (and buffer-file-name
                   (file-writable-p buffer-file-name))
        (find-alternate-file (concat "/sudo::" buffer-file-name))))))

(provide 'setup-ido)
