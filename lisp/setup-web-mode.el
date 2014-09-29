(require 'use-package)
(require 'yasnippet)

(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.[gj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :config
  (progn
    (autopair-mode -1) ;; use web-mode's auto pairing
    (setq-default web-mode-markup-indent-offset tab-width)
    (setq-default web-mode-css-indent-offset tab-width)
    (setq-default web-mode-code-indent-offset tab-width)

    (bind-key "C-_" 'undo-tree-undo web-mode-map)
    (bind-key "M-_" 'undo-tree-redo web-mode-map)
    (bind-key "C-x u" 'undo-tree-visualize web-mode-map)
    (bind-key "C-/" 'web-mode-comment-or-uncomment web-mode-map)

    (defun rangi/web-mode-hook ()
      (when (stringp buffer-file-name)
          (cond ((string-match "\\.php\\'" buffer-file-name)
                 (yas-activate-extra-mode 'php-mode)))))

    (add-hook 'web-mode-hook 'rangi/web-mode-hook)))

(provide 'setup-web-mode)
