(require 'use-package)
(require 'yasnippet)

(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.blade.php\\'" . web-mode)
         ("\\.[gj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.tmpl\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :config
  (progn
    (setq-default web-mode-markup-indent-offset tab-width)
    (setq-default web-mode-css-indent-offset tab-width)
    (setq-default web-mode-code-indent-offset tab-width)
    (setq-default web-mode-sql-indent-offset tab-width)

    (add-hook 'before-save-hook 'delete-trailing-whitespace)

    (bind-key "C-_" 'undo-tree-undo web-mode-map)
    (bind-key "M-_" 'undo-tree-redo web-mode-map)
    (bind-key "C-x u" 'undo-tree-visualize web-mode-map)
    (bind-key "C-/" 'web-mode-comment-or-uncomment web-mode-map)

    (defun rangi/web-mode-hook ()
      (toggle-truncate-lines)
      (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
      (when (stringp buffer-file-name)
        (if (string-match "\\.php\\'" buffer-file-name)
            (yas-activate-extra-mode 'php-mode))
        (if (or (string-match "\\.blade.php\\'" buffer-file-name)
                (string-match "\\.tmpl\\'" buffer-file-name))
            (setq web-mode-enable-auto-pairing nil))))

    (add-hook 'web-mode-hook 'rangi/web-mode-hook)))

(provide 'setup-web-mode)
