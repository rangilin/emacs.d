(add-to-list 'load-path (expand-file-name "vendor/webmacro-mode" user-emacs-directory))

(require 'webmacro-mode)

;; active on files with .wm .wmm extension
(add-to-list 'auto-mode-alist '("\\.wm[m]?\\'" . webmacro-mode))

(provide 'init-webmacro-mode)

