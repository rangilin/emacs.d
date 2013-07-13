(require 'webmacro-mode)

;; active on files with .wm .wmm extension
(add-to-list 'auto-mode-alist '("\\.wm[m]?\\'" . webmacro-mode))

(eval-after-load "webmacro-mode"
  '(setq indent-line-function 'insert-tab))

(provide 'init-webmacro-mode)


