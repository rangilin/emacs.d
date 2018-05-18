;;;; ag.el
(require-package 'ag)
(require 'ag)

(setq ag-highlight-search t)




;;;; make ag search result editable
(require-package 'wgrep)
(require-package 'wgrep-ag)

(require 'wgrep)
(require 'wgrep-ag)

(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)



;; dumb-jump
(require-package 'dumb-jump)
(require 'dumb-jump)

(setq dumb-jump-selector 'ivy)
(setq dumb-jump-force-searcher 'ag)


(provide 'init-ag)
