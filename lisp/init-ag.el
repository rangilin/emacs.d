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

(defhydra hydra-dumb-jump (:color pink :columns 3)
  "Dumb Jump"
  ("j" dumb-jump-go "Go")
  ("o" dumb-jump-go-other-window "Other window")
  ("e" dumb-jump-go-prefer-external "Go external")
  ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
  ("i" dumb-jump-go-prompt "Prompt")
  ("l" dumb-jump-quick-look "Quick look")
  ("b" dumb-jump-back "Back")
  ("q" nil "Quit"))

(global-set-key (kbd "C-c j") 'hydra-dumb-jump/body)


(provide 'init-ag)
