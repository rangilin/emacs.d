;; show hotkey when input incomplete command
(use-package which-key
  :delight
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))


;; tldr client
(use-package tldr
  :config
  (setq tldr-directory-path (expand-file-name "tldr/" rangi-generated-files-directory)))


;; csv mode
(use-package csv-mode
  :mode "\\.csv\\'"
  :config
  (setq csv-separators '("," ";" "|" " " "\t")))


(use-package lorem-ipsum
  :config
  (lorem-ipsum-use-default-bindings))


(use-package synosaurus
  :bind (("C-c e s l" . synosaurus-lookup)
         ("C-c e s r" . synosaurus-choose-and-replace)
         ("C-c e s i" . synosaurus-choose-and-insert))
  :config
  (setq synosaurus-choose-method 'ivy-read)
  (setq synosaurus-backend 'synosaurus-backend-wordnet))


(use-package ag
  :bind (("s-F" . ag)
         ("C-c s s" . ag)
         ("C-c s f" . ag-dired))
  :config
  (setq ag-highlight-search t))



(provide 'init-misc)
