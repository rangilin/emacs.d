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


(provide 'init-misc)
