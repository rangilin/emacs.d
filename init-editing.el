(setq-default
 show-trailing-whitespace t
 indent-tabs-mode nil
 tab-width 4
 line-number-mode 1
 column-number-mode 1
 x-select-enable-clipboard t
 line-spacing 0.1)


;;----------------------------------------------------------------------------
;; delete/replace active region when typing
;;----------------------------------------------------------------------------
(delete-selection-mode 1)

;;----------------------------------------------------------------------------
;; Auto refresh buffers
;;----------------------------------------------------------------------------
(global-auto-revert-mode 1)
;; also refresh non-file buffer like dired
(setq global-auto-revert-non-file-buffers t)
;; do it sliently
(setq auto-revert-verbose nil)

(provide 'init-editing)
