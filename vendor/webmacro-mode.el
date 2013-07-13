(require 'generic-x)

(defvar webmacro-mode-hook nil)

(defvar webmacro-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for webmacro-mode")

(defun _init-webmacro ()
  (use-local-map webmacro-mode-map)
  (run-hooks 'webmacro-mode-hook))

(define-generic-mode 'webmacro-mode
  '("##")
  '()
  '(("#if\\|#elseif\\|#else\\|#foreach\\|#begin\\|#end\\|#comment\\|#macro\\|#set\\|#include\\|#text\\|#alternate" . 'font-lock-keyword-face)
    ("\\$[a-zA-Z]+[A-Za-z0-9_]*" . 'font-lock-variable-name-face)
    ("#[a-zA-Z]+[A-Za-z0-9_]*" . 'font-lock-function-name-face))
  '("\\.wm[m]?\'")
  (list `_init-webmacro)
  "A minimum mode for display Webmacro syntax highlight"
  )

(provide 'webmacro-mode)
