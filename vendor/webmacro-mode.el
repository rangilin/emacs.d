(require 'generic-x)

(define-generic-mode 'webmacro-mode
  '("##")
  '()
  '(("#if\\|#elseif\\|#else\\|#foreach\\|#begin\\|#end\\|#comment\\|#macro\\|#set\\|#include\\|#text\\|#alternate" . 'font-lock-keyword-face)
    ("\\$[a-zA-Z]+[A-Za-z0-9_]*" . 'font-lock-variable-name-face)
    ("#[a-zA-Z]+[A-Za-z0-9_]*" . 'font-lock-function-name-face))
  '("\\.wm[m]?\'")
  nil
  "A minimum mode for display Webmacro syntax highlight"
  )

(provide 'webmacro-mode)
