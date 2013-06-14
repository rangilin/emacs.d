(require 'generic-x)

(define-generic-mode 'webmacro-mode
  '("##")
  '()
  '(("#if\\|#elseif\\|#else\\|#foreach\\|#begin\\|#end\\|#comment\\|#macro\\|#set\\|#include\\|#text\\|#alternate" . 'font-lock-keyword-face)
    ("\\$\\w+" . 'font-lock-variable-name-face))
  '("\\.wm[m]?\'")
  nil
  "A generic mode for Webmacro"
  )

(provide 'webmacro-mode)
