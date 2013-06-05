(require 'generic-x)

(define-generic-mode 'webmacro-mode
  '("##")
  '()
  '(("#if\\|#elseif\\|#else\\|#foreach\\|#begin\\|#end\\|#comment\\|#macro\\|#set\\|#include" . 'font-lock-keyword-face))
  '("\\.wm[m]?\'")
  nil
  "A generic mode for Webmacro"
  )

(provide 'webmacro-mode)
