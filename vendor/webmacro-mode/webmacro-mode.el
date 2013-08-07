(defvar webmacro-mode-hook nil)

(defvar webmacro-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for webmacro-mode")

(defvar webmacro-keywords
  (list
   '("##.*\\|#comment \{\\(.*\n\\)+}\\|#comment #begin\\(.*\n\\)+#end" . 'font-lock-comment-face)
   '("#if\\|#elseif\\|#else\\|#foreach\\|#begin\\|#end\\|#comment\\|#macro\\|#set\\|#include\\|#text\\|#alternate" . 'font-lock-keyword-face)
   '("\\$[a-zA-Z]+[A-Za-z0-9_]*" . 'font-lock-variable-name-face)
   '("#[a-zA-Z]+[A-Za-z0-9_]*" . 'font-lock-function-name-face)))

(defun _init-webmacro ()
  (use-local-map webmacro-mode-map)
  (run-hooks 'webmacro-mode-hook)
  (set (make-local-variable 'font-lock-defaults) '(webmacro-keywords))
  (set (make-local-variable 'font-lock-multiline) t)
  (setq comment-start "##"))


(define-derived-mode webmacro-mode fundamental-mode "WM"
  "Webmacro mode is a major mode for editing webmacro template files"
  (_init-webmacro))


(provide 'webmacro-mode)
