(require 'functions)
(require 'use-package)

;; http://stackoverflow.com/a/9414763/554279
(defun rangi-copy-current-file-path ()
  "Copy file path of the current buffer"
  (interactive)
  (let ((filename (if (equal major-mode `dired-mode)
                      default-directory
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied buffer file name '%s' to the clipboard." filename))
      (message "Not in a file, do nothing"))))

(bind-key "C-c c f" 'rangi-copy-current-file-path)


(provide 'setup-copy)
