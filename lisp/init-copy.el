;;
;; Copy file related info
;; ----------------------------------------------------------------------------
;;

(defun rangi-copy-current-path (&optional prefix)
  "Copy file path of the current buffer"
  (interactive "p")
  (let ((path (rangi-current-path)))
    (if path
        (progn
          (let ((result (rangi-strip-path (/ prefix 4) path)))
            (kill-new result)
            (message "Copied '%s' to the clipboard." result)))
      (message "Not in a file or directory, do nothing"))))


(defun rangi-current-path ()
  "Return full path of current buffer"
  (if (equal major-mode `dired-mode)
      default-directory
    (buffer-file-name)))

(defun rangi-strip-path (index path)
  "Strip path according to index, 1 will return last element of the path,
4 return parent directory, otherwise return path itself"
  (cond
   ((equal 1 index) (rangi-path-base path))
   ((equal 4 index) (file-name-directory (directory-file-name path)))
   (t path)))

(defun rangi-path-base (path)
  "Return last element of path of PATH"
  (let* ((parent-dir (file-name-directory (directory-file-name path))))
    (s-chop-prefix parent-dir (directory-file-name path))))

;; prefix: C-c c
(global-set-key (kbd "C-c c f") 'rangi-copy-current-path)

;; prefix: s-C
(global-unset-key (kbd "s-C"))
(global-set-key (kbd "s-C f") 'rangi-copy-current-path)


;;
;; Clipboard & Kill Ring functions
;; ----------------------------------------------------------------------------
;;

;; make stuff in system clipboard always saved in kill ring
(setq-default save-interprogram-paste-before-kill t)

(global-set-key (kbd "s-<backspace>") 'kill-whole-line)
(global-set-key (kbd "M-S-<backspace>") 'kill-word)
(global-set-key (kbd "s-y") 'yank-pop)

;; allow us to select kill ring content from a list
(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))


;; home-made package tp duplicate lines
(use-package duplicator
  :load-path "site-lisp/duplicator"
  :bind ("M-D" . duplicator/duplicate-lines))


;;
;; Sequence Generate
;; ----------------------------------------------------------------------------
;;

;; generate linear ranges
(use-package tiny
  :config
  (tiny-setup-default))



(provide 'init-copy)
