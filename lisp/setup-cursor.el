;;
;; All operation related to cursor, such as moving cursor.
;;
(require 'use-package)
(require 'variables)
(require 'thingatpt)

;; ------------------------------ horizontal recenter
;; http://stackoverflow.com/a/1249665/554279
(defun rangi/horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur) (set-window-hscroll (selected-window) (- cur mid)))))

(bind-key "C-S-l" 'rangi/horizontal-recenter)

;; ------------------------------ multiple cursors
(use-package multiple-cursors
  :init
  (progn
    (setq-default mc/list-file (expand-file-name ".mc-lists.el" rangi/gen-dir)))
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-c C->" . mc/mark-more-like-this-extended)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; ------------------------------ forward/backward by whitespace
(defun rangi/backward-whitespace ()
  (interactive)
  (forward-whitespace -1))

(bind-key "M-F" `forward-whitespace)
(bind-key "M-B" `rangi/backward-whitespace)

;; ------------------------------ back to indentation or beginning
;; http://www.emacswiki.org/emacs/BackToIndentationOrBeginning
;; (defun rangi/back-to-indentation-or-beginning () (interactive)
;;   "Back to indentation or beginning of current line"
;;   (if (= (point) (progn (back-to-indentation) (point)))
;;       (beginning-of-line)))
;; (bind-key "C-a" 'rangi/back-to-indentation-or-beginning)

;; ------------------------------ ace jump
(use-package ace-jump-mode
  :bind (("C-;" . ace-jump-mode)))

;; ------------------------------ forward/backward paragraph
;; http://whattheemacsd.com/setup-html-mode.el-01.html
(defun rangi/forward-paragraph ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-forward " >")
    (unless (search-forward-regexp "^\\s *$" nil t)
      (goto-char (point-max)))))

(defun rangi/backward-paragraph ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-backward " >")
    (unless (search-backward-regexp "^\\s *$" nil t)
      (goto-char (point-min)))))

(bind-key "M-a" 'rangi/backward-paragraph)
(bind-key "M-e" 'rangi/forward-paragraph)

;; ------------------------------ move around a little faster
(bind-key "C-S-p" (lambda () (interactive) (ignore-errors (previous-line 5))))
(bind-key "C-S-n" (lambda () (interactive) (ignore-errors (next-line 5))))
(bind-key "C-S-b" (lambda () (interactive) (ignore-errors (backward-char 5))))
(bind-key "C-S-f" (lambda () (interactive) (ignore-errors (forward-char 5))))

(provide 'setup-cursor)
