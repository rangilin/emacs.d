;;
;; All operation related to cursor, such as moving cursor, mark.
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
(setq-default mc/list-file (expand-file-name ".mc-lists.el" rangi/gen-dir))
(use-package multiple-cursors
  :init
  (progn

    (use-package mc-hide-unmatched-lines-mode
      :init (bind-key "C-:" 'mc-hide-unmatched-lines-mode)))

  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-<" . mc/mark-all-dwim)
         ("C-S-c C->" . mc/mark-more-like-this-extended)
         ("C-c <C-mouse-1>" . mc/add-cursor-on-click)))

;; ------------------------------ sub word
(use-package subword
  :diminish ""
  :init
  (progn
    (global-subword-mode)

    ;; solve issue that subword not support shift selection at my version
    (defadvice subword-forward (before advice-subword-forward activate)
      (handle-shift-selection))
    (defadvice subword-backward (before advice-subword-backward activate)
      (handle-shift-selection))))


;; ------------------------------ mark

;; so I can pop mark multiple time with C-u C-@ C-@...
(setq-default set-mark-command-repeat-pop t)

(bind-key "M-H" 'mark-defun)

;; ------------------------------ ace jump
(use-package ace-jump-mode
  :bind (("C-;" . ace-jump-mode)
         ("M-;" . ace-jump-mode-pop-mark)))

;; ------------------------------ back to indentation or beginning
;; http://www.emacswiki.org/emacs/BackToIndentationOrBeginning
(defun rangi/back-to-indentation-or-beginning ()
  "Back to indentation or beginning of current line"
  (interactive "^")
  ;; workaround in visual line mode
  (if (bound-and-true-p visual-line-mode)
      (beginning-of-visual-line)
      (when (= (point) (progn (back-to-indentation) (point)))
        (beginning-of-line))))

(bind-key "C-a" 'rangi/back-to-indentation-or-beginning)

;; ------------------------------ forward/backward whitespace
;; (defun rangi/backward-whitespace (arg)
;;   (interactive "p")
;;   (forward-whitespace (- arg)))
;;
;; (bind-key "M-F" 'forward-whitespace)
;; (bind-key "M-B" 'rangi/backward-whitespace)

(provide 'setup-cursor)
