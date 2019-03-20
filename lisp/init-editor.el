;;
;; Random stuff
;;----------------------------------------------------------------------------
;;

;; warn when open file larger than 100MB
(setq large-file-warning-threshold 100000000)

;; highlight matching parentheses
(show-paren-mode 1)

;; pairing parenthesis automatically
(electric-pair-mode 1)

;; show keystrokes right away
(setq echo-keystrokes 0.1)

;; no startup messages
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message nil)

;; no start up screen
(setq inhibit-startup-screen t)

;; hide cursor in inactive windows
(setq cursor-in-non-selected-windows t)

;; make scratch buffer empty
(setq initial-scratch-message nil)

;; open scratch buffer in text mode
(setq initial-major-mode 'text-mode)

;; sentence end after one space line
(setq sentence-end-double-space nil)

;; ask before quit emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; select help window automatically, so it is easier to close it with `q`
(setq help-window-select t)

;; force ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; remove selected text when inserting new text
(delete-selection-mode 1)

;; toggle truncate lines
(global-set-key (kbd "C-c e t") 'toggle-truncate-lines)

;; move cursor to top or bottom of the buffer when it cannot be scrolled anymore
(setq-default scroll-error-top-bottom t)



;;
;; Whitespace
;;----------------------------------------------------------------------------
;;


;; show trailing whitespace in prog-mode
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; hide trailing whiespace in minibuffer
(add-hook 'minibuffer-inactive-mode-hook (lambda () (setq show-trailing-whitespace nil)))



;;
;; Fonts
;;----------------------------------------------------------------------------
;;

;;
;; Match the size of English font and CJK font so they align.
;;
;; Modified based on
;; https://github.com/coldnew/coldnew-emacs
;;
;;
;; A quick test to show whether characters are aligned:
;;
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa (40 chars)
;; 測測測測測測測測測測測測測測測測測測測測 (20 chars)
;; あいうえおあいうえおあいうえおあいうえお (20 chars)
;; 한한한한한한한한한한한한한한한한한한한한 (20 chars) (not aligned ATM)
;;


;; predefined font sets

(defvar rangi-font-alist-hack-hiragino-sans
  '((english-font . "Hack")
    (cjk-font . "Hiragino Sans GB W3")
    (default-size-pair . (14 . 16))
    (size-pairs . ((10 . 12) (12 . 14) (14 . 16) (15 . 18) (16 . 20) (18 . 22) (20 . 24)
                   (22 . 26) (24 . 28) (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44)))))

(defvar rangi-font-alist-monaco-hiragino-sans
  '((english-font . "Monaco")
    (cjk-font . "Hiragino Sans GB W3")
    (default-size-pair . (12 . 14))
    (size-pairs . ((10 . 12) (12 . 14) (14 . 16) (15 . 18) (16 . 20) (18 . 22) (20 . 24)
                   (22 . 26) (24 . 28) (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44)))))

(defvar rangi-font-alist-source-code-pro-hiragino-sans
  '((english-font . "Source Code Pro")
    (cjk-font . "Hiragino Sans GB W3")
    (default-size-pair . (14 . 16))
    (size-pairs . ((10 . 12) (12 . 14) (14 . 16) (15 . 18) (16 . 20) (18 . 22) (20 . 24)
                   (22 . 26) (24 . 28) (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44)))))

(defvar rangi-font-alist-menlo-hiragino-sans
  '((english-font . "Menlo")
    (cjk-font . "Hiragino Sans GB W3")
    (default-size-pair . (14 . 16))
    (size-pairs . ((10 . 12) (12 . 14) (14 . 16) (15 . 18) (16 . 20) (18 . 22) (20 . 24)
                   (22 . 26) (24 . 28) (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44)))))


(defvar rangi-font-alist rangi-font-alist-hack-hiragino-sans "Current font set")
(defvar rangi-font-size-pair (cdr (assoc 'default-size-pair rangi-font-alist)) "Current font size pair")


(defun rangi-font-exist-p (fontname)
  "Test if this font is exist or not.
This function only work on GUI mode, on terminal it just
return nil since you can't set font for emacs on it."
  (if (or (not fontname) (string= fontname "") (not (display-graphic-p)))
      nil
    (if (not (x-list-fonts fontname))
        nil t)))


(defun rangi-set-font-size (size-pair)
  "Set size of current font set"
  (message "font size set to (%d . %d)" (car size-pair) (cdr size-pair))
  (let ((english (cdr (assoc 'english-font rangi-font-alist)))
        (cjk (cdr (assoc 'cjk-font rangi-font-alist))))

    (if (rangi-font-exist-p english)
        (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t)
      (message "font %s does not exist" english))

    (if (rangi-font-exist-p cjk)
        (dolist (charset '(kana han hangul cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font) charset
                            (font-spec :family cjk :size (cdr size-pair))))
      (message "font %s does not exist" cjk))))


(defun rangi-step-font-size (step)
  "Increase/Decrease emacs's font size."
  (let ((scale-steps (assoc 'size-pairs rangi-font-alist)))
    (if (< step 0) (setq scale-steps (reverse scale-steps)))
    (setq rangi-font-size-pair
          (or (cadr (member rangi-font-size-pair scale-steps))
              rangi-font-size-pair))
    (when rangi-font-size-pair
      (rangi-set-font-size rangi-font-size-pair))))


(defun rangi-increase-emacs-font-size ()
  "Decrease emacs's font size acording font set."
  (interactive)
  (rangi-step-font-size 1))

(defun rangi-decrease-emacs-font-size ()
  "Increase emacs's font size acording font set."
  (interactive)
  (rangi-step-font-size -1))

(defun rangi-reset-emacs-font-size ()
  "Reset emacs's font size to default size of current font set."
  (interactive)
  (let ((pair (cdr (assoc 'default-size-pair rangi-font-alist))))
    (setq rangi-font-size-pair pair)
    (rangi-set-font-size pair)))

(global-set-key (kbd "C-=") 'rangi-increase-emacs-font-size)
(global-set-key (kbd "C--") 'rangi-decrease-emacs-font-size)
(global-set-key (kbd "C-\\") 'rangi-reset-emacs-font-size)

(rangi-set-font-size rangi-font-size-pair)


;; allow emacs to display most unicode emoji properly
(set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)





(provide 'init-editor)
