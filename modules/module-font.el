;;
;; Match the size of English font and CJK font, so the character width remains the same.
;;
;; --------------------------------------------------
;; Test whether width of 2 English chars equals to 1 CJK char
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa (40 chars)
;; 測測測測測測測測測測測測測測測測測測測測 (20 chars)
;; あいうえおあいうえおあいうえおあいうえお (20 chars)
;; --------------------------------------------------
;; inspired from https://gist.github.com/coldnew/7398845


;; predefined English and CJK font combinations

(defvar rl-font-alist-hack-hiragino-sans
  '((english-font . "Hack")
    (cjk-font . "Hiragino Sans GB W3")
    (default-size-pair . (13 . 16))
    (size-pairs . ((10 . 12) (11 . 14) (13 . 16) (15 . 18) (17 . 20) (19 . 22) (20 . 24)
                   (21 . 26) (24 . 28) (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44)))))

(defvar rl-font-alist-monaco-hiragino-sans
  '((english-font . "Monaco")
    (cjk-font . "Hiragino Sans GB W3")
    (default-size-pair . (13 . 16))
    (size-pairs . ((10 . 12) (11 . 14) (13 . 16) (15 . 18) (17 . 20) (19 . 22) (20 . 24)
                   (21 . 26) (24 . 28) (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44)))))

(defvar rl-font-alist-menlo-hiragino-sans
  '((english-font . "Menlo")
    (cjk-font . "Hiragino Sans GB W3")
    (default-size-pair . (12 . 14))
    (size-pairs . ((12 . 14) (14 . 16) (15 . 18) (16 . 20) (18 . 22) (20 . 24) (21 . 26)
                   (23 . 28) (25 . 30) (26 . 32) (28 . 34)))))

;; Set up fonts

(defvar rl-font-alist rl-font-alist-hack-hiragino-sans
  "Default font alist")

(defvar rl-font-size-pair (cdr (assoc 'default-size-pair rl-font-alist))
  "Current font size pair, By default it is the default size pair of the default font alist")

(defun rl-init-module-font ()
  (rl-reset-font-size))

(defun rl-set-font (size-pair frame)
  "Setup font of specified FRAME with ENGLISH, CJK font of current alist and  specified SIZE-PAIR"
  (let ((english-font (cdr (assoc 'english-font rl-font-alist)))
        (cjk-font (cdr (assoc 'cjk-font rl-font-alist))))
    (setq rl-font-size-pair size-pair)
    (set-frame-font (format "%s:pixelsize=%d" english-font (car size-pair)) t (list frame))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter frame 'font) charset (font-spec :family cjk-font :size (cdr size-pair)) frame))))


(defun rl-step-font-size (step frame)
  "Increase/Decrease emacs's font size."
    (let ((size-pair (or (cadr (member rl-font-size-pair (rl--get-pairs-steps step)))
                         rl-font-size-pair)))
      (message "Font size set to %s" size-pair)
      (rl-set-font size-pair frame)))

(defun rl-increase-font-size ()
  "Decrease emacs's font-size acording emacs-font-size-pair-list."
  (interactive)
  (if window-system
      (rl-step-font-size 1 (selected-frame))
    (message "Not in GUI Emacs frame, do nothing")))

(defun rl-decrease-font-size ()
  "Increase emacs's font-size according emacs-font-size-pair-list."
  (interactive)
  (if window-system
      (rl-step-font-size -1 (selected-frame))
    (message "Not in GUI Emacs frame, do nothing")))

(defun rl-reset-font-size ()
  (interactive)
  (if window-system
      (rl-set-font (cdr (assoc 'default-size-pair rl-font-alist)) (selected-frame))
    (message "Not in GUI Emacs frame, do nothing")))

(defun rl--get-pairs-steps (step)
  "Get size pairs from current font alist, if step < 0, get reversed pairs"
  (let ((steps (cdr (assoc 'size-pairs rl-font-alist))))
  (if (< step 0)
      (reverse steps)
    steps)))

(bind-key "C-x C-=" 'rl-increase-font-size)
(bind-key "C-x C--" 'rl-decrease-font-size)
(bind-key "C-x C-0" 'rl-reset-font-size)

;; advice so new frame can be setup font size
(defadvice server-create-window-system-frame
  (after set-frame-font-size ())
  "Set custom frame colours when creating the first frame on a display"
  (rl-reset-font-size))



(provide 'module-font)
