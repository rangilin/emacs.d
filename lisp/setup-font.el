(require 'use-package)

;; --------------------------------------------------
;; Test whether width of 2 English chars equals to 1 CJK char
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa (40 chars)
;; 測測測測測測測測測測測測測測測測測測測測 (20 chars)
;; あいうえおあいうえおあいうえおあいうえお (20 chars)
;; --------------------------------------------------

; inspired from https://gist.github.com/coldnew/7398845
;; -------------------------------------------------- pre-made font alist

;; ------------------------------ linux
(defvar rangi-font-alist-monaco-wqymh
  '((english-font . "Monaco")
    (cjk-font . "WenQuanYi Micro Hei")
    (default-size-pair . (15 . 18))
    (size-pairs . ((13 . 16) (15 . 18) (17 . 20) (19 . 22) (20 . 24) (21 . 26) (24 . 28)
                   (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44)))))

(defvar rangi-font-alist-consolas-wqymh
  '((english-font . "Consolas")
    (cjk-font . "WenQuanYi Micro Hei")
    (default-size-pair . (17 . 18))
    (size-pairs . ((15 . 16) (17 . 18) (19 . 20) (20 . 22) (21 . 24) (24 . 26)
                   (26 . 28) (28 . 30) (30 . 34) (34 . 38) (36 . 40)))))

(defvar rangi-font-alist-inconsolata-wqymh
  '((english-font . "Inconsolata")
    (cjk-font . "WenQuanYi Micro Hei")
    (default-size-pair . (20 . 20))
    (size-pairs . ((15 . 16) (17 . 18) (19 . 20) (20 . 20) (21 . 22) (24 . 24)
                   (26 . 26) (28 . 28) (30 . 30) (34 . 34) (36 . 36)))))

(defvar rangi-font-alist-source-code-pro-wqymh
  '((english-font . "Source Code Pro")
    (cjk-font . "WenQuanYi Micro Hei")
    (default-size-pair . (19 . 22))
    (size-pairs . ((15 . 18) (17 . 20) (19 . 22) (20 . 24) (21 . 26) (24 . 28)
                   (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44)))))

(defvar rangi-font-alist-ubuntu-mono-wqymh
  '((english-font . "Ubuntu Mono")
    (cjk-font . "WenQuanYi Micro Hei")
    (default-size-pair . (19 . 20))
    (size-pairs . ((15 . 16) (17 . 18) (19 . 20) (21 . 22) (24 . 24)
                   (26 . 26) (28 . 28) (30 . 30) (34 . 34) (36 . 36)))))

(defvar rangi-font-alist-dejavu-sans-mono-wqymh
  '((english-font . "DejaVu Sans Mono")
    (cjk-font . "WenQuanYi Micro Hei")
    (default-size-pair . (19 . 22))
    (size-pairs . ((15 . 18) (17 . 20) (19 . 22) (20 . 24) (21 . 26) (24 . 28)
                   (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44)))))

(defvar rangi-font-alist-droid-sans-mono-wqymh
  '((english-font . "Droid Sans Mono")
    (cjk-font . "WenQuanYi Micro Hei")
    (default-size-pair . (19 . 22))
    (size-pairs . ((15 . 18) (17 . 20) (19 . 22) (20 . 24) (21 . 26) (24 . 28)
                   (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44)))))

(defvar rangi-font-alist-anonymous-pro-wqymh
  '((english-font . "Anonymous Pro")
    (cjk-font . "WenQuanYi Micro Hei")
    (default-size-pair . (21 . 22))
    (size-pairs . ((15 . 16) (17 . 18) (19 . 20) (21 . 22) (24 . 26)
                   (26 . 28) (28 . 30) (30 . 32) (34 . 38) (36 . 40)))))

(defvar rangi-font-alist-input-mono-wqymh
  '((english-font . "Input Mono")
    (cjk-font . "WenQuanYi Micro Hei")
    (default-size-pair . (17 . 22))
    (size-pairs . ((15 . 20) (17 . 22) (19 . 24) (20 . 26) (21 . 26) (24 . 30)
                   (26 . 34) (28 . 36) (30 . 38) (34 . 44) (36 . 46)))))

;; ------------------------------ mac

(defvar rangi-font-alist-monaco-lehei
  '((english-font . "Monaco")
    (cjk-font . "LiHei Pro")
    (default-size-pair . (13 . 16))
    (size-pairs . ((10 . 12) (11 . 14) (13 . 16) (15 . 18) (17 . 20) (19 . 22) (20 . 24)
                   (21 . 26) (24 . 28) (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44)))))

(defvar rangi-font-alist-inconsolata-lehei
  '((english-font . "Inconsolata")
    (cjk-font . "LiHei Pro")
    (default-size-pair . (14 . 14))
    (size-pairs . ((14 . 14) (15 . 16) (17 . 18) (19 . 20) (20 . 20) (21 . 22)
                   (24 . 24) (26 . 26) (28 . 28) (30 . 30) (34 . 34) (36 . 36)))))

(defvar rangi-font-alist-menlo-lehei
  '((english-font . "Menlo")
    (cjk-font . "LiHei Pro")
    (default-size-pair . (14 . 16))
    (size-pairs . ((14 . 16) (15 . 18) (16 . 20) (18 . 22) (20 . 24) (21 . 26)
                   (23 . 28) (25 . 30) (26 . 32) (28 . 34)))))

;; -------------------------------------------------- setup font
(defvar rangi-font-alist rangi-font-alist-consolas-wqymh
  "Default font alist")

(when (string-equal system-type "darwin")
  (setq-default rangi-font-alist rangi-font-alist-menlo-lehei))

(defvar rangi-font-size-pair (cdr (assoc 'default-size-pair rangi-font-alist))
  "Current font size pair, By default it is the default size pair of the default font alist")

(defun rangi-set-font (size-pair frame)
  "Setup font of specified FRAME with ENGLISH, CJK font of current alist and  specified SIZE-PAIR"
  (let ((english-font (cdr (assoc 'english-font rangi-font-alist)))
        (cjk-font (cdr (assoc 'cjk-font rangi-font-alist))))
    (setq rangi-font-size-pair size-pair)
    (set-frame-font (format "%s:pixelsize=%d" english-font (car size-pair)) t (list frame))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter frame 'font) charset (font-spec :family cjk-font :size (cdr size-pair)) frame))))

(defun rangi-step-font-size (step frame)
  "Increase/Decrease emacs's font size."
    (let ((size-pair (or (cadr (member rangi-font-size-pair (rangi--get-pairs-steps step)))
                         rangi-font-size-pair)))
      (message "Font size set to %s" size-pair)
      (rangi-set-font size-pair frame)))

(defun rangi-increase-font-size ()
  "Decrease emacs's font-size acording emacs-font-size-pair-list."
  (interactive)
  (if window-system
      (rangi-step-font-size 1 (selected-frame))
    (message "Not in GUI Emacs frame, do nothing")))

(defun rangi-decrease-font-size ()
  "Increase emacs's font-size according emacs-font-size-pair-list."
  (interactive)
  (if window-system
      (rangi-step-font-size -1 (selected-frame))
    (message "Not in GUI Emacs frame, do nothing")))

(defun rangi-reset-font-size ()
  (interactive)
  (if window-system
      (rangi-set-font (cdr (assoc 'default-size-pair rangi-font-alist)) (selected-frame))
    (message "Not in GUI Emacs frame, do nothing")))

(defun rangi--get-pairs-steps (step)
  "Get size pairs from current font alist, if step < 0, get reversed pairs"
  (let ((steps (cdr (assoc 'size-pairs rangi-font-alist))))
  (if (< step 0)
      (reverse steps)
    steps)))

(bind-key "C-x C-=" 'rangi-increase-font-size)
(bind-key "C-x C--" 'rangi-decrease-font-size)
(bind-key "C-x C-0" 'rangi-reset-font-size)

;; advice so new frame can be setup font size
(defadvice server-create-window-system-frame
  (after set-frame-font-size ())
  "Set custom frame colours when creating the first frame on a display"
  (rangi-reset-font-size))

(rangi-reset-font-size)

(provide 'setup-font)
