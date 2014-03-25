(require 'use-package)

;; --------------------------------------------------
;; Test whether width of 2 English chars equals to 1 CJK char
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa (40 chars)
;; 測測測測測測測測測測測測測測測測測測測測 (20 chars)
;; あいうえおあいうえおあいうえおあいうえお (20 chars)
;; --------------------------------------------------

; from https://gist.github.com/coldnew/7398845

(defvar emacs-english-font "Consolas")
(defvar emacs-cjk-font "WenQuanYi Micro Hei")
(defvar emacs-font-size-pair '(17 . 20)
  "Default font size pair for (english . chinese)")
(defvar emacs-font-size-pair-list
  '(( 5 . 6) (10 . 12)
    (13 . 16) (15 . 18) (17 . 20)
    (19 . 22) (20 . 24) (21 . 26)
    (24 . 28) (26 . 32) (28 . 34)
    (30 . 36) (34 . 40) (36 . 44))
  "This list is used to store matching (englis . chinese) font-size.")

(defun font-exist-p (fontname)
  "test if this font is exist or not."
  (if (or (not fontname) (string= fontname ""))
      nil
    (if (not (x-list-fonts fontname))
        nil t)))

(defun set-font (english chinese size-pair)
  "Setup emacs English and Chinese font on x window-system."
  (if (font-exist-p english)
      (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))
  (if (font-exist-p chinese)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family chinese :size (cdr size-pair))))))

(defun emacs-step-font-size (step)
  "Increase/Decrease emacs's font size."
  (let ((scale-steps emacs-font-size-pair-list))
    (if (< step 0) (setq scale-steps (reverse scale-steps)))
    (setq emacs-font-size-pair
          (or (cadr (member emacs-font-size-pair scale-steps))
              emacs-font-size-pair))
    (when emacs-font-size-pair
      (message "emacs font size set to %.1f" (car emacs-font-size-pair))
      (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))))

(defun rangi/increase-emacs-font-size ()
  "Decrease emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (emacs-step-font-size 1))

(defun rangi/decrease-emacs-font-size ()
  "Increase emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (emacs-step-font-size -1))

(defun rangi/reset-emacs-font-size ()
  (interactive)
  (setq emacs-font-size-pair '(19 . 22))
  (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))

(rangi/reset-emacs-font-size)

(bind-key "C-x C-=" 'rangi/increase-emacs-font-size)
(bind-key "C-x C--" 'rangi/decrease-emacs-font-size)
(bind-key "C-x C-0" 'rangi/reset-emacs-font-size)

(provide 'setup-font)
