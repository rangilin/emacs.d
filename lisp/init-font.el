;;
;; Match the size of English font and CJK font so they align.
;;
;; Modified based on
;; https://github.com/coldnew/coldnew-emacs/blob/master/local-lisp/setup-font.el
;;
;;
;; A quick test to show whether characters are aligned:
;;
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa (40 chars)
;; 測測測測測測測測測測測測測測測測測測測測 (20 chars)
;; あいうえおあいうえおあいうえおあいうえお (20 chars)



;; predefined font sets

(defvar rangi-font-alist-hack-hiragino-sans
  '((english-font . "Hack")
    (cjk-font . "Hiragino Sans GB")
    (default-size-pair . (13 . 16))
    (size-pairs . ((10 . 12) (12 . 14) (13 . 16) (15 . 18) (17 . 20) (19 . 22) (20 . 24)
                   (21 . 26) (24 . 28) (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44)))))


(defvar rangi-font-alist-monaco-hiragino-sans
  '((english-font . "Monaco")
    (cjk-font . "Hiragino Sans GB W3")
    (default-size-pair . (13 . 16))
    (size-pairs . ((10 . 12) (11 . 14) (13 . 16) (15 . 18) (17 . 20) (19 . 22) (20 . 24)
                   (21 . 26) (24 . 28) (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44)))))


(defvar rangi-font-alist rangi-font-alist-hack-hiragino-sans "Current font set")

(defvar rangi-font-size-pair '(13 . 16) "Current font size pair")


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
  (let ((english (cdr (assoc 'english-font rangi-font-alist)))
        (cjk (cdr (assoc 'cjk-font rangi-font-alist))))

    (if (rangi-font-exist-p english)
        (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t)
      (message "font %s does not exist" english))

    (if (rangi-font-exist-p cjk)
        (dolist (charset '(kana han cjk-misc bopomofo))
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
      (message "font size set to %.1f" (car rangi-font-size-pair))
      (rangi-set-font-size rangi-font-size-pair))))


(defun rangi-increase-emacs-font-size ()
  "Decrease emacs's font-size acording font set."
  (interactive) (rangi-step-font-size 1))

(defun rangi-decrease-emacs-font-size ()
  "Increase emacs's font-size acording font set."
  (interactive) (rangi-step-font-size -1))


(global-set-key (kbd "C-=") 'rangi-increase-emacs-font-size)
(global-set-key (kbd "C--") 'rangi-decrease-emacs-font-size)

(rangi-set-font-size rangi-font-size-pair)

(provide 'init-font)
