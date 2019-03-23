;; stop cursor jumping around while scrolling
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

;; turn off these UIs
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; show column & line number in mode line
(column-number-mode t)
(line-number-mode t)

;; display path of current buffer in the frame title
(setq-default frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))

;; no alarm bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; don't blink cursor
(blink-cursor-mode -1)

;; set up fringe
(fringe-mode '(nil . 10))
(setq-default indicate-buffer-boundaries 'right)
(setq-default indicate-empty-lines t)

;; show current line
(global-hl-line-mode)

;; resize frame by pixel
(setq frame-resize-pixelwise t)

;; adjust scrolling speed of mouse/trackpad
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))



;;
;; Windows
;;----------------------------------------------------------------------------
;;

;; split window vertically if window width is more than 120, otherwise do it horizontally
(setq split-height-threshold nil)
(setq split-width-threshold 120)

;; change window with simpler keystroke
(global-set-key (kbd "s-1") (kbd "C-x 1"))
(global-set-key (kbd "s-2") (kbd "C-x 2"))
(global-set-key (kbd "s-3") (kbd "C-x 3"))
(global-set-key (kbd "s-0") (kbd "C-x 0"))
(global-set-key (kbd "s-w") (kbd "C-x 0"))


;; restore window configurations easiler
(use-package winner
  :bind (("M-s-[" . winner-undo)
         ("M-s-]" . winner-redo)
         ;; workaround for accented characters
         ("M-s-“" . winner-undo)
         ("M-s-‘" . winner-redo))
  :config
  (winner-mode 1))


(use-package windmove
  :config
  (global-set-key (kbd "<C-s-left>")  'windmove-left)
  (global-set-key (kbd "s-[")  'windmove-left)
  (global-set-key (kbd "<C-s-right>") 'windmove-right)
  (global-set-key (kbd "s-]")  'windmove-right)
  (global-set-key (kbd "<C-s-up>")    'windmove-up)
  (global-set-key (kbd "s-{")  'windmove-up)
  (global-set-key (kbd "<C-s-down>")  'windmove-down)
  (global-set-key (kbd "s-}")  'windmove-down))


;;
;; Search Completion
;;----------------------------------------------------------------------------
;;

;; use ivy to do do completion everywhere
(use-package ivy
  :delight
  :bind
  (("s-b" . ivy-switch-buffer)
   ("C-c C-r" . ivy-resume))
  :config
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d "))


;; use swiper to search local file
(use-package swiper
  :config
  :bind
  (("C-s" . swiper)
   ("s-f" . swiper)))

;; use ivy for emacs command
(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("s-P" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("s-o" . counsel-find-file)
   ("M-s-f" . counsel-ag)
   ("M-s-ƒ" . counsel-ag)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)))


(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  ;; use abbrevation file path
  (setq ivy-rich-path-style 'abbrev))



;;
;; Fonts
;;----------------------------------------------------------------------------
;;

;; allow emacs to display most unicode emoji properly
(set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)

;; make font used by variable-pitch face to make it looks better
(set-face-font 'variable-pitch "Noto Sans CJK TC")

;;
;; Match the size of English and CJK fonts so they align.
;;
;; Based on: https://github.com/coldnew/coldnew-emacs
;;
;;
;; A quick test to show whether characters are aligned:
;;
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa (40 chars)
;; 測測測測測測測測測測測測測測測測測測測測 (20 chars)
;; あいうえおあいうえおあいうえおあいうえお (20 chars)
;; 한한한한한한한한한한한한한한한한한한한한 (20 chars) (not aligned by design)
;;

;; predefined font sets

(defvar rangi-font-alist-roboto-and-noto-sans
  '((english-font . "Roboto Mono")
    (cjk-font . "Noto Sans Mono CJK TC")
    (default-size-pair . (14 . 16))
    (size-pairs . ((10 . 12) (12 . 14) (14 . 16) (15 . 18) (16 . 20) (18 . 22) (20 . 24)
                   (22 . 26) (24 . 28) (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44)))))


(defvar rangi-font-alist rangi-font-alist-roboto-and-noto-sans "Current font set")
(defvar rangi-font-size-pair (cdr (assoc 'default-size-pair rangi-font-alist)) "Current font size pair")

(defun rangi-font-exist-p (fontname)
  "Test if this font is exist or not. This function only work on GUI mode,
on terminal it just return nil since you can't set font for emacs on it."
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



;;
;; Themes
;;----------------------------------------------------------------------------
;;

(use-package solarized-theme
  :config
  (load-theme 'solarized-light t))


;; some customizations
(let ((fg (face-attribute 'default :foreground))
      (bg (face-attribute 'default :background)))

  ;; increase mode line height
  (set-face-attribute 'mode-line nil :box `(:line-width 5 :color ,(face-attribute 'mode-line :background)))
  (set-face-attribute 'mode-line-inactive nil :box `(:line-width 5 :color ,(face-attribute 'mode-line-inactive :background)))

  ;; put underline below the font bottomline so mode line looks better
  (setq x-underline-at-descent-line t))


(provide 'init-gui)
