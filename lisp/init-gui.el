;; stop cursor jumping around while scrolling
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

;; improve speed when cursor jumping
(setq auto-window-vscroll nil)

;; turn off these UIs
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; disable column & line number for performance
(column-number-mode -1)
(line-number-mode -1)
;; use this when I need to know line number
(bind-key "s-=" 'what-line)

;; allow cursor to act on visual line by default
(visual-line-mode)

;; display path of current buffer in the frame title
(setq-default frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))

;; no alarm bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; don't blink cursor
(blink-cursor-mode -1)

;; set up fringe
(fringe-mode '(nil . 15))
(setq-default indicate-buffer-boundaries 'right)
(setq-default indicate-empty-lines t)

;; resize frame by pixel
(setq frame-resize-pixelwise t)

;; adjust scrolling speed of mouse/trackpad
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))


;;
;; Windows
;;----------------------------------------------------------------------------
;;

;; focus to new window after split
(defun rangi-split-window-below ()
  "split a window below and focus it"
  (interactive)
  (split-window-below)
  (other-window 1))

(defun rangi-split-window-right ()
  "split a window to the right and focus it"
  (interactive)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'rangi-split-window-below)
(global-set-key (kbd "C-x 3") 'rangi-split-window-right)

;; split window vertically if window width is more than 120, otherwise do it horizontally
(setq split-height-threshold nil)
(setq split-width-threshold 120)

;; change window with simpler keystroke
(global-set-key (kbd "C-1") (kbd "C-x 1"))
(global-set-key (kbd "C-2") (kbd "C-x 2"))
(global-set-key (kbd "C-3") (kbd "C-x 3"))
(global-set-key (kbd "C-0") (kbd "C-x 0"))




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
  (global-set-key (kbd "s-}")  'windmove-up)
  (global-set-key (kbd "<C-s-down>")  'windmove-down)
  (global-set-key (kbd "s-{")  'windmove-down))


(use-package transpose-frame)

(defhydra hydra-fwb-arrangement (:color red :hint none)
  "
  Arranging window & buffer & frame ...
  ___: Shrink window horizontally     _j_: Move window down    _J_: Move buffer down    _d_: Rotate frame clockwise
  _+_: Enlarge window horizontally    _k_: Move window up      _K_: Move buffer up      _a_: Rotate frame anticlockwise
  _-_: Shrink window                  _l_: Move window right   _L_: Move buffer right   _w_: Flip frame (up/down)
  _=_: Enlarge window                 _h_: Move window left    _H_: Move buffer left    _s_: Flop frame (left/right)
  ^ ^                                 ^ ^                      ^ ^                      _e_: Transpose frame (diagonally)
  "

  ("_" shrink-window-horizontally)
  ("+" enlarge-window-horizontally)
  ("-" shrink-window)
  ("=" enlarge-window)

  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("h" windmove-left)

  ("J" buf-move-down)
  ("K" buf-move-up)
  ("L" buf-move-right)
  ("H" buf-move-left)

  ("d" rotate-frame-clockwise)
  ("a" rotate-frame-anticlockwise)
  ("w" flip-frame)
  ("s" flop-frame)
  ("e" transpose-frame))


(defhydra hydra-other-window-or-arrangement ()
  "switch between other window, or call arrangement"
  ("<tab>" (other-window 1) "next other window")
  ("S-<tab>" (other-window -1) "previous other window")
  ("a" (hydra-fwb-arrangement/body) "arrange" :color blue))


(defun rangi-next-other-window-or-arrangement ()
  (interactive)
  (other-window 1)
  (hydra-other-window-or-arrangement/body))

(defun rangi-previous-other-window-or-arrangement ()
  (interactive)
  (other-window -1)
  (hydra-other-window-or-arrangement/body))

(bind-key "C-c a" 'hydra-fwb-arrangement/body)
(bind-key "C-c <tab>" 'rangi-next-other-window-or-arrangement)
(bind-key "C-c S-<tab>" 'rangi-previous-other-window-or-arrangement)



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
  (setq ivy-count-format "%d/%d ")
  (setq ivy-height-alist
      '((t
         lambda (_caller)
         (max 5 (min 20 (/ (frame-height) 3)))))))

;; install ivy-hydra for additional hydra keybindings
(use-package ivy-hydra)

;; use swiper to search local file
(use-package swiper)

;; use ivy for emacs commands
(use-package counsel
  :bind
  (("C-s" . counsel-grep-or-swiper)
   ("s-f" . counsel-grep-or-swiper)
   ("M-x" . counsel-M-x)
   ("s-P" . counsel-M-x)
   ("C-x C-f" . rangi-counsel-find-file)
   ("s-o" . rangi-counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char))
  :config

  (setq counsel-grep-swiper-limit 10000000)

  (defun rangi-counsel-find-file (arg)
    (interactive "P")
    (if arg (counsel-file-jump)
      (counsel-find-file))))


(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  ;; use abbrevation file path
  (setq ivy-rich-path-style 'abbrev)
  ;; cache project info
  (ivy-rich-project-root-cache-mode 1)
  ;; don't parse remote buffer
  (setq ivy-rich-parse-remote-buffer nil))



;;
;; Fonts
;;----------------------------------------------------------------------------
;;

;; make font used by variable-pitch face to make it looks better
(set-face-font 'variable-pitch "Noto Sans CJK TC")

;; default line spacing
(setq-default line-spacing 0)

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
;;

;; predefined font sets
(defvar rangi-font-alist-berkeley-and-noto-sans
  '((english-font . "Berkeley Mono")
    (cjk-font . "Noto Sans Mono CJK TC")
    (default-size-pair . (14 . 16))
    (size-pairs . ((10 . 12) (12 . 14) (14 . 16) (15 . 18) (16 . 20) (18 . 22) (20 . 24)
                   (22 . 26) (24 . 28) (26 . 32) (28 . 34) (30 . 36) (34 . 40) (36 . 44)))))


(defvar rangi-font-alist rangi-font-alist-berkeley-and-noto-sans "Current font set")
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
        (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t t)
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


;; enable some ligatures, this configuration is for Berkeley Mono font
;; https://github.com/mickeynp/ligature.el/wiki#berkeley-mono
(use-package ligature
  :config
  (ligature-set-ligatures
   'prog-mode
   '(; Group A
     ".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???"
     ".?" "?." ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"
     ; Group B
     "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->"
     "<-|" "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>"
     "<<=" "=>>" "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||="
     "|=" "//=" "/="
     ; Group C
     "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<"
     "<:<" ">:" ":>" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>"
     "<|>" "<||" "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"
     ; Group D
     "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"
     ; Group E
     "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}"
     "{!--" "//" "///" "!!"
     ; Group F
     "www" "@_" "&&" "&&&" "&=" "~@" "++" "+++" "/\\" "\\/" "_|_" "||"
     ; Group G
     "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~"
     "--" "---"))
  (global-ligature-mode t))


;;
;; Themes & Appearances
;;----------------------------------------------------------------------------
;;

(use-package modus-themes)

(defun rangi-light-theme ()
  (interactive)
  (load-theme 'modus-operandi t)
  (rangi--common-theme))

(defun rangi-dark-theme ()
  (interactive)
  (load-theme 'modus-vivendi t)
  (rangi--common-theme))

(defun rangi-disable-all-themes ()
  "disable all themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defun rangi--common-theme()
  ;; make fatty mode-line, this will override theme's mode line style but that's fine
  (set-face-attribute 'mode-line nil :box `(:line-width 6 :color ,(face-attribute 'mode-line :background)))
  (set-face-attribute 'mode-line-inactive nil :box `(:line-width 6 :color ,(face-attribute 'mode-line-inactive :background))))

(bind-key "C-c e t l" 'rangi-light-theme)
(bind-key "C-c e t d" 'rangi-dark-theme)
(rangi-dark-theme)


;; highlight numbers in prog-mode
(use-package highlight-numbers
  :defer t
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; highlight keywords
(use-package hl-todo
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'markdown-mode-hook #'hl-todo-mode))

;; highlight current line
(global-hl-line-mode)
(use-package lin
  :config
  (lin-global-mode 1))

(provide 'init-gui)
