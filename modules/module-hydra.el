(defun rl-init-module-hydra ()
  (use-package hydra
    :ensure t
    :init
    :config
    (setq hydra-is-helpful t)
    (hydra-add-font-lock)
    (rl--set-up-hydra-ace)))



(defun rl--set-up-hydra-ace ()
  (defhydra hydra-ace (:color teal :hint nil)
    "
Goto             ^^Duplicate          ^^Move             ^^Kill           ^^Copy
------------------------------------------------------------------------------------
_w_: window        _d_: line            _m_: line          _k_: line        _c_: line
_g_: char          _D_: region          _M_: region        _K_: region      _C_: region
_l_: line
"
    ("w" ace-window)
    ("g" avy-goto-char)
    ("l" avy-goto-line)
    ("d" avy-copy-line)
    ("D" avy-copy-region)
    ("m" avy-move-line)
    ("M" avy-move-region)
    ("k" avy-kill-whole-line)
    ("K" avy-kill-region)
    ("c" avy-kill-ring-save-whole-line)
    ("C" avy-kill-ring-save-region))

  (bind-key "M-g" 'hydra-ace/body))

(provide 'module-hydra)
