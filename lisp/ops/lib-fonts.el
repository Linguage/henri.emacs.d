;;; lib-fonts.el --- Fonts and scaling -*- lexical-binding: t -*-

;;; Code:

(require 'lib-system)
(require 'seq)

(defun henri/font-family-available-p (family)
  "Return non-nil when font FAMILY is available."
  (member family (font-family-list)))

(defun henri/first-available-font (&rest families)
  "Return the first available font from FAMILIES."
  (seq-find #'henri/font-family-available-p families))

(defvar henri--current-font-size nil
  "Last applied `:height' for `default'; seed from `henri-font-default-size'.")

(defun henri--sync-current-font-from-face ()
  (setq henri--current-font-size
        (let ((h (face-attribute 'default :height nil 'default)))
          (cond
           ((integerp h) h)
           ((and (floatp h) (> h 1.0)) (round (* 10 h)))
           (t henri-font-default-size)))))

(defun henri/set-font ()
  "Pick Latin/CJK fonts from `henri/get-os-type' and apply `henri-font-default-size'."
  (let ((os-type (henri/get-os-type))
        (h (and (boundp 'henri-font-default-size) henri-font-default-size)))
    (unless (and h (integerp h)) (setq h 140))
    (cond
     ((eq os-type 'macos)
      (let ((latin-font (henri/first-available-font
                         "Cascadia Code NF"
                         "CaskaydiaCove Nerd Font Mono"
                         "CaskaydiaCove Nerd Font"
                         "JetBrains Mono")))
        (when latin-font
          (set-face-attribute 'default nil :family latin-font :height h)
          (set-face-attribute 'fixed-pitch nil :family latin-font :height 1.0)))
      (when (henri/font-family-available-p "SF Pro Text")
        (set-face-attribute 'variable-pitch nil :family "SF Pro Text" :height 1.0))
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font t charset (font-spec :family "PingFang SC"))))
     ((eq os-type 'windows)
      (set-face-attribute 'default nil :family "Fira Code" :height h)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font t charset (font-spec :family "Microsoft YaHei"))))
     ((eq os-type 'wsl)
      (set-face-attribute 'default nil :family "Cascadia Code PL" :height h)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font t charset (font-spec :family "Microsoft YaHei"))))
     ((eq os-type 'linux)
      (set-face-attribute 'default nil :family "Cascadia Code PL" :height h)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font t charset (font-spec :family "Noto Sans CJK SC"))))
     (t
      (set-face-attribute 'default nil :height h))))
  (henri--sync-current-font-from-face))

(defun henri/font-size-adjust (delta &optional _)
  "Change `default' height by DELTA (in 1/10 pt steps).  With \\[universal-argument], larger step."
  (interactive "p")
  (when (bound-and-true-p henri-big-font-mode)
    (henri-big-font-mode -1))
  (unless henri--current-font-size
    (setq henri--current-font-size henri-font-default-size))
  (let ((step (* delta 10)))
    (setq henri--current-font-size (max 60 (+ henri--current-font-size step)))
    (set-face-attribute 'default nil :height henri--current-font-size)
    (message "[henri] font size: %d" henri--current-font-size)))

(defvar henri--font-height-before-big-font nil
  "`default' :height saved when entering `henri-big-font-mode'.")

(defun henri/font-size-reset ()
  "Reset to `henri-font-default-size' and re-apply `henri/set-font' families."
  (interactive)
  (when (bound-and-true-p henri-big-font-mode)
    (henri-big-font-mode -1))
  (setq henri--current-font-size henri-font-default-size)
  (henri/set-font))

(define-minor-mode henri-big-font-mode
  "Toggle a larger `default' height for presentations."
  :global t
  :group 'henri-theme
  :lighter " BigF"
  (if henri-big-font-mode
      (progn
        (unless henri--current-font-size
          (setq henri--current-font-size henri-font-default-size))
        (setq henri--font-height-before-big-font henri--current-font-size)
        (set-face-attribute 'default nil :height henri-font-big-size))
    (progn
      (set-face-attribute 'default nil
                          :height (or henri--font-height-before-big-font
                                      henri-font-default-size))
      (henri--sync-current-font-from-face))))

(provide 'lib-fonts)
;;; lib-fonts.el ends here
