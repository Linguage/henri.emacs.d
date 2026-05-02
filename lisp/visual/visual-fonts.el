;;; visual-fonts.el --- Global and Org font helpers -*- lexical-binding: t -*-

;;; Commentary:
;; Fonts, scaling, `henri-big-font-mode', Org CJK fontset/body font, and
;; `henri/org-font-diagnose'.  Keybindings and initial `henri/apply-fonts' run
;; from `visual-components.el' after themes load.  `lisp/ops/lib-fonts.el' is a
;; compatibility shim that requires this file.

;;; Code:

;; Declared in init-custom.el — defaults used if compiled alone.
(defvar henri-font-default-size 140)
(defvar henri-font-big-size 220)

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
      (when (fboundp 'set-fontset-font)
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font t charset (font-spec :family "PingFang SC")))))
     ((eq os-type 'windows)
      (set-face-attribute 'default nil :family "Fira Code" :height h)
      (when (fboundp 'set-fontset-font)
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font t charset (font-spec :family "Microsoft YaHei")))))
     ((eq os-type 'wsl)
      (set-face-attribute 'default nil :family "Cascadia Code PL" :height h)
      (when (fboundp 'set-fontset-font)
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font t charset (font-spec :family "Microsoft YaHei")))))
     ((eq os-type 'linux)
      (set-face-attribute 'default nil :family "Cascadia Code PL" :height h)
      (when (fboundp 'set-fontset-font)
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font t charset (font-spec :family "Noto Sans CJK SC")))))
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

;; -----------------------------------------------------------------------------
;; Org CJK body / headings (themes + removable face remapping)

(defconst henri--org-cjk-serif-candidates
  '("TsangerJinKai02"
    "仓耳今楷02"
    "TsangerJinKai02 W03"
    "仓耳今楷02 W03"
    "仓耳今楷"
    "苍耳今楷 02"
    "Source Han Serif SC"
    "Source Han Serif TC"
    "Source Han Serif"
    "Noto Serif CJK SC"
    "Noto Serif CJK TC"
    "Noto Serif CJK JP"
    "Noto Serif CJK HK"
    "Songti SC"
    "Songti TC"
    "Kaiti SC"
    "Kaiti TC"
    "STSong"
    "STKaiti")
  "Org 正文候选（越靠前越优先）。未安装则自动跳过。")

(defconst henri--org-cjk-sans-candidates
  '("PingFang SC"
    "SF Pro Text"
    "Helvetica Neue"
    "Heiti SC"
    "Noto Sans CJK SC"
    "Noto Sans CJK TC"
    "Microsoft YaHei"
    "Microsoft YaHei UI"
    "Arial Unicode MS")
  "Org 标题等用的无衬线中文（越靠前越优先）。未安装则自动跳过。")

(defun henri--org-first-available-font (candidates font-families)
  "在 FONT-FAMILIES 中返回 CANDIDATES 里第一个已安装的字族名。"
  (seq-find (lambda (name) (member name font-families)) candidates))

(defun henri/org--cjk-char-p (char)
  "Return non-nil when CHAR is likely to be a CJK character."
  (and (characterp char)
       (or (memq (get-char-code-property char 'script)
                 '(han cjk-misc kana bopomofo))
           (and (>= char #x3000) (<= char #x9fff))
           (and (>= char #x3400) (<= char #x4dbf))
           (and (>= char #x20000) (<= char #x2fa1f)))))

(defun henri/org--sample-cjk-position ()
  "Return a nearby CJK character position for font diagnostics."
  (or (and (henri/org--cjk-char-p (char-after)) (point))
      (save-excursion
        (catch 'pos
          (while (re-search-forward "[^[:ascii:]]" nil t)
            (when (henri/org--cjk-char-p (char-before))
              (throw 'pos (1- (point)))))))))

(defun henri/org-apply-cjk-fontset (family)
  "Prefer FAMILY for CJK characters in the selected frame fontset."
  (when (and (display-graphic-p)
             (stringp family)
             (fboundp 'set-fontset-font))
    (let ((font (font-spec :family family)))
      (dolist (target '(han cjk-misc kana bopomofo))
        (set-fontset-font nil target font nil 'prepend))
      (dolist (range '((#x3000 . #x303f)   ; CJK symbols and punctuation
                       (#x3400 . #x4dbf)   ; CJK Extension A
                       (#x4e00 . #x9fff)   ; CJK Unified Ideographs
                       (#xf900 . #xfaff)   ; CJK Compatibility Ideographs
                       (#x20000 . #x2fa1f))) ; CJK extensions
        (set-fontset-font nil range font nil 'prepend)))))

(defun henri/org-font-diagnose ()
  "Report the Org body font preference and the current buffer remap."
  (interactive)
  (let* ((families (and (display-graphic-p) (font-family-list)))
         (serif (or (and (boundp 'henri-org-cjk-serif-family)
                         (stringp henri-org-cjk-serif-family)
                         (member henri-org-cjk-serif-family families)
                         henri-org-cjk-serif-family)
                    (henri--org-first-available-font henri--org-cjk-serif-candidates families)))
         (default-remap (alist-get 'default face-remapping-alist))
         (variable-remap (alist-get 'variable-pitch face-remapping-alist))
         (sample-pos (henri/org--sample-cjk-position))
         (sample-char (and sample-pos (char-after sample-pos)))
         (face-at-point (and sample-pos (get-char-property sample-pos 'face)))
         (actual-font (and (display-graphic-p)
                           (fboundp 'font-at)
                           sample-pos
                           (ignore-errors (font-at sample-pos))))
         (actual-family (and actual-font (font-get actual-font :family))))
    (message "Org body font: selected=%S custom=%S sample=%S actual=%S face=%S default-remap=%S variable-remap=%S candidates-found=%S"
             serif
             (when (boundp 'henri-org-cjk-serif-family) henri-org-cjk-serif-family)
             sample-char
             actual-family
             face-at-point
             default-remap
             variable-remap
             (seq-filter (lambda (name) (member name families))
                         henri--org-cjk-serif-candidates))))

(defvar-local henri--org-face-remap-cookies nil
  "本缓冲区 `face-remap-add-relative' 的 cookie，用于重复进入 org-mode 时收回。")

(defun henri/org-clear-cjk-face-remaps ()
  "Remove face remaps installed by `henri/org-setup-body-font'."
  (dolist (cookie henri--org-face-remap-cookies)
    (when cookie
      (face-remap-remove-relative cookie)))
  (setq henri--org-face-remap-cookies nil))

(defun henri/org-setup-body-font (&optional buffer)
  "Apply serif to body and sans to Org headings in BUFFER or the current buffer."
  (let ((target (or buffer (current-buffer))))
    (when (buffer-live-p target)
      (with-current-buffer target
        (when (derived-mode-p 'org-mode)
          (henri/org-clear-cjk-face-remaps)
          (when (display-graphic-p)
            (let* ((families (font-family-list))
                   (serif
                    (or (and (boundp 'henri-org-cjk-serif-family)
                             (stringp henri-org-cjk-serif-family)
                             (member henri-org-cjk-serif-family families)
                             henri-org-cjk-serif-family)
                        (henri--org-first-available-font henri--org-cjk-serif-candidates families)))
                   (sans
                    (or (and (boundp 'henri-org-cjk-sans-family)
                             (stringp henri-org-cjk-sans-family)
                             (member henri-org-cjk-sans-family families)
                             henri-org-cjk-sans-family)
                        (henri--org-first-available-font henri--org-cjk-sans-candidates families))))
              (when serif
                (henri/org-apply-cjk-fontset serif)
                (push (face-remap-add-relative 'default :family serif :height 1.08)
                      henri--org-face-remap-cookies)
                (push (face-remap-add-relative 'variable-pitch :family serif :height 1.08)
                      henri--org-face-remap-cookies))
              (when sans
                (dolist (sym '(org-document-title
                                org-level-1 org-level-2 org-level-3 org-level-4
                                org-level-5 org-level-6 org-level-7 org-level-8))
                  (when (facep sym)
                    (push (face-remap-add-relative sym :family sans)
                          henri--org-face-remap-cookies)))))))))))

(with-eval-after-load 'org
  ;; 略延迟，避免早于本缓冲区其它 hook 或主题对 Org face 的改写。
  (add-hook 'org-mode-hook #'henri/org-setup-body-font)
  (add-hook 'org-mode-hook
            (lambda ()
              (let ((buffer (current-buffer)))
                (run-with-timer 0.05 nil #'henri/org-setup-body-font buffer)))))

(defun henri/apply-fonts ()
  "Apply global and Org-related font preferences."
  (henri/set-font)
  (when (fboundp 'henri/apply-org-faces)
    (henri/apply-org-faces)))

(provide 'visual-fonts)

;;; visual-fonts.el ends here
