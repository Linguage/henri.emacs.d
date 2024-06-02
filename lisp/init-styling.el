;; 美化配置

;; 加载主题
(load-theme 'modus-operandi t)

;; 设置字体
; (set-frame-font "Cascadia Code NF-14" nil t)
(when (member "Cascadia Code NF" (font-family-list))
  (set-face-attribute 'default nil :family "Cascadia Code NF" :height 140))
(when (member "STKaiTi" (font-family-list))
  (dolist (charset '(han kana symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset "STKaiTi" nil 'prepend)))

;; 启用模型线
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 启用模式线
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; 设置透明度
(add-to-list 'default-frame-alist '(alpha . (95. 95)))

(provide 'init-styling)