
;; ============
;; 初始配置 ;;
;; ============
;; 设置Emacs的外观
(load-theme 'modus-operandi t) ; 使用Modus Operandi主题，这是一个现代化且舒适的主题
(menu-bar-mode -1) ; 关闭菜单栏
(tool-bar-mode -1) ; 关闭工具栏
(scroll-bar-mode -1) ; 关闭滚动条
(global-display-line-numbers-mode) ; 显示行号
(column-number-mode) ; 显示列号
(global-hl-line-mode t) ; 高亮当前行
(blink-cursor-mode 0) ; 关闭光标闪烁

;; 提高Emacs的性能
(setq gc-cons-threshold 100000000) ; 增加GC阈值，提升性能
(setq inhibit-startup-screen t) ; 禁用启动屏幕
(setq auto-save-default nil) ; 禁止自动保存文件
(setq backup-inhibited t) ; 禁止备份文件
(setq ring-bell-function 'ignore) ; 关闭提示音

;; 设置快捷键
(global-set-key (kbd "C-x C-b") 'ibuffer) ; 使用ibuffer代替默认的buffer列表
(global-set-key (kbd "M-x") 'counsel-M-x) ; 使用counsel提供更好的M-x功能

;; 其他个性化设置
; (setq-default indent-tabs-mode nil) ; 使用空格而不是制表符缩进

;; 启用更快的模式
(global-auto-revert-mode t) ; 自动重新加载文件
(electric-pair-mode 1) ; 自动补全括号
(global-subword-mode 1) ; 在单词内部移动光标

;; ============
;; 美化配置   ;;
;; ============


;; 加载主题
(load-theme 'modus-operandi t)

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


;; 设置字体
; (set-frame-font "Cascadia Code NF-14" nil t)
(when (member "Cascadia Code NF" (font-family-list))
  (set-face-attribute 'default nil :family "Cascadia Code NF" :height 140))
(when (member "STKaiTi" (font-family-list))
  (dolist (charset '(han kana symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset "STKaiTi" nil 'prepend)))

(provide 'init-styling)