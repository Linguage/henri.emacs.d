;;; init-styling.el --- Emacs 界面外观定制 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: faces, frames, themes

;;; Commentary:

;; 本配置文件提供 Emacs 的界面美化和外观定制，包含以下主要模块：

;; 1. 基础界面设置
;;    - 窗口布局      -- 大小和位置
;;    - 界面元素      -- 菜单栏、工具栏等
;;    - 显示增强      -- 行号、列号等

;; 2. 性能优化
;;    - GC 优化       -- 垃圾回收阈值
;;    - 启动优化      -- 禁用不必要功能

;; 3. 主题美化
;;    - doom-themes   -- 现代化主题支持
;;    - doom-modeline -- 美化状态栏
;;    - 彩虹括号      -- 优化代码可读性

;; 4. 字体配置
;;    - 等宽编程字体   -- JetBrains Mono
;;    - 中文字体适配   -- 自动配置

;;; Code:

;; =============================================================================
;; 基础界面设置

;; 窗口布局配置
(set-frame-size (selected-frame) 130 40)   ; 设置窗口大小
(add-to-list 'default-frame-alist '(top . 0.5))    ; 窗口垂直居中
(add-to-list 'default-frame-alist '(left . 0.5))   ; 窗口水平居中
(add-to-list 'default-frame-alist '(alpha . (95 . 95))) ; 设置透明度

;; 界面元素配置
(menu-bar-mode -1)                         ; 关闭菜单栏
(tool-bar-mode -1)                         ; 关闭工具栏
(scroll-bar-mode -1)                       ; 关闭滚动条
(blink-cursor-mode 0)                      ; 关闭光标闪烁

;; 显示增强
(global-display-line-numbers-mode)         ; 显示行号
(column-number-mode)                       ; 显示列号
(global-hl-line-mode t)                    ; 高亮当前行

;; =============================================================================
;; 性能优化配置

(setq gc-cons-threshold 100000000)         ; 提升GC阈值
(setq inhibit-startup-screen t)            ; 禁用启动屏幕
(setq auto-save-default nil)               ; 禁用自动保存
(setq backup-inhibited t)                  ; 禁用备份文件
(setq ring-bell-function 'ignore)          ; 禁用提示音

;; 编辑优化
(global-auto-revert-mode t)                ; 自动重载文件
(electric-pair-mode 1)                     ; 自动补全括号
(global-subword-mode 1)                    ; 驼峰语法支持

;; =============================================================================
;; 主题美化配置

;; =============================================================================
;; 主题配置

;; 定义一个变量来控制是否启用随机主题
(defvar enable-random-theme t
  "如果为 t，则启用随机主题；如为nil启用默认主题。")

;; 定义默认主题
(defvar default-theme 'doom-solarized-light
  "默认主题名称。")

;; 定义所有可用主题的列表
(defvar available-themes
  '(doom-Iosvkem
    doom-acario-dark
    doom-acario-light
    doom-challenger-deep
    doom-city-lights
    doom-dark+
    doom-dracula
    doom-ephemeral
    doom-fairy-floss
    doom-gruvbox
    doom-horizon
    doom-laserwave
    doom-manegarm
    doom-material
    doom-molokai
    doom-monokai-classic
    doom-monokai-pro
    doom-monokai-spectrum
    doom-moonlight
    doom-nord-light
    doom-nord
    doom-nova
    doom-oceanic-next
    doom-one-light
    doom-one
    doom-opera-light
    doom-opera
    doom-outrun-electric
    doom-palenight
    doom-peacock
    doom-rouge
    doom-snazzy
    doom-solarized-dark
    doom-solarized-light
    doom-sourcerer
    doom-spacegrey
    doom-tomorrow-day
    doom-tomorrow-night
    doom-vibrant
    doom-wilmersdorf)
  "可用的主题列表。")

;; 随机选择主题的函数
(defun load-random-theme ()
  "随机选择并加载一个主题。"
  (load-theme (nth (random (length available-themes)) available-themes) t))

;; 安装并配置 doom-themes
(use-package doom-themes
  :ensure t
  :config
  (if enable-random-theme
      (load-random-theme)
    (load-theme default-theme t)))


;; 彩虹括号
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 状态栏美化
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))


;; =============================================================================
;; 字体配置
;; windows系统需要安装NerdFontsSymbolsOnly字体，否则会出现图标显示异常


(use-package all-the-icons
  :ensure t)

(require 'all-the-icons)


(defun henri/get-os-type ()
  "获取当前操作系统类型。"
  (cond
   ((eq system-type 'darwin) 'macos)
   ((eq system-type 'gnu/linux)
    (if (string-match "Microsoft" (shell-command-to-string "uname -r"))
        'wsl
      'linux))
   ((eq system-type 'windows-nt) 'windows)
   (t 'unknown)))

(defun henri/set-font ()
  "根据操作系统设置字体。"
  (let ((os-type (henri/get-os-type)))
    (cond
     ;; macOS 字体设置
     ((eq os-type 'macos)
      (set-face-attribute 'default nil 
                         :family "JetBrains Mono"
                         :height 140)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font t charset
                         (font-spec :family "PingFang SC"))))
     
     ;; Windows 字体设置
     ((eq os-type 'windows)
      (set-face-attribute 'default nil
                         :family "Fira Code"
                         :height 120)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font t charset
                         (font-spec :family "Microsoft YaHei"))))
     
     ;; WSL 字体设置
     ((eq os-type 'wsl)
      (set-face-attribute 'default nil
                         :family "Cascadia Code PL"
                         :height 120)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font t charset
                         (font-spec :family "Microsoft YaHei"))))
     
     ;; Linux 字体设置
     ((eq os-type 'linux)
      (set-face-attribute 'default nil
                         :family "Cascadia Code PL"
                         :height 120)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font t charset
                         (font-spec :family "Noto Sans CJK SC")))))))

;; 应用字体设置
(when window-system
  (henri/set-font))

(provide 'init-styling)

;;; init-styling.el ends here