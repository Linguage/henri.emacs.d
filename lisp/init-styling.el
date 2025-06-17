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
(defvar enable-random-theme nil
  "如果为 t，则启用随机主题；如为nil启用默认主题。")


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
  
;; 定义白天和夜晚使用的主题
(defvar day-theme 'doom-acario-light
  "白天（9:00-18:00）使用的主题。")
(defvar night-theme 'doom-one ; 您可以更改为您喜欢的深色主题
  "夜晚（18:00-次日9:00）使用的主题。")

;; 根据时间设置主题的函数
(defun henri/set-theme-based-on-time ()
  "根据当前时间设置白天或夜晚主题。"
  (let ((current-hour (nth 2 (decode-time (current-time)))))
    (if (and (>= current-hour 9) (< current-hour 18)) ; 早上9点到下午6点之前
        (progn
          (message "Applying day theme: %s" day-theme)
          (load-theme day-theme t))
      (progn
        (message "Applying night theme: %s" night-theme)
        (load-theme night-theme t)))))

;; 安装并配置 doom-themes
(use-package doom-themes
  :ensure t
  :config
  (if enable-random-theme
      (load-random-theme)
    (henri/set-theme-based-on-time))) ; 调用新函数根据时间设置主题


;; 彩虹括号
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 状态栏美化
;; 安装并配置 doom-modeline
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  ;; 设置 doom-modeline 主题
  ; (setq doom-modeline-theme 'doom-modeline) ; 默认主题
  (setq doom-modeline-theme 'doom-modeline-light) ; 亮色主题
  ; (setq doom-modeline-theme 'doom-modeline-dark) ; 暗色主题
    ;; 其他配置选项
  (setq doom-modeline-height 30) ; 设置模型线高度
  (setq doom-modeline-bar-width 4) ; 设置模型线宽度
  (setq doom-modeline-lsp t) ; 启用 LSP 状态显示
  (setq doom-modeline-github t) ; 启用 GitHub 状态显示
  (setq doom-modeline-mu4e t) ; 启用 mu4e 状态显示
  (setq doom-modeline-irc t) ; 启用 IRC 状态显示
  (setq doom-modeline-minor-modes t) ; 显示次要模式
  (setq doom-modeline-enable-word-count t) ; 启用字数统计
  )


;; =============================================================================
;; 字体配置
;; windows系统需要安装NerdFontsSymbolsOnly字体，否则会出现图标显示异常


(use-package all-the-icons
  :ensure t
  :config
  ;; 检查字体是否已安装，如果没有则安装
  (unless (member "all-the-icons" (font-family-list))
    (when (display-graphic-p)
      (all-the-icons-install-fonts t))))


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

;; =============================================================================
;; 标签页配置 (Centaur Tabs)

(use-package centaur-tabs
  :ensure t
  :demand t
  :config
  ;; 启用 centaur-tabs
  (centaur-tabs-mode t)
    ;; 基础设置
  (setq centaur-tabs-style "bar")              ; 标签页样式: "alternate", "bar", "box", "chamfer", "rounded", "slant", "wave", "zigzag"
  (setq centaur-tabs-height 32)                ; 标签页高度
  (setq centaur-tabs-set-icons nil)            ; 暂时禁用图标避免字体问题
  (setq centaur-tabs-show-new-tab-button t)    ; 显示新建标签按钮
  (setq centaur-tabs-set-close-button t)       ; 显示关闭按钮
  (setq centaur-tabs-close-button "×")         ; 关闭按钮样式
  (setq centaur-tabs-new-tab-button "+")       ; 新建按钮样式
  (setq centaur-tabs-set-modified-marker t)    ; 显示修改标记
  (setq centaur-tabs-modified-marker "●")      ; 修改标记样式
  
  ;; 标签页分组
  (setq centaur-tabs-adjust-buffer-order t)    ; 自动调整标签页顺序
  (setq centaur-tabs-enable-buffer-alphabetical-reordering t) ; 按字母顺序排序
  
  ;; 鼠标操作
  (setq centaur-tabs-enable-ido-completion nil) ; 禁用 ido 补全
  
  ;; 定制标签页分组规则
  (defun centaur-tabs-buffer-groups ()
    "自定义缓冲区分组规则"
    (list
     (cond
      ;; Org 模式文件
      ((derived-mode-p 'org-mode)
       "Org")
      ;; 编程语言文件
      ((or (derived-mode-p 'prog-mode)
           (derived-mode-p 'python-mode)
           (derived-mode-p 'emacs-lisp-mode)
           (derived-mode-p 'js-mode)
           (derived-mode-p 'c-mode)
           (derived-mode-p 'java-mode))
       "Programming")
      ;; 文本文件
      ((or (derived-mode-p 'text-mode)
           (derived-mode-p 'markdown-mode)
           (derived-mode-p 'latex-mode))
       "Text")
      ;; Dired 目录浏览
      ((derived-mode-p 'dired-mode)
       "Dired")
      ;; 系统缓冲区
      ((string-equal "*" (substring (buffer-name) 0 1))
       "System")
      ;; 默认分组
      (t
       "General"))))
  
  ;; 隐藏特定缓冲区的标签页
  (defun centaur-tabs-hide-tab (x)
    "隐藏不需要显示标签页的缓冲区"
    (let ((name (format "%s" x)))
      (or
       ;; 当前缓冲区不是文件缓冲区
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*mybuf" name)
       ;; 不是普通文件或者目录
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name))))))
  
  ;; 键盘快捷键
  :bind
  ("C-<prior>" . centaur-tabs-backward)        ; Ctrl+PageUp: 前一个标签
  ("C-<next>" . centaur-tabs-forward)          ; Ctrl+PageDown: 后一个标签
  ("C-c t s" . centaur-tabs-counsel-switch-group) ; 切换标签组
  ("C-c t p" . centaur-tabs-group-by-projectile-project) ; 按项目分组
  ("C-c t g" . centaur-tabs-group-buffer-groups) ; 重新分组
  )

;; 可选：添加额外的鼠标滚轮支持
(global-set-key [mouse-4] 'centaur-tabs-backward) ; 鼠标滚轮向上
(global-set-key [mouse-5] 'centaur-tabs-forward)  ; 鼠标滚轮向下

;; 可选：标签页主题美化 (与当前主题集成)
(defun henri/centaur-tabs-theme ()
  "为 centaur-tabs 设置主题颜色"
  (when (display-graphic-p)
    ;; 根据当前主题调整标签页颜色
    (centaur-tabs-headline-match)))

;; 在主题加载后应用标签页主题
(add-hook 'doom-themes-after-load-theme-hook #'henri/centaur-tabs-theme)

;; 安全启用图标的函数
(defun henri/enable-centaur-tabs-icons ()
  "安全地启用 centaur-tabs 图标。"
  (when (and (featurep 'all-the-icons)
             (member "all-the-icons" (font-family-list))
             (display-graphic-p))
    (setq centaur-tabs-set-icons t)
    (centaur-tabs-mode t)  ; 重新启用以应用图标设置
    (message "Centaur-tabs 图标已启用")))

;; 延迟启用图标，确保字体已加载
(run-with-idle-timer 3 nil #'henri/enable-centaur-tabs-icons)

(provide 'init-styling)

;;; init-styling.el ends here