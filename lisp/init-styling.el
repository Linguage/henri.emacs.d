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

(require 'seq)

;; =============================================================================
;; 基础界面设置

;; 窗口布局配置
;; 初始 frame 尺寸在 early-init.el 中设置，避免启动后再跳动。
(add-to-list 'default-frame-alist '(top . 0.5))    ; 窗口垂直居中
(add-to-list 'default-frame-alist '(left . 0.5))   ; 窗口水平居中
(add-to-list 'default-frame-alist '(alpha . (95 . 95))) ; 设置透明度

;; 界面元素配置（菜单栏等在 early-init.el 的 GUI 分支已处理）
(global-display-line-numbers-mode)         ; 显示行号
(column-number-mode)                       ; 显示列号
(global-hl-line-mode t)                    ; 高亮当前行

;; 性能：GC 由 early-init.el 提高启动阈值，init.el 启动钩子恢复。
;; 此处不再重复设置 `gc-cons-threshold'。

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




;; 主题策略由 init-custom.el 中的 defcustom 控制
(use-package doom-themes
  :ensure t
  :demand t
  :config
  (when (fboundp 'doom-themes-org-config)
    (doom-themes-org-config))
  (henri/apply-current-theme))

;; 彩虹括号（仅此一处；lisp 语言模块不再重复）
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 状态栏美化
;; 安装并配置 doom-modeline
(use-package nerd-icons
  :ensure t
  :demand t)

(use-package doom-modeline
  :ensure t
  :demand t
  :after nerd-icons
  :init
  (setq doom-modeline-height 28
        doom-modeline-bar-width 3
        doom-modeline-window-width-limit 85
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-buffer-encoding nil
        doom-modeline-indent-info nil
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-irc nil
        doom-modeline-lsp nil
        doom-modeline-env-version nil
        doom-modeline-check-simple-format t
        doom-modeline-vcs-max-length 12)
  (doom-modeline-mode 1)
  :config
  ;; 设置 doom-modeline 主题
  ; (setq doom-modeline-theme 'doom-modeline) ; 默认主题
  (setq doom-modeline-theme 'doom-modeline-light) ; 亮色主题
  ; (setq doom-modeline-theme 'doom-modeline-dark) ; 暗色主题
  )


;; =============================================================================
;; 字体配置
;; windows系统需要安装NerdFontsSymbolsOnly字体，否则会出现图标显示异常


(use-package all-the-icons
  :ensure t
  :config
  (unless (member "all-the-icons" (font-family-list))
    (when (display-graphic-p)
      (message "[henri] all-the-icons fonts not found; run M-x all-the-icons-install-fonts if icons look wrong."))))


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

(defun henri/font-family-available-p (family)
  "Return non-nil when font FAMILY is available."
  (member family (font-family-list)))

(defun henri/first-available-font (&rest families)
  "Return the first available font from FAMILIES."
  (seq-find #'henri/font-family-available-p families))

(defun henri/set-font ()
  "根据操作系统设置字体。"
  (let ((os-type (henri/get-os-type)))
    (cond
     ;; macOS 字体设置
     ((eq os-type 'macos)
      (let ((latin-font (henri/first-available-font
                         "Cascadia Code NF"
                         "CaskaydiaCove Nerd Font Mono"
                         "CaskaydiaCove Nerd Font"
                         "JetBrains Mono")))
        (when latin-font
          (set-face-attribute 'default nil :family latin-font :height 140)
          (set-face-attribute 'fixed-pitch nil :family latin-font :height 1.0)))
      (when (henri/font-family-available-p "SF Pro Text")
        (set-face-attribute 'variable-pitch nil :family "SF Pro Text" :height 1.0))
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
;; 标签页配置 (Centaur Tabs) - 延迟加载避免崩溃

(when (and (boundp 'henri-enable-centaur-tabs) henri-enable-centaur-tabs)
  (use-package centaur-tabs
    :ensure t
    :defer t
    :if (display-graphic-p)
    :bind (("C-<prior>" . centaur-tabs-backward)
           ("C-<next>" . centaur-tabs-forward))
    :init
    (when (display-graphic-p)
      (add-hook 'emacs-startup-hook #'henri/setup-centaur-tabs))
    :config
    (setq centaur-tabs-style "bar"
          centaur-tabs-height 32
          centaur-tabs-set-icons nil
          centaur-tabs-show-new-tab-button t
          centaur-tabs-set-close-button t
          centaur-tabs-close-button "×"
          centaur-tabs-new-tab-button "+"
          centaur-tabs-set-modified-marker t
          centaur-tabs-modified-marker "●"
          centaur-tabs-adjust-buffer-order t
          centaur-tabs-enable-buffer-alphabetical-reordering t
          centaur-tabs-enable-ido-completion nil)
    (defun centaur-tabs-buffer-groups ()
      "自定义缓冲区分组规则。"
      (list
       (cond
        ((derived-mode-p 'org-mode) "Org")
        ((or (derived-mode-p 'prog-mode)
             (derived-mode-p 'python-mode)
             (derived-mode-p 'emacs-lisp-mode)
             (derived-mode-p 'js-mode)
             (derived-mode-p 'c-mode)
             (derived-mode-p 'java-mode))
         "Programming")
        ((or (derived-mode-p 'text-mode)
             (derived-mode-p 'markdown-mode)
             (derived-mode-p 'latex-mode))
         "Text")
        ((derived-mode-p 'dired-mode) "Dired")
        ((string-equal "*" (substring (buffer-name) 0 1)) "System")
        (t "General"))))
    (defun centaur-tabs-hide-tab (x)
      "隐藏不需要显示标签页的缓冲区。"
      (let ((name (format "%s" x)))
        (or
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
         (string-prefix-p "*Warnings*" name)
         (string-prefix-p "*Messages*" name)
         (string-prefix-p "*scratch*" name)
         (string-prefix-p "*Completions*" name)
         (string-prefix-p "*Async-native-compile-log*" name)
         (string-prefix-p "*eshell*" name)
         (string-prefix-p "*shell*" name)
         (string-prefix-p "*terminal*" name)
         (and (string-prefix-p "magit" name)
              (not (file-name-extension name)))
         (string-prefix-p "*markdown-preview*" name)
         (string-prefix-p "*grip-*" name))))))

;; 延迟设置函数，确保安全启动
(defun henri/setup-centaur-tabs ()
  "安全地设置和启动 centaur-tabs"
  (interactive)
  (run-with-idle-timer 
   2 nil
   (lambda ()
     (when (and (boundp 'henri-enable-centaur-tabs)
                henri-enable-centaur-tabs
                (display-graphic-p)
                (not (bound-and-true-p centaur-tabs-mode)))
       ;; 先确保包已加载
       (require 'centaur-tabs nil t)
       ;; 启用 tabs 模式
       (centaur-tabs-mode 1)
       (message "Centaur-tabs 已启用")
       ;; 设置鼠标滚轮支持
       (henri/setup-tabs-mouse-support)
       ;; 尝试启用图标（如果字体可用）
       (henri/maybe-enable-tabs-icons)))))

;; 鼠标支持设置
(defun henri/setup-tabs-mouse-support ()
  "设置标签页的鼠标支持"
  (global-set-key [mouse-4] 'centaur-tabs-backward) ; 鼠标滚轮向上
  (global-set-key [mouse-5] 'centaur-tabs-forward)) ; 鼠标滚轮向下

;; 安全的图标启用函数
(defun henri/maybe-enable-tabs-icons ()
  "尝试安全地启用 centaur-tabs 图标"
  (when (and (featurep 'all-the-icons)
             (display-graphic-p))
    ;; 在图形环境下检查字体
    (condition-case err
        (when (and (> (length (font-family-list)) 0)
                   (or (member "all-the-icons" (font-family-list))
                       (member "All The Icons" (font-family-list))))
          (setq centaur-tabs-set-icons t)
          ;; 重新应用设置
          (when (bound-and-true-p centaur-tabs-mode)
            (centaur-tabs-mode -1)
            (centaur-tabs-mode 1))
          (message "Centaur-tabs 图标已启用"))
      (error 
       (message "图标启用失败: %s" err)))))

;; 可选：标签页主题美化 (与当前主题集成)
(defun henri/centaur-tabs-theme ()
  "为 centaur-tabs 设置主题颜色"
  (when (and (display-graphic-p)
             (bound-and-true-p centaur-tabs-mode))
    ;; 根据当前主题调整标签页颜色
    (condition-case nil
        (centaur-tabs-headline-match)
      (error nil))))

;; 在主题加载后应用标签页主题
(add-hook 'doom-themes-after-load-theme-hook #'henri/centaur-tabs-theme)

;; 移除之前可能有问题的延迟加载代码
;; (run-with-idle-timer 3 nil #'henri/enable-centaur-tabs-icons)

(provide 'init-styling)

;;; init-styling.el ends here
