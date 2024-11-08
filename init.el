;;; init.el --- Emacs 配置入口文件 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience

;;; Commentary:

;; 本配置文件是 Emacs 的主要入口配置，包含以下主要模块：

;; 1. 包管理
;;    - package.el      -- 内置包管理器配置
;;    - use-package    -- 声明式包管理工具
;;    - ELPA 镜像源    -- 清华镜像源配置

;; 2. 核心配置模块 (通过外部文件加载)
;;    - init-managing  -- 基础管理功能
;;    - init-styling   -- 界面外观定制
;;    - init-programming -- 编程开发环境
;;    - init-writing   -- 写作环境配置

;; 3. 功能增强
;;    - which-key      -- 快捷键提示
;;    - exec-path-from-shell -- 环境变量同步
;;    - eshell        -- Shell 环境配置

;; 4. 布局管理
;;    - 窗口分割      -- 自定义窗口布局
;;    - 启动布局      -- 自动设置初始布局

;;; Code:

;; =============================================================================
;; 包管理配置
(require 'package)

;; 配置包管理源
(setq package-archives 
      '(("tunu_gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("tunu_nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("tunu_melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; 初始化包管理
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; 安装并加载 use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; =============================================================================
;; 加载核心配置模块
(load-file (expand-file-name "lisp/init-managing.el" user-emacs-directory))
(load-file (expand-file-name "lisp/init-styling.el" user-emacs-directory))
(load-file (expand-file-name "lisp/init-programming.el" user-emacs-directory))
(load-file (expand-file-name "lisp/init-writing.el" user-emacs-directory))

;; =============================================================================
;; 功能增强配置

;; 快捷键提示
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Shell 环境配置
(setq shell-file-name "/bin/zsh")
(setq explicit-shell-file-name shell-file-name)
(add-to-list 'exec-path "/bin")
(setenv "SHELL" shell-file-name)

;; 环境变量同步
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-copy-env "ZSH")
  (exec-path-from-shell-copy-env "CONDA_PREFIX")
  (exec-path-from-shell-copy-env "CONDA_DEFAULT_ENV")
  (exec-path-from-shell-copy-env "SHELL"))

;; =============================================================================
;; 窗口布局配置

;; 自定义 eshell 窗口布局函数
(defun open-eshell-and-split-windows ()
  "打开 eshell 并优化窗口布局。
在右侧分割出一个新窗口并打开 eshell。"
  (interactive)
  (split-window-right)
  (other-window 1)
  (eshell))

;; 启用全局自动换行
(global-visual-line-mode 1)

;; 设置初始窗口布局
(defun my-setup-windows ()
  "设置自定义的窗口布局。
清除其他窗口并设置 eshell 布局。"
  (interactive)
  (delete-other-windows)
  (open-eshell-and-split-windows))

;; 在 Emacs 启动时应用窗口布局
(add-hook 'emacs-startup-hook 'my-setup-windows)

;; =============================================================================
;; 性能优化配置

;; 垃圾回收优化
(setq gc-cons-threshold (* 100 1024 1024))    ; 设置为 100MB
(setq gc-cons-percentage 0.6)                  ; 触发阈值设为 0.6
(setq read-process-output-max (* 1024 1024))   ; 增加进程读取量为 1MB

;; 启动后恢复正常的 GC
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))  ; 16MB
            (setq gc-cons-percentage 0.1)))

;; 大文件优化
(setq large-file-warning-threshold (* 100 1024 1024)) ; 设置大文件警告阈值为 100MB
(setq vc-follow-symlinks t)                           ; 自动跟踪符号链接
(setq auto-window-vscroll nil)                        ; 禁用自动垂直滚动
(setq inhibit-compacting-font-caches t)              ; 禁用字体缓存压缩

;; 显示优化
(setq redisplay-skip-fontification-on-input t)       ; 输入时跳过字体渲染
(setq fast-but-imprecise-scrolling t)                ; 快速滚动
(setq jit-lock-defer-time 0)                         ; 延迟语法高亮
(setq frame-inhibit-implied-resize t)                ; 禁止框架自动调整

;; 进程优化
(setq process-adaptive-read-buffering nil)           ; 禁用自适应缓冲
(setq remote-file-name-inhibit-locks t)             ; 禁用远程文件锁定

;; 模块延迟加载
(setq package-enable-at-startup nil)                 ; 禁止启动时加载包
(setq site-run-file nil)                            ; 禁用 site-start.el

;; =============================================================================
;; 自定义设置文件
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here