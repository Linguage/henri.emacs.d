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

;;; Code:
;;; init.el --- Emacs 配置入口文件 -*- lexical-binding: t -*-

;; =============================================================================
;; 启动性能优化

;; 禁用文件名处理程序
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; 启动时间统计
(defvar henri/startup-time-init (current-time))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs 启动耗时 %.2f 秒，共进行 %d 次 GC"
                     (float-time (time-subtract (current-time) henri/startup-time-init))
                     gcs-done)))

;; =============================================================================
;; 快速启动模式

; (defvar henri/file-name-handler-alist file-name-handler-alist)
; (defvar henri/normal-gc-cons-threshold (* 16 1024 1024))
; (defvar henri/init-gc-cons-threshold (* 128 1024 1024))

; ;; 临时提高 GC 阈值
; (setq gc-cons-threshold henri/init-gc-cons-threshold
;       gc-cons-percentage 0.6
;       file-name-handler-alist nil)

; ;; 首次空闲时恢复正常设置
; (add-hook 'emacs-startup-hook
;           (lambda ()
;             (run-with-idle-timer
;              1 nil
;              (lambda ()
;                (setq gc-cons-threshold henri/normal-gc-cons-threshold
;                      gc-cons-percentage 0.1
;                      file-name-handler-alist henri/file-name-handler-alist)
;                (garbage-collect)))))


;; =============================================================================
;; 包管理配置
(require 'package)

;; 配置包管理源
(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; 初始化包管理
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; 安装并加载 use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; 设置 Emacs 启动时的默认目录
(setq default-directory "~/Documents/EmacsNotes/")

; ;; 启动时打开特定文件
; (find-file "~/Documents/Code-Test/Emacs/test.org")

; ;; 启动时打开特定目录
; (dired "~/Documents/Code-Test/Emacs/")

;; =============================================================================
;; 加载核心配置模块
;; 功能增强配置
(load-file (expand-file-name "lisp/init-managing.el" user-emacs-directory))
(load-file (expand-file-name "lisp/init-styling.el" user-emacs-directory))
(load-file (expand-file-name "lisp/init-programming.el" user-emacs-directory))
(load-file (expand-file-name "lisp/init-writing.el" user-emacs-directory))


;; =============================================================================


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