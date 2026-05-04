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
;;    - init-dashboard -- 启动页与笔记快捷入口
;;    - init-managing  -- 基础管理功能
;;    - init-styling   -- 界面外观定制
;;    - init-programming -- 编程开发环境
;;    - init-writing   -- 写作环境配置

;;; Code:
;;; init.el --- Emacs 配置入口文件 -*- lexical-binding: t -*-

(require 'seq)

;; =============================================================================
;; 启动性能优化

;; （已移至 early-init）启动后恢复 file-name-handler-alist
(defvar default-file-name-handler-alist (or (bound-and-true-p henri--saved-file-name-handler-alist)
                                           file-name-handler-alist)
  "Original file-name-handler-alist saved for restoration after startup.")
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (boundp 'henri--saved-file-name-handler-alist)
              (setq file-name-handler-alist henri--saved-file-name-handler-alist))
            (message "[henri] file-name-handler-alist restored.")))

;; 启动时间统计
(defvar henri/startup-time-init (current-time))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs 启动耗时 %.2f 秒，共进行 %d 次 GC"
                     (float-time (time-subtract (current-time) henri/startup-time-init))
                     gcs-done)
            ;; 降低 GC 阈值
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.15)
            ;; 条件健康报告
            (when (and (boundp 'henri-health-report-on-startup)
                       henri-health-report-on-startup
                       (fboundp 'henri/report-health))
              (henri/report-health))))

;; =============================================================================
;; 包管理配置
(require 'package)

;; 配置包管理源
(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ;; TUNA's MELPA mirror can temporarily reference missing
                         ;; tarballs; keep MELPA official for first-time bootstrap.
                         ("melpa"  . "https://melpa.org/packages/")))

;; (setq package-archives
;;       '(("gnu" . "https://elpa.gnu.org/packages/")
;;         ("melpa" . "https://melpa.org/packages/")
;;         ("org" . "https://orgmode.org/elpa/")))

; ; 使用 USTC 镜像源（中科大）
; (setq package-archives '(("gnu"    . "http://mirrors.ustc.edu.cn/elpa/gnu/")
;                          ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")
;                          ("melpa"  . "http://mirrors.ustc.edu.cn/elpa/melpa/")))

;; 或使用 163 镜像源
;; (setq package-archives '(("gnu"    . "http://mirrors.163.com/elpa/gnu/")
;;                          ("nongnu" . "http://mirrors.163.com/elpa/nongnu/")
;;                          ("melpa"  . "http://mirrors.163.com/elpa/melpa/")))


;; 初始化包管理
(package-initialize)

;; 安装并加载 use-package（仅此路径刷新 ELPA；避免日常启动时空索引即联网）
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Local configuration modules
(dolist (dir '("lisp" "lisp/visual" "lisp/ops" "lisp/programming_languages"
               "lisp/writing" "lisp/writing/markdown"
               "lisp/writing/org" "lisp/writing/LaTeX"
               "lisp/writing/pdf"))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))

(require 'fix-warnings)
(require 'init-custom)
(require 'paths)
(require 'profiles)
(henri/load-profile)
(require 'lib-system)
(require 'lib-hooks)
(require 'lib-fonts)
(require 'lib-files)
(require 'doctor)
(require 'init-dashboard)

;; =============================================================================
;; 加载核心配置模块
(require 'init-managing)
(require 'init-styling)
(require 'init-programming)
(require 'init-writing)
(require 'status)
(require 'backup)


;; =============================================================================


;; =============================================================================
;; 性能优化配置

;; 垃圾回收 / 进程 IO 优化
(setq read-process-output-max (* 1024 1024))   ; 增加进程读取量为 1MB

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

;;; ---------------------------------------------------------------------------
;;; 健康报告（初始骨架，可扩展）

(require 'cl-lib)

(defun henri/report-health (&optional verbose)
  "打印当前配置健康状态。
VERBOSE 非空时输出更多细节。"
  (interactive "P")
  (let* ((uptime (float-time (time-subtract (current-time) henri/startup-time-init)))
         (pkg-count (length package-activated-list))
         (gc-thresh gc-cons-threshold)
         (lsp-bufs (cl-loop for b in (buffer-list)
                            when (and (boundp 'eglot--managed-mode)
                                      (buffer-local-boundp 'eglot--managed-mode b)
                                      (buffer-local-value 'eglot--managed-mode b))
                            collect (buffer-name b)))
         (msg (format "[health] uptime=%.2fs packages=%d gc-threshold=%s lsp-buffers=%d"
                      uptime pkg-count gc-thresh (length lsp-bufs))))
    (message "%s" msg)
    (when verbose
      (message "[health] lsp buffers: %s" lsp-bufs))
    msg))

(when (and (boundp 'henri-enable-rime) henri-enable-rime)
  (require 'init-rime))

(provide 'init)
