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
      (henri/report-health))) )

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
; (setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;                          ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
;                          ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

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
;; 早期警告修复
;; 在加载其他模块之前先加载警告修复
(load-file (expand-file-name "lisp/fix-warnings.el" user-emacs-directory))

;; 自定义项与分组（需最早加载）
(load-file (expand-file-name "lisp/init-custom.el" user-emacs-directory))

;; =============================================================================
;; 加载核心配置模块
;; 功能增强配置
(load-file (expand-file-name "lisp/init-managing.el" user-emacs-directory))
(load-file (expand-file-name "lisp/init-styling.el" user-emacs-directory))
(load-file (expand-file-name "lisp/init-programming.el" user-emacs-directory))
(load-file (expand-file-name "lisp/init-writing.el" user-emacs-directory))

;; 诊断/状态工具
(load-file (expand-file-name "lisp/ops/status.el" user-emacs-directory))
(load-file (expand-file-name "lisp/ops/backup.el" user-emacs-directory))


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

(provide 'init)

;;; ---------------------------------------------------------------------------
;;; 健康报告（初始骨架，可扩展）

(defun henri/report-health (&optional verbose)
  "打印当前配置健康状态。
VERBOSE 非空时输出更多细节。"
  (interactive "P")
  (let* ((uptime (float-time (time-subtract (current-time) henri/startup-time-init)))
         (pkg-count (length package-activated-list))
         (gc-thresh gc-cons-threshold)
         (lsp-bufs (cl-loop for b in (buffer-list)
                            when (buffer-local-value 'eglot--managed-mode b)
                            collect (buffer-name b)))
         (msg (format "[health] uptime=%.2fs packages=%d gc-threshold=%s lsp-buffers=%d"
                      uptime pkg-count gc-thresh (length lsp-bufs))))
    (message "%s" msg)
    (when verbose
      (message "[health] lsp buffers: %s" lsp-bufs))
    msg))

;;; eglot before-save hook 本地化修正（若 eglot 已加载）
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (and (boundp 'eglot-managed-mode) eglot-managed-mode)
                (add-hook 'before-save-hook #'eglot-format-buffer nil t)))))


;;; init.el ends here