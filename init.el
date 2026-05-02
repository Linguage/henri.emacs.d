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

(unless package-archive-contents
  (package-refresh-contents))

;; 安装并加载 use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; 设置 Emacs 启动时的默认目录
(defvar henri/default-notes-directory
  (expand-file-name "~/Documents/EmacsNotes/")
  "Default directory opened when Emacs starts.")

(defvar henri/config-directory
  (file-name-directory (or load-file-name user-init-file))
  "Directory of Henri's Emacs configuration.")

(defvar henri/startup-directory-suppression-active t
  "Non-nil while startup directory Dired buffers should be suppressed.")

(defun henri/config-directory-p (dir)
  "Return non-nil when DIR is this Emacs configuration directory."
  (when (stringp dir)
    (let* ((expanded (file-truename (expand-file-name dir)))
           (config-dir (file-truename user-emacs-directory))
           (repo-dir (file-truename henri/config-directory)))
      (or (string= expanded (directory-file-name config-dir))
          (string= expanded (directory-file-name repo-dir))))))

(defun henri/startup-directory-suppression-active-p ()
  "Return non-nil while startup directory suppression is active."
  henri/startup-directory-suppression-active)

(defun henri/drop-startup-directory-args ()
  "Ignore directory command-line args during app startup.

This prevents Emacs from opening a redundant Dired frame for the
configuration directory before the dashboard is shown.  File arguments are
left intact, so explicit file opens still work."
  (when (boundp 'command-line-args-left)
    (setq command-line-args-left
          (seq-remove
           (lambda (arg)
             (and (stringp arg)
                  (not (string-prefix-p "-" arg))
                  (file-directory-p (expand-file-name arg))))
           command-line-args-left))))

(henri/drop-startup-directory-args)

(run-at-time
 60 nil
 (lambda ()
   (setq henri/startup-directory-suppression-active nil)))

(defun henri/open-notes-directory ()
  "Open Henri's notes directory in Dired."
  (interactive)
  (dired henri/default-notes-directory))

(defun henri/open-journal-directory ()
  "Open Henri's Journal directory in Dired."
  (interactive)
  (dired (expand-file-name "Journal/" henri/default-notes-directory)))

(defun henri/find-file-in-notes ()
  "Find file starting from `henri/default-notes-directory'."
  (interactive)
  (let ((default-directory henri/default-notes-directory))
    (if (fboundp 'counsel-find-file)
        (counsel-find-file)
      (call-interactively #'find-file))))

(global-set-key [remap find-file] #'henri/find-file-in-notes)

(defun henri/dashboard-insert-action (label action)
  "Insert dashboard LABEL as a button invoking ACTION."
  (insert-text-button
   label
   'action (lambda (_button) (call-interactively action))
   'follow-link t
   'help-echo (format "Run %s" action))
  (insert "\n"))

(defconst henri/dashboard-logo
  '("        _________"
    "     .-'  _____  '-."
    "    /   .'     '.   \\"
    "   /   /  .---.  \\   \\"
    "  |   |  /  _  \\  |   |"
    "  |   | |  / \\  | |   |"
    "  |   |  \\ '-' /  |   |"
    "   \\   \\  '---'  /   /"
    "    '.  '.___.'  .'"
    "      '-._____.-'")
  "ASCII logo displayed on Henri's dashboard.")

(defun henri/dashboard-insert-centered (text &optional face)
  "Insert TEXT centered in the current window, optionally using FACE."
  (let* ((width (max 80 (window-width)))
         (padding (max 0 (/ (- width (string-width text)) 2))))
    (insert (make-string padding ?\s))
    (insert (if face (propertize text 'face face) text))
    (insert "\n")))

(defun henri/dashboard ()
  "Create and return Henri's startup dashboard buffer."
  (let ((buffer (get-buffer-create "*Henri Dashboard*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n")
        (dolist (line henri/dashboard-logo)
          (henri/dashboard-insert-centered line 'font-lock-keyword-face))
        (insert "\n")
        (henri/dashboard-insert-centered "henri.emacs.d" 'font-lock-function-name-face)
        (henri/dashboard-insert-centered "Personal writing and coding workspace")
        (insert "\n  Notes root: " henri/default-notes-directory "\n\n")
        (henri/dashboard-insert-action "  [RET] Open Notes" #'henri/open-notes-directory)
        (henri/dashboard-insert-action "        Open Journal" #'henri/open-journal-directory)
        (henri/dashboard-insert-action "        Find File in Notes" #'henri/find-file-in-notes)
        (insert "\n  Shortcuts\n")
        (insert "  C-x C-f  Find file from Notes\n")
        (insert "  C-c c    Org capture\n")
        (insert "  C-c a    Org agenda\n")
        (insert "  C-c h 0  Apply default HTML theme\n")
        (goto-char (point-min))
        (special-mode)
        (setq-local display-line-numbers nil)
        (setq-local default-directory henri/default-notes-directory)))
    buffer))

(defun henri/redirect-startup-config-dired-noselect (orig-fun dir-or-list &rest args)
  "Return dashboard instead of startup Dired for the config directory."
  (if (and (henri/startup-directory-suppression-active-p)
           (stringp dir-or-list)
           (henri/config-directory-p dir-or-list))
      (henri/dashboard)
    (apply orig-fun dir-or-list args)))

(defun henri/redirect-startup-config-dired (orig-fun dirname &rest args)
  "Show dashboard instead of startup Dired for the config directory."
  (if (and (henri/startup-directory-suppression-active-p)
           (stringp dirname)
           (henri/config-directory-p dirname))
      (switch-to-buffer (henri/dashboard))
    (apply orig-fun dirname args)))

(advice-add 'dired-noselect :around #'henri/redirect-startup-config-dired-noselect)
(advice-add 'dired :around #'henri/redirect-startup-config-dired)

(defun henri/replace-startup-config-dired-buffer ()
  "Replace an accidental startup Dired buffer for the config directory."
  (when (and (henri/startup-directory-suppression-active-p)
             (derived-mode-p 'dired-mode)
             (boundp 'dired-directory)
             (stringp dired-directory)
             (henri/config-directory-p dired-directory))
    (let ((dired-buffer (current-buffer)))
      (switch-to-buffer (henri/dashboard))
      (when (buffer-live-p dired-buffer)
        (kill-buffer dired-buffer)))))

(add-hook 'dired-mode-hook #'henri/replace-startup-config-dired-buffer)

(defun henri/show-dashboard-only ()
  "Show the dashboard and remove redundant startup config Dired frames."
  (interactive)
  (let ((buffer (henri/dashboard)))
    (pop-to-buffer-same-window buffer)
    (delete-other-windows)
    (when (display-graphic-p)
      (let ((dashboard-frame (selected-frame)))
        (dolist (frame (frame-list))
          (unless (eq frame dashboard-frame)
            (delete-frame frame t)))))
    buffer))

(defun henri/startup-config-dired-buffer-p (buffer)
  "Return non-nil when BUFFER is a startup Dired buffer for the config dir."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (and (derived-mode-p 'dired-mode)
           (boundp 'dired-directory)
           (stringp dired-directory)
           (henri/config-directory-p dired-directory)))))

(defun henri/cleanup-startup-config-dired ()
  "Remove late startup Dired windows for the config directory."
  (when (henri/startup-directory-suppression-active-p)
    (let ((dashboard (henri/dashboard))
          (dired-buffers (seq-filter #'henri/startup-config-dired-buffer-p
                                     (buffer-list))))
      (when dired-buffers
        (switch-to-buffer dashboard)
        (delete-other-windows)
        (dolist (buffer dired-buffers)
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(when (file-directory-p henri/default-notes-directory)
  (setq default-directory henri/default-notes-directory
        initial-buffer-choice #'henri/dashboard))

(add-hook 'window-setup-hook #'henri/show-dashboard-only)
(add-hook 'window-setup-hook #'henri/cleanup-startup-config-dired)
(run-at-time 0.2 nil #'henri/cleanup-startup-config-dired)
(run-at-time 1.0 nil #'henri/cleanup-startup-config-dired)
(run-at-time 3.0 nil #'henri/cleanup-startup-config-dired)

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

;;; eglot before-save hook 本地化修正（若 eglot 已加载）
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (and (boundp 'eglot-managed-mode) eglot-managed-mode)
                (add-hook 'before-save-hook #'eglot-format-buffer nil t)))))


(when (and (boundp 'henri-enable-rime) henri-enable-rime)
  (load-file (expand-file-name "lisp/init-rime.el" user-emacs-directory)))
