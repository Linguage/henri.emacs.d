;;; init-managing.el --- Emacs 基础管理功能配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience

;;; Commentary:

;; 本配置文件提供 Emacs 的基础管理功能，包含以下主要模块：

;; 1. 搜索与补全
;;    - ivy          -- 通用补全框架
;;    - counsel      -- 命令补全增强
;;    - swiper       -- 交互式搜索
;;    - which-key    -- 按键提示

;; 2. 文件管理
;;    - neotree      -- 文件树侧边栏

;; 3. 功能增强
;;    - which-key      -- 快捷键提示
;;    - exec-path-from-shell -- 环境变量同步
;;    - vterm        -- 主力终端
;;    - eshell       -- 备用 Lisp shell
;;
;; 4. Git（本地）
;;    - magit       -- 状态、blame、log 等
;;    - diff-hl    -- 改动高亮；Magit 刷新后同步
;;    - smerge-mode -- 合并冲突辅助
;;
;; 5. 布局管理
;;    - 窗口分割      -- 自定义窗口布局
;;    - 启动布局      -- 自动设置初始布局

;;; Code:

;; =============================================================================
;; 搜索与补全配置

;; Ivy - 轻量级补全框架
(use-package ivy
  :ensure t
  :defer 0.1
  :diminish
  :config
  (ivy-mode 1))

;; Counsel - 增强的命令补全
(use-package counsel
  :ensure t
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-c f n" . henri/find-file-in-notes)))

;; Swiper - 交互式搜索工具
(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)))

;; =============================================================================
;; 文件管理配置
;; all-the-icons: configured in init-styling.el only.

;; NeoTree - 文件树侧边栏
(use-package neotree
  :ensure t
  :defer t
  :bind (("<f8>" . neotree-toggle)
         ("C-c f p" . henri/neotree-project-dir))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)  ; 智能打开
  (setq neo-autorefresh t) ; 自动刷新
  (setq neo-window-fixed-size nil) ; 窗口大小可调整
  (add-hook 'neo-after-create-hook
            (lambda (&rest _)
              (display-line-numbers-mode -1) ; 关闭行号显示
              (setq truncate-lines t))) ; 禁止自动换行
  (defun henri/neotree-project-dir ()
    "打开项目根目录中的 NeoTree。"
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (if project-dir
          (progn
            (neotree-dir project-dir)
            (neotree-find file-name))
        (message "Could not find project root.")))))

;; 确保 projectile 已安装并配置
(use-package projectile
  :ensure t
  :defer t
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path
        (list (directory-file-name (expand-file-name henri-projects-directory))))
  (setq projectile-switch-project-action 'neotree-projectile-action))


;; =============================================================================
;; Shell / terminal 配置 (延迟加载)
(setq shell-file-name henri-shell
      explicit-shell-file-name henri-shell)

;; 环境变量同步优化 - 更激进的延迟加载
;; macOS 下 PATH 注入在 `emacs-startup-hook' 中调用 `henri/initialize-shell-env'（见下方 add-hook）。
(use-package exec-path-from-shell
  :ensure t
  :defer t  ; 完全延迟加载，仅在首次需要时加载
  :commands (exec-path-from-shell-initialize exec-path-from-shell-copy-env)
  :init
  ;; 更激进的性能优化设置
  (setq exec-path-from-shell-check-startup-files nil)    ; 禁用启动文件检查
  (setq exec-path-from-shell-debug nil)                  ; 禁用调试输出
  (setq exec-path-from-shell-shell-name
        (file-name-nondirectory henri-shell))             ; 与 `henri-shell' 一致
  (setq exec-path-from-shell-arguments '("-l"))          ; 减少参数
  (setq exec-path-from-shell-variables '("PATH" "SHELL"))) ; use-package 收尾，仅同步必要变量
(defun henri/initialize-shell-env ()
  "延迟初始化 shell 环境变量。"
  (interactive)
  (when (memq window-system '(mac ns))
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))

;; Conda 环境变量仅在需要时加载
(defun henri/setup-conda-env ()
  "设置 Conda 环境变量。"
  (interactive)
  (henri/initialize-shell-env)  ; 确保 shell 环境已初始化
  (exec-path-from-shell-copy-env "CONDA_PREFIX")
  (exec-path-from-shell-copy-env "CONDA_DEFAULT_ENV"))

;; 在启动阶段同步 shell PATH，确保 pandoc/grip 等外部工具在所有模块中可见。
(add-hook 'emacs-startup-hook #'henri/initialize-shell-env)

;; vterm 作为主力终端；eshell 只保留为备用 Lisp shell。
(use-package vterm
  :ensure t
  :commands (vterm henri/vterm)
  :init
  (setq vterm-always-compile-module t)
  (setq vterm-shell henri-shell)
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 10000)
  :config
  (setq vterm-shell henri-shell))

(defun henri/vterm ()
  "Open Henri's primary terminal with the configured login shell."
  (interactive)
  (henri/initialize-shell-env)
  (require 'vterm)
  (vterm))

;; =============================================================================
;; 窗口布局配置 (延迟加载)
(use-package eshell
  :ensure nil  ; 内置包不需要安装
  :defer t    ; 延迟加载
  :commands (eshell eshell-command)
  :init
  (setq eshell-prefer-lisp-functions t) ; 优先使用 Lisp 函数
  :config
  ;; eshell 的配置仅在首次使用时加载
  (setq eshell-history-size 1000)
  (setq eshell-save-history-on-exit t))

;; 延迟窗口布局设置 - 改为手动触发
(defun henri/setup-window-layout ()
  "手动设置窗口布局：分割窗口并启动 vterm。
现在通过快捷键 C-c w l 手动触发，不再自动执行。"
  (interactive)  ; 添加 interactive 使其可以通过快捷键调用
  (when (display-graphic-p)
    (split-window-right)
    (other-window 1)
    (henri/vterm)))

;; 全局 visual-line：首次按键前启用（与 `henri-first-input-hook' 一致）
(add-hook 'henri-first-input-hook (lambda () (global-visual-line-mode 1)))

;; =============================================================================
;; 快捷键提示
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; =============================================================================
;; 手动控制选项快捷键
(global-set-key (kbd "C-c w l") #'henri/setup-window-layout)  ; 手动触发窗口布局
(global-set-key (kbd "C-c w e") #'eshell)                     ; 备用 eshell
(global-set-key (kbd "C-c w v") #'henri/vterm)                ; 主力终端
(global-set-key (kbd "C-c w E") #'eshell)                     ; legacy explicit eshell

;; =============================================================================
;; Git：Magit + 边距/ fringe 变更提示（diff-hl）+ 合并冲突（smerge）
(when (boundp 'henri-enable-magit)
  (use-package magit
    :ensure t
    :if henri-enable-magit
    :commands (magit-status magit-dispatch magit-file-dispatch
                            magit-blame magit-log-buffer-file)
    :bind (("C-x g" . magit-status)
           ("C-c g g" . magit-status)
           ("C-c g d" . magit-dispatch)
           ("C-c g f" . magit-file-dispatch)
           ("C-c g b" . magit-blame)
           ("C-c g l" . magit-log-buffer-file))
    :config
    (setq magit-define-global-key-bindings nil))
  (with-eval-after-load 'magit
    (when (fboundp 'diff-hl-magit-post-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

(use-package diff-hl
  :ensure t
  :bind (:map diff-hl-mode-map
         ("C-c g p" . diff-hl-previous-hunk)
         ("C-c g n" . diff-hl-next-hunk)
         ("C-c g r" . diff-hl-revert-hunk))
  :init
  (add-hook
   'henri-first-file-hook
   (lambda ()
     (require 'diff-hl nil t)
     (global-diff-hl-mode 1)
     (unless (display-graphic-p)
       (when (fboundp 'diff-hl-margin-mode)
         (diff-hl-margin-mode 1)))
     (add-hook 'dired-mode-hook #'diff-hl-dired-mode))))

(defun henri/maybe-enable-smerge ()
  "若缓冲区含 Git 冲突标记，自动开启 `smerge-mode'."
  (when (and buffer-file-name
             (not (derived-mode-p 'dired-mode))
             (not (string-match-p "\\.org_archive\\'" buffer-file-name)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1)
        (message "已启用 smerge-mode（检测到合并冲突）")))))

(add-hook 'find-file-hook #'henri/maybe-enable-smerge)

(with-eval-after-load 'smerge-mode
  (define-key smerge-mode-map (kbd "C-c g m n") #'smerge-next)
  (define-key smerge-mode-map (kbd "C-c g m p") #'smerge-prev)
  (define-key smerge-mode-map (kbd "C-c g m u") #'smerge-keep-upper)
  (define-key smerge-mode-map (kbd "C-c g m l") #'smerge-keep-lower)
  (define-key smerge-mode-map (kbd "C-c g m b") #'smerge-keep-base)
  (define-key smerge-mode-map (kbd "C-c g m a") #'smerge-keep-all)
  (when (bound-and-true-p henri-keybindings-enable-legacy-aliases)
    (define-key smerge-mode-map (kbd "C-c ^ n") #'smerge-next)
    (define-key smerge-mode-map (kbd "C-c ^ p") #'smerge-prev)
    (define-key smerge-mode-map (kbd "C-c ^ u") #'smerge-keep-upper)
    (define-key smerge-mode-map (kbd "C-c ^ l") #'smerge-keep-lower)
    (define-key smerge-mode-map (kbd "C-c ^ b") #'smerge-keep-base)
    (define-key smerge-mode-map (kbd "C-c ^ a") #'smerge-keep-all)))

;; ================================
;; centaur-tabs 迁移到 styling，保持此处不重复定义（保留函数引用）。

(provide 'init-managing)
;;; init-managing.el ends here
