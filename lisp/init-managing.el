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
;;    - eshell        -- Shell 环境配置

;; 4. 布局管理
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
         ("C-x C-f" . counsel-find-file)))

;; Swiper - 交互式搜索工具
(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)))

;; =============================================================================
;; 文件管理配置

;; 安装并配置 all-the-icons
(use-package all-the-icons
  :ensure t
  :config
  ;; 安装字体（仅需运行一次）
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

;; NeoTree - 文件树侧边栏
(use-package neotree
  :ensure t
  :defer t
  :bind (("<f8>" . neotree-toggle)
         ("C-c n p" . henri/neotree-project-dir))
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
  (setq projectile-project-search-path '("~/projects/"))
  (setq projectile-switch-project-action 'neotree-projectile-action))


;; =============================================================================
;; Shell 环境配置
(setq shell-file-name "/bin/zsh"
      explicit-shell-file-name "/bin/zsh")

;; 环境变量同步优化
(use-package exec-path-from-shell
  :ensure t
  :defer 2  ; 延迟 2 秒加载
  :init
  (setq exec-path-from-shell-check-startup-files nil) ; 禁用启动文件检查
  (setq exec-path-from-shell-variables '("PATH" "SHELL")) ; 仅同步必要变量
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; Conda 环境变量仅在需要时加载
(defun henri/setup-conda-env ()
  "设置 Conda 环境变量。"
  (interactive)
  (exec-path-from-shell-copy-env "CONDA_PREFIX")
  (exec-path-from-shell-copy-env "CONDA_DEFAULT_ENV"))

;; =============================================================================
;; 窗口布局配置
(use-package eshell
  :ensure nil  ; 内置包不需要安装
  :defer t    ; 延迟加载
  :commands (eshell)
  :init
  (setq eshell-prefer-lisp-functions t)) ; 优先使用 Lisp 函数

;; 延迟窗口布局设置
(defun henri/setup-window-layout ()
  "延迟设置窗口布局。"
  (run-with-idle-timer
   2 nil
   (lambda ()
     (when (and (display-graphic-p)
                (not (member (buffer-name) '("*scratch*" "*Messages*"))))
       (split-window-right)
       (other-window 1)
       (eshell)))))

;; 仅在图形界面时设置窗口布局
(when (display-graphic-p)
  (add-hook 'emacs-startup-hook #'henri/setup-window-layout))

;; 全局设置延迟到空闲时加载
(run-with-idle-timer
 1 nil
 (lambda ()
   (global-visual-line-mode 1)))

;; =============================================================================
;; 快捷键提示
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(provide 'init-managing)

;;; init-managing.el ends here