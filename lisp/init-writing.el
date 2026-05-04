;;; init-writing.el --- Emacs 写作环境配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: writing, markdown, org, latex

;;; Commentary:

;; 本配置文件提供写作环境支持，包含以下主要模块：

;; 1. Markdown 支持
;;    - markdown-mode      -- Markdown 语法支持
;;    - markdown-preview   -- 实时预览支持
;;    - grip-mode         -- GitHub 风格预览

;; 2. Org Mode 增强
;;    - org-bullets       -- 美化标题样式
;;    - org-superstar     -- 美化列表符号
;;    - org-fancy-priorities -- 优先级美化

;; 3. LaTeX 支持 (模块化配置)
;;    - 使用 LaTeX/ 目录下的模块化配置

;;; Code:

;; =============================================================================
;; Markdown 配置

(require 'lib-system)

(defun henri/markdown-check-preview-deps ()
  "报告 pandoc / grip 是否在 PATH 中（用于 Markdown 预览）。"
  (interactive)
  (let ((p (henri/executable-p "pandoc"))
        (g (henri/executable-p "grip")))
    (message "[Markdown 预览依赖] pandoc: %s | grip: %s | henri-enable-grip: %S"
             (if p "OK" "缺失（brew install pandoc）")
             (if g "OK" "缺失（pip install grip 等）")
             (if (boundp 'henri-enable-grip) henri-enable-grip 'unbound))))

(defun henri/markdown-preview-offline ()
  "使用 pandoc 的离线预览：优先 EWW，其次 `markdown-mode' 的 `markdown-preview'."
  (interactive)
  (unless (eq major-mode 'markdown-mode)
    (user-error "当前不是 markdown-mode"))
  (unless (henri/executable-p "pandoc")
    (user-error "未找到 pandoc：请先安装并保证在 PATH 中（如 brew install pandoc）"))
  (cond
   ((fboundp 'markdown-preview-eww)
    (call-interactively #'markdown-preview-eww))
   ((fboundp 'markdown-preview)
    (call-interactively #'markdown-preview))
   (t
    (user-error "未加载 markdown 预览命令（检查 markdown-preview-eww / markdown-mode）"))))

(defun henri/markdown-preview-github-style ()
  "使用 grip 的 GitHub 风格预览（需 `henri-enable-grip' 非 nil 且已安装 grip）。"
  (interactive)
  (unless (eq major-mode 'markdown-mode)
    (user-error "当前不是 markdown-mode"))
  (unless (bound-and-true-p henri-enable-grip)
    (user-error "grip 已在配置中关闭：将 `henri-enable-grip' 设为 t 并重启 Emacs"))
  (unless (henri/executable-p "grip")
    (user-error "未找到 grip：请先安装（如 pip install grip）并保证在 PATH 中"))
  (unless (fboundp 'grip-mode)
    (user-error "grip-mode 未加载：请检查 `use-package grip-mode' 是否启用"))
  (call-interactively #'grip-mode))

;; Markdown 基础配置
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")     ; 使用 pandoc 作为转换工具
  :config
  ;; 内置预览设置
  (setq markdown-preview-stylesheets
        '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"))
  ;; 预览配置
  (setq markdown-fontify-code-blocks-natively t)  ; 原生代码块高亮
  (setq markdown-display-remote-images t)         ; 显示远程图片
  :bind (:map markdown-mode-map
         ("C-c C-c p" . markdown-preview-mode)   ; 备选预览模式
         ("C-c m p" . henri/markdown-preview-offline)
         ("C-c m g" . henri/markdown-preview-github-style)
         ("C-c m c" . henri/markdown-check-preview-deps)))

;; GitHub 风格预览支持
(use-package grip-mode
  :ensure t
  :after markdown-mode
  :if henri-enable-grip
  :bind (:map markdown-mode-map
         ("C-c C-g" . grip-mode)))

;; 增强预览支持 - 禁用自动预览
(use-package markdown-preview-eww
  :ensure t
  :after markdown-mode
  :config
  (setq markdown-preview-eww-open-on-start nil)   ; 禁用打开文件时自动预览
  (setq markdown-preview-eww-relative-images t))  ; 支持相对路径图片

;; =============================================================================
;; Org Mode 配置

(require 'init-org)

;; =============================================================================
;; LaTeX 支持 - 最小化配置
;; load-path 已在 `init.el' 中统一加入 `lisp/writing/LaTeX/'。

;; 加载最小化 LaTeX 配置（仅服务于 Org 导出）
(require 'latex-minimal)

(provide 'init-writing)

;;; init-writing.el ends here