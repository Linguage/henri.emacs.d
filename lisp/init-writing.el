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
         ("C-c C-v" . markdown-preview)          ; 使用内置预览
         ("C-c C-c p" . markdown-preview-mode))) ; 备选预览模式

;; GitHub 风格预览支持
(use-package grip-mode
  :ensure t
  :after markdown-mode
  :if (and (boundp 'henri-enable-grip) henri-enable-grip)
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

(load-file (expand-file-name "lisp/writing/init-org.el" user-emacs-directory))

;; =============================================================================
;; LaTeX 支持 - 最小化配置

;; 添加 LaTeX 配置目录到 load-path
(add-to-list 'load-path (expand-file-name "lisp/writing/LaTeX/" user-emacs-directory))

;; 加载最小化 LaTeX 配置（仅服务于 Org 导出）
(require 'latex-minimal)

(provide 'init-writing)

;;; init-writing.el ends here