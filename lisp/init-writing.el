;;; init-writing.el --- Emacs 写作环境配置 -*- lexical-binding: t -*-

;; Author: Your Name
;; Maintainer: Your Name
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

;; 3. LaTeX 支持
;;    - AucTeX            -- TeX/LaTeX 支持
;;    - CDLaTeX           -- 快速输入支持

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
  :bind (:map markdown-mode-map
         ("C-c C-g" . grip-mode)))

;; 增强预览支持
(use-package markdown-preview-eww
  :ensure t
  :after markdown-mode
  :config
  (setq markdown-preview-eww-open-on-start t)   ; 打开文件时自动预览
  (setq markdown-preview-eww-relative-images t)) ; 支持相对路径图片


;; =============================================================================
;; Org Mode 配置

(use-package org
  :ensure t
  :mode (("\\.org$" . org-mode))
  :init
  (setq org-startup-indented t)           ; 启用缩进
  (setq org-startup-with-inline-images t) ; 显示内联图片
  :config
  (setq org-log-done 'time))             ; 记录完成时间

;; 美化支持
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t))

;; =============================================================================
;; LaTeX 配置

(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

(use-package cdlatex
  :ensure t
  :hook ((latex-mode . turn-on-cdlatex)
         (org-mode . turn-on-org-cdlatex)))

(provide 'init-writing)

;;; init-writing.el ends here