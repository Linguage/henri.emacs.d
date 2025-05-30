;;; init-org.el --- Org Mode 模块化配置入口 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 2.0 (模块化版本)
;; Keywords: org

;;; Commentary:

;; Org Mode 模块化配置入口文件
;; 此文件负责加载所有 Org Mode 相关的子模块

;; 子模块说明：
;; - org-base.el     : 基础配置和美化
;; - org-pdf.el      : PDF 工具和查看
;; - org-journal.el  : 日志系统和 Agenda
;; - org-latex.el    : LaTeX 导出配置
;; - org-academic.el : 学术写作功能（可选）

;;; Code:

;; =============================================================================
;; 添加模块路径

(add-to-list 'load-path (expand-file-name "org" (file-name-directory load-file-name)))

;; =============================================================================
;; 加载所有 Org Mode 子模块

;; 1. 基础配置 - 必需
(require 'org-base)

;; 2. PDF 工具 - 推荐
(require 'org-pdf)

;; 3. 日志系统 - 推荐  
(require 'org-journal)

;; 4. LaTeX 导出 - 推荐（新的统一管理系统）
(require 'org-latex)

;; 5. LaTeX 主题扩展 - 可选
(condition-case err
    (require 'org-themes)
  (error 
   (message "org-themes 加载失败: %s" (error-message-string err))))

;; 6. 学术写作 - 可选（简化版本）
(require 'org-academic)



(provide 'init-org)

;;; init-org.el ends here