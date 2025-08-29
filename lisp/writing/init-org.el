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
;; - org-latex.el    : LaTeX/PDF 导出和工具（合并版本）
;; - org-journal.el  : 日志系统和 Agenda
;; - org-html.el     : HTML 导出和主题
;; - org-academic.el : 学术写作模板系统

;;; Code:

;; =============================================================================
;; 添加模块路径

(add-to-list 'load-path (expand-file-name "org" (file-name-directory load-file-name)))

;; =============================================================================
;; 加载所有 Org Mode 子模块

;; 1. 基础配置 - 必需
;; 条件加载基础模块
(when (and (boundp 'henri-org-enable-base) henri-org-enable-base)
	(require 'org-base))

;; 2. LaTeX/PDF 导出 - 必需（合并了 PDF 工具）
;; LaTeX / PDF
(when (and (boundp 'henri-org-enable-latex) henri-org-enable-latex)
	(require 'org-latex))

;; 3. 日志系统 - 推荐  
;; Journal
(when (and (boundp 'henri-org-enable-journal) henri-org-enable-journal)
	(require 'org-journal))

;; 4. HTML 导出 - 推荐
;; HTML
(when (and (boundp 'henri-org-enable-html) henri-org-enable-html)
	(require 'org-html))

;; 5. 学术写作 - 新增
;; Academic
(when (and (boundp 'henri-org-enable-academic) henri-org-enable-academic)
	(require 'org-academic))

;; =============================================================================
;; 初始化学术写作环境

(when (and (boundp 'henri-org-enable-academic) henri-org-enable-academic
		   (fboundp 'org-academic-init))
  (add-hook 'after-init-hook 'org-academic-init))

(provide 'init-org)

;;; init-org.el ends here