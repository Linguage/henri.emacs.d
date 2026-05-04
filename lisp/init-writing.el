;;; init-writing.el --- Emacs 写作环境配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: writing, markdown, org, latex

;;; Commentary:

;; 本配置文件是写作环境总入口，按子模块组织：
;;
;;   - markdown/  -- Markdown 写作（markdown-base.el）
;;   - org/       -- Org Mode 写作（init-org.el 及其下属模块）
;;   - LaTeX/     -- LaTeX 支持（latex-minimal.el 及主题/模板）
;;
;; load-path 已在 `init.el' 中统一注册各子目录，本文件只负责装配各子模块。

;;; Code:

;; =============================================================================
;; Markdown 子模块

(require 'markdown-base)
(when (and (boundp 'henri-md-enable-export) henri-md-enable-export)
  (require 'markdown-export))
(when (and (boundp 'henri-md-enable-toc) henri-md-enable-toc)
  (require 'markdown-nav))
(when (and (boundp 'henri-md-enable-notes) henri-md-enable-notes)
  (require 'markdown-notes))
(when (and (boundp 'henri-md-enable-lint) henri-md-enable-lint)
  (require 'markdown-lint))
(when (and (boundp 'henri-md-enable-template) henri-md-enable-template)
  (require 'markdown-template))

;; =============================================================================
;; Org Mode 子模块

(require 'init-org)

;; =============================================================================
;; LaTeX 子模块（最小化配置，仅服务于 Org 导出）

(require 'latex-minimal)

;; =============================================================================
;; PDF 子模块（pdf-tools，在 Emacs 内查看 PDF）

(when (and (boundp 'henri-pdf-enable-tools) henri-pdf-enable-tools)
  (require 'pdf-base))

(provide 'init-writing)

;;; init-writing.el ends here
