;;; org-base.el --- Org Mode 基础配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Keywords: org, base

;;; Commentary:

;; Org Mode 基础配置，包含：
;; - 基础设置
;; - 美化插件
;; - 基本功能配置

;;; Code:

;; =============================================================================
;; Org Mode 基础配置

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

;; 图片下载和管理
(use-package org-download
  :ensure t
  :config
  ;; 设置拖放图片的默认目录
  (setq org-download-image-dir "./images")
  ;; 自动插入图片链接
  (add-hook 'dired-mode-hook 'org-download-enable))

(provide 'org-base)

;;; org-base.el ends here
