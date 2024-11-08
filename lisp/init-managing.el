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

;; NeoTree - 文件树侧边栏
(use-package neotree
  :ensure t
  :defer t
  :bind (("<f8>" . neotree-toggle))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(provide 'init-managing)

;;; init-managing.el ends here