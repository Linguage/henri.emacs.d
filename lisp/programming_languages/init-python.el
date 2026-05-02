;;; init_python.el --- Python 开发环境配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, python, development

;;; Commentary:

;; 本配置文件提供 Python 开发环境支持，包含以下主要模块：

;; 1. 环境管理
;;    - conda          -- Conda 环境管理
;;    - pyvenv         -- 虚拟环境支持
;;    - Henri_env      -- 个人环境配置

;; 2. 开发工具
;;    - elpy          -- Python IDE 功能
;;    - company-jedi  -- 智能补全
;;    - flycheck      -- 语法检查

;; 3. 交互环境
;;    - jupyter       -- Jupyter 支持
;;    - ein          -- Notebook 集成
;;    - dap-mode     -- 调试支持

;;; Code:

;; =============================================================================
;; 环境管理配置 (延迟加载)

;; Conda 环境配置 - 延迟加载，仅在需要时初始化
(use-package conda
  :ensure t
  :defer t  ; 延迟加载
  :commands (conda-env-activate conda-env-deactivate conda-env-list)
  :init
  (setq conda-anaconda-home (directory-file-name (expand-file-name henri-conda-home)))
  (setq conda-env-home-directory conda-anaconda-home)
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode t))

;; 虚拟环境支持 - 延迟加载
(use-package pyvenv
  :ensure t
  :defer t  ; 延迟加载
  :commands (pyvenv-activate pyvenv-deactivate pyvenv-workon)
  :hook (python-mode . pyvenv-mode)
  :config
  (setenv "WORKON_HOME" (directory-file-name (expand-file-name henri-conda-envs-directory))))

;; =============================================================================
;; IDE 功能配置 (延迟加载)

;; Python IDE 支持 - 仅在 Python 模式下加载
(use-package elpy
  :ensure t
  :defer t  ; 延迟加载
  :hook (python-mode . elpy-enable)  ; 仅在 Python 模式时启用
  :config
  (setq elpy-rpc-python-command "python3"))

;; company-mode 全局配置见 init-programming.el

;; Python 专用补全 - 仅在 Python 模式下加载
(use-package company-jedi
  :ensure t
  :defer t  ; 延迟加载
  :after company
  :hook (python-mode . (lambda () (add-to-list 'company-backends 'company-jedi))))

;; =============================================================================
;; Jupyter 支持

;; =============================================================================
;; 调试配置 (延迟加载)

;; DAP 调试支持 - 仅在需要时加载
(use-package dap-mode
  :ensure t
  :defer t  ; 延迟加载
  :commands (dap-debug dap-hydra)
  :after python
  :config
  (dap-auto-configure-mode)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy))

;; =============================================================================
;; Python 模式钩子 (延迟配置)

(defun my/python-mode-hook ()
  "Python 模式的个性化配置 - 延迟加载环境。"
  ;; 延迟激活 conda 环境，避免启动时阻塞
  (run-with-idle-timer 1 nil
                       (lambda ()
                         (when (and (featurep 'conda) (conda-env-list)
                                    (boundp 'henri-conda-default-env))
                           (conda-env-activate henri-conda-default-env))))
  (font-lock-mode 1)
  (flycheck-mode 1))

;; 仅在 Python 模式首次使用时添加钩子
(with-eval-after-load 'python
  (add-hook 'python-mode-hook 'my/python-mode-hook))

(provide 'init-python)

;;; init_python.el ends here