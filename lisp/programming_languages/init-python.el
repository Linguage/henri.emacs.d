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
  (setq conda-anaconda-home (expand-file-name "~/miniconda3/"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3/"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode t))

;; 虚拟环境支持 - 延迟加载
(use-package pyvenv
  :ensure t
  :defer t  ; 延迟加载
  :commands (pyvenv-activate pyvenv-deactivate pyvenv-workon)
  :config
  (setenv "WORKON_HOME" "~/.conda/envs")
  (pyvenv-mode 1))

; ;; tree-siter
;     (use-package tree-sitter
;     :ensure t
;     :hook (python-mode . tree-sitter-mode)
;           (python-mode . tree-sitter-hl-mode))
  
;   (use-package tree-sitter-langs
;     :ensure t
;     :after tree-sitter)

;; =============================================================================
;; IDE 功能配置 (延迟加载)

;; Python IDE 支持 - 仅在 Python 模式下加载
(use-package elpy
  :ensure t
  :defer t  ; 延迟加载
  :hook (python-mode . elpy-enable)  ; 仅在 Python 模式时启用
  :config
  (setq elpy-rpc-python-command "python3"))

;; 代码补全 - 全局但延迟加载
(use-package company
  :ensure t
  :defer 2  ; 延迟 2 秒加载
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2))

;; Python 专用补全 - 仅在 Python 模式下加载
(use-package company-jedi
  :ensure t
  :defer t  ; 延迟加载
  :after company
  :hook (python-mode . (lambda () (add-to-list 'company-backends 'company-jedi))))

;; =============================================================================
;; Jupyter 支持

; (use-package ein
;   :ensure t
;   :config
;   (setq ein:output-area-inlined-images t))  ; 支持显示内嵌图片

; (use-package jupyter
;   :ensure t
;   :config
;   (setq jupyter-repl-echo-eval-p t))        ; 显示执行的代码

;; =============================================================================
;; 调试配置 (延迟加载)

;; DAP 调试支持 - 仅在需要时加载
(use-package dap-mode
  :ensure t
  :defer t  ; 延迟加载
  :commands (dap-debug dap-hydra)
  :after lsp-mode
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
                         (when (and (featurep 'conda) (conda-env-list))
                           (conda-env-activate "Henri_env"))))
  (font-lock-mode 1)
  (flycheck-mode 1))

;; 仅在 Python 模式首次使用时添加钩子
(with-eval-after-load 'python
  (add-hook 'python-mode-hook 'my/python-mode-hook))

(provide 'init-python)

;;; init_python.el ends here