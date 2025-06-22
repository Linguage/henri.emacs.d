;;; init_lisp.el --- Common lisp  开发环境配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, lisp, development

;;; init-lisp.el --- Lisp 开发环境配置 -*- lexical-binding: t -*-

;;; Commentary:
;; 提供完整的 Common Lisp、Scheme 和 Emacs Lisp 开发支持

;;; Code:

;; =============================================================================
;; 通用 Lisp 编辑功能 (延迟加载)
(use-package paredit
  :ensure t
  :defer t  ; 延迟加载
  :hook ((emacs-lisp-mode lisp-mode scheme-mode slime-repl-mode) . paredit-mode)
  :config
  (show-paren-mode 1)                      ; 显示匹配的括号
  (setq show-paren-style 'parenthesis))    ; 高亮整个括号表达式

(use-package rainbow-delimiters
  :ensure t
  :defer t  ; 延迟加载
  :hook ((emacs-lisp-mode lisp-mode scheme-mode slime-repl-mode) . rainbow-delimiters-mode))

;; =============================================================================
;; Common Lisp 开发环境 (延迟加载)
(use-package slime
  :ensure t
  :defer t  ; 延迟加载，仅在需要时启动
  :commands (slime slime-connect slime-mode)
  :init
  (setq inferior-lisp-program "sbcl")      ; 设置 Lisp 解释器
  :config
  ;; SLIME 配置
  (slime-setup '(slime-fancy                ; 包含多种常用扩展
                slime-asdf                 ; ASDF 支持
                slime-quicklisp            ; Quicklisp 支持
                slime-company              ; 公司模式集成
                slime-banner               ; 启动信息
                slime-indentation          ; 正确的缩进
                slime-tramp))              ; 远程开发支持
  
  ;; SLIME 行为设置
  (setq slime-net-coding-system 'utf-8-unix)
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (setq slime-completion-at-point-functions 'slime-simple-completion-at-point)
  
  ;; 键绑定
  :bind (:map lisp-mode-map
              ("C-c C-z" . slime)          ; 启动 SLIME
              ("C-c C-c" . slime-compile-defun)
              ("C-c C-l" . slime-load-file)
              :map slime-mode-map
              ("C-c i" . slime-inspect)))

(use-package slime-company
  :ensure t
  :defer t  ; 延迟加载
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))

;; =============================================================================
;; Scheme 开发环境 (延迟加载)
(use-package geiser
  :ensure t
  :defer t  ; 延迟加载
  :commands (geiser geiser-mode run-geiser)
  :config
  (setq geiser-active-implementations '(racket guile))
  (setq geiser-repl-use-other-window t))

(use-package geiser-racket
  :ensure t
  :defer t  ; 延迟加载
  :after geiser)

(use-package geiser-guile
  :ensure t
  :defer t  ; 延迟加载
  :after geiser)

;; =============================================================================
;; Emacs Lisp 开发环境增强
(use-package elisp-mode
  :ensure nil
  :hook ((emacs-lisp-mode . eldoc-mode)
         (emacs-lisp-mode . flycheck-mode)
         (emacs-lisp-mode . company-mode))
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . eval-defun)
              ("C-c C-b" . eval-buffer)
              ("C-c C-r" . eval-region)))

(use-package eldoc
  :ensure t
  :diminish
  :hook ((emacs-lisp-mode lisp-mode) . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.2))

;; =============================================================================
;; Lisp 代码格式化
(use-package lisp-mode
  :ensure nil
  :config
  ;; 自定义缩进规则
  (setq lisp-indent-function 'common-lisp-indent-function)
  
  ;; 格式化函数
  (defun my/format-lisp-buffer ()
    "格式化当前 Lisp 缓冲区。"
    (interactive)
    (save-excursion
      (indent-region (point-min) (point-max))))
  
  :bind (:map lisp-mode-map
              ("C-c f" . my/format-lisp-buffer)))

;; =============================================================================
;; 代码导航与文档查看
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))



(provide 'init-lisp)

;;; init-lisp.el ends here