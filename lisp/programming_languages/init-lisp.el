;;; init_lisp.el --- Common lisp  开发环境配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, lisp, development

;; ============================================================================
;; Lisp 开发环境配置
;; SLIME：Superior Lisp Interaction Mode for Emacs
(use-package slime
  :ensure t
  :init
  ;; 设置 SBCL 解释器路径
  (setq inferior-lisp-program (if (eq system-type 'darwin)
                                  "/opt/homebrew/bin/sbcl" ; macOS 路径
                                  "/usr/bin/sbcl"))        ; Linux 路径
  :config
  ;; 加载扩展模块
  (slime-setup '(slime-fancy                    ; 常用增强功能
                 slime-asdf                      ; ASDF 支持
                 slime-quicklisp                 ; Quicklisp 支持
                 slime-banner                    ; 欢迎信息
                 slime-repl                      ; REPL 增强
                 slime-autodoc))                 ; 自动文档

  ;; 设置模糊补全
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

  ;; 设置编码
  (setq slime-net-coding-system 'utf-8-unix))


;; ParEdit - 结构化编辑
(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode slime-repl-mode) . paredit-mode)
  :config
  (show-paren-mode t))                          ; 显示匹配的括号

;; Rainbow Delimiters - 彩虹括号
(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode) . rainbow-delimiters-mode))

;; Lisp Doc - 文档查看
(use-package lisp-extra-font-lock
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode) . lisp-extra-font-lock-mode))

;; Common Lisp 代码格式化
; (use-package sly
;   :ensure t
;   :config
;   (setq sly-complete-symbol-function 'sly-flex-completions))

;; macrostep - 宏展开
(use-package macrostep
  :ensure t
  :bind ("C-c e" . macrostep-expand)
  :config
  (setq macrostep-expand-in-separate-buffer t))

(provide 'init-lisp)