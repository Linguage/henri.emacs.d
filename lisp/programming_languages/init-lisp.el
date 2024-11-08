;; =============================================================================
;; Lisp 开发环境配置
;; SLIME：Superior Lisp Interaction Mode for Emacs
(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")  ; 设置 SBCL 解释器路径
  :config
  (slime-setup '(slime-fancy                    ; 加载扩展模块
                 slime-asdf                      ; ASDF 支持
                 slime-quicklisp                 ; Quicklisp 支持
                 slime-banner                    ; 显示欢迎信息
                 slime-repl                      ; REPL 增强
                 slime-autodoc))                 ; 自动文档
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol) ; 模糊补全
  (setq slime-net-coding-system 'utf-8-unix))   ; 设置编码

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