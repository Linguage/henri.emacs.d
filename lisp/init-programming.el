;;; init-programming.el --- 编程开发环境配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: your-homepage
;; Keywords: programming, development, tools

;;; Commentary:

;; 本配置文件提供完整的编程开发环境支持，包含以下主要模块：

;; 1. 核心编程功能
;;    - company-mode    -- 智能代码补全框架
;;    - eglot          -- 轻量级 LSP 客户端
;;    - tree-sitter    -- 增强的语法分析和高亮
;;    - flycheck       -- 实时语法检查

;; 2. 编程语言支持
;;    - Lisp (SLIME)   -- Common Lisp 开发环境
;;    - C/C++          -- 支持 tree-sitter 模式
;;    - Fortran        -- 现代 Fortran 支持
;;    - Julia          -- 科学计算语言支持
;;    - Python         -- Python 开发环境
;;    - Octave         -- Octave/MATLAB 开发环境

;; 3. 开发工具
;;    - imenu          -- 代码导航和大纲
;;    - imenu-list     -- 代码结构侧边栏
;;    - realgud        -- 统一调试器界面
;;    - quickrun       -- 快速运行代码
;;    - leetcode       -- LeetCode 刷题工具

;; 使用说明：
;; 1. 确保 Emacs 版本 >= 29.1
;; 2. 安装必要的外部依赖（clangd, pylsp, fortls, octave 等）
;; 3. 按需配置各语言的 LSP 服务器

;;; Code:



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 核心编程功能配置                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; =============================================================================
;; 代码补全配置：company-mode
;; 提供智能的代码补全功能，支持多种后端补全源
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2)            ; 设置延迟显示建议的时间
  (setq company-minimum-prefix-length 2))   ; 设置触发补全的最小前缀长度

;; =============================================================================
;; 语言服务器协议(LSP)支持：eglot
;; Emacs 29+ 内置的轻量级 LSP 客户端
(use-package eglot
  :ensure t
  :bind ("C-c e f" . eglot-format)         ; 绑定格式化快捷键
  :init
  ;; 导入前自动格式化
  (advice-add 'eglot-code-action-organize-imports :before #'eglot-format-buffer)
  ;; 保存前自动格式化
  (add-hook 'eglot-managed-mode-hook 
            (lambda () 
              (add-hook 'before-save-hook #'eglot-format-buffer)))
  ;; 自动启用 eglot（除 elisp-mode 外）
  (add-hook 'prog-mode-hook
            (lambda () 
              (unless (member major-mode '(emacs-lisp-mode))
                (eglot-ensure))))
  :config
  ;; 配置语言服务器
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs '(fortran-mode . ("fortls")))
  ;; 受管 buffer 保存前格式化（尊重 size 与开关）
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (and (boundp 'henri-lsp-auto-format)
                         henri-lsp-auto-format
                         (or (not (boundp 'henri-lsp-format-size-threshold))
                             (< (buffer-size) henri-lsp-format-size-threshold)))
                (add-hook 'before-save-hook #'eglot-format-buffer nil t))))
  ;; 为特定模式启用 eglot
  (dolist (hook '(c-mode-hook
                  c++-mode-hook
                  python-mode-hook
                  fortran-mode-hook))
    (add-hook hook #'eglot-ensure)))

;; =============================================================================
;; 语法树分析：tree-sitter
;; 提供精确的语法分析和语法高亮功能

;; tree-sitter 自动安装和管理
(use-package treesit-auto
  :ensure t
  :demand t
  :config
  (setq treesit-auto-install 'prompt)      ; 提示是否安装语法解析器
  (global-treesit-auto-mode))              ; 全局启用自动模式

;; tree-sitter 核心配置
(use-package treesit
  :when (and (fboundp 'treesit-available-p) 
             (treesit-available-p))
  :mode (("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode)
         ("\\.go\\'" . go-ts-mode)
         ("/go\\.mod\\'" . go-mod-ts-mode)
         ("\\.rs\\'" . rust-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.y[a]?ml\\'" . yaml-ts-mode))
  :config
  (setq treesit-font-lock-level 4)         ; 设置语法高亮级别
  :init
  ;; 设置模式映射表
  (setq major-mode-remap-alist 
        '((sh-mode        . bash-ts-mode)
          (c-mode         . c-ts-mode)
          (c++-mode       . c++-ts-mode)
          (c-or-c++-mode  . c-or-c++-ts-mode)
          (css-mode       . css-ts-mode)
          (js-mode        . js-ts-mode)
          (java-mode      . java-ts-mode)
          (js-json-mode   . json-ts-mode)
          (julia-mode     . julia-ts-mode)
          (makefile-mode  . cmake-ts-mode)
          (python-mode    . python-ts-mode)
          (ruby-mode      . ruby-ts-mode)
          (conf-toml-mode . toml-ts-mode)))
  ;; 配置语言源
  (setq treesit-language-source-alist
        '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
          (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
          (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
          (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua        . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make       . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown   . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
          (ocaml      . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
          (org        . ("https://github.com/milisims/tree-sitter-org"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
          (php        . ("https://github.com/tree-sitter/tree-sitter-php"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql        . ("https://github.com/m-novikov/tree-sitter-sql"))
          (vue        . ("https://github.com/merico-dev/tree-sitter-vue"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (zig        . ("https://github.com/GrayJack/tree-sitter-zig")))))

;; =============================================================================
;; 语法检查：flycheck
;; 实时语法检查工具，支持多种编程语言
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

;; 大文件优化逻辑 -----------------------------------------------------------
(defun henri/large-file-optimizations ()
  "Apply performance tweaks for large files based on `henri-large-file-threshold'."
  (when (and (boundp 'henri-large-file-threshold)
             buffer-file-name
             (> (buffer-size) henri-large-file-threshold))
    (when (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode -1))
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1))
    (when (boundp 'eglot--managed-mode)
      (when eglot--managed-mode
        (message "[henri] Skipping eglot for large file: %s" (buffer-name))))
    (setq-local eglot--managed-mode nil)
    (setq-local bidi-display-reordering nil)
    (setq-local bidi-paragraph-direction 'left-to-right)
    (setq-local font-lock-maximum-decoration 1)
    (message "[henri] Large file optimizations applied (size=%d)." (buffer-size))))

(add-hook 'find-file-hook #'henri/large-file-optimizations)

(defun henri/restore-from-large-file ()
  "Manually restore common minor modes after large file optimizations."
  (interactive)
  (display-line-numbers-mode 1)
  (when (fboundp 'flycheck-mode) (flycheck-mode 1))
  (message "[henri] Restored modes for current buffer."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 编程语言支持配置                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; =============================================================================
;; Lisp 开发环境配置
;; 从单独的配置文件加载 Python 相关设置
(load-file (expand-file-name "lisp/programming_languages/init-lisp.el" user-emacs-directory))


;; =============================================================================
;; C/C++ 开发环境配置
;; 使用内置的 c-mode 和 c++-mode，配合 tree-sitter 提供现代化的开发体验

;; C 语言配置
(use-package c-mode
  :ensure nil
  :hook ((c-mode . eglot-ensure)
         (c-mode . company-mode))
  :mode (("\\.c\\'" . c-ts-mode)))

;; C++ 语言配置
(use-package c++-mode
  :ensure nil
  :hook ((c++-mode . eglot-ensure)
         (c++-mode . company-mode))
  :mode (("\\.cpp\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.h\\'" . c++-ts-mode)))

;; =============================================================================
;; 科学计算相关语言配置

;; Fortran 语言配置
(use-package fortran-mode
  :ensure nil
  :hook ((fortran-mode . eglot-ensure)
         (fortran-mode . company-mode))
  :mode (("\\.f\\'" . fortran-ts-mode)
         ("\\.f90\\'" . fortran-ts-mode)
         ("\\.f95\\'" . fortran-ts-mode)))

;; Julia 语言配置
(use-package julia-mode
  :ensure t
  :hook ((julia-mode . eglot-ensure)
         (julia-mode . company-mode))
  :mode (("\\.jl\\'" . julia-ts-mode)))

;; =============================================================================
;; Python 开发环境配置
;; 从单独的配置文件加载 Python 相关设置
(load-file (expand-file-name "lisp/programming_languages/init-python.el" user-emacs-directory))

;; =============================================================================
;; Octave 开发环境配置
;; 从单独的配置文件加载 Octave 相关设置
(load-file (expand-file-name "lisp/programming_languages/init-octave.el" user-emacs-directory))

;; =============================================================================
;; 代码导航与调试工具配置

;; 代码大纲：imenu
(use-package imenu
  :ensure t
  :config
  (setq imenu-auto-rescan t))              ; 自动重新扫描代码结构

;; 代码大纲侧边栏：imenu-list
(use-package imenu-list
  :ensure t
  :bind (("C-' C-'" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t))

;; 调试器支持：realgud
(use-package realgud
  :ensure t
  :config
  (require 'realgud)
  ;; 调试快捷键设置
  :bind (("<f6>" . realgud:pdb)
         ("<f9>" . realgud:cmd-break)
         ("<f10>" . realgud:cmd-step-over)
         ("<f11>" . realgud:cmd-step)
         ("<f12>" . realgud:cmd-next)))

;; =============================================================================
;; 代码运行工具配置

;; 快速运行代码：quickrun
(use-package quickrun
  :ensure t
  :commands (quickrun)
  :bind ("<f5>" . quickrun)
  :init
  (setq quickrun-shell "/bin/zsh")         ; 使用 zsh 作为默认 shell
  :config
  ;; C++ 运行配置
  (quickrun-add-command "c++/c1z"
    '((:command . "g++")
      (:exec . ("%c -std=c++1z %o -o %e %s"
                "%e %a"))
      (:remove . ("%e")))
    :default "c++")
  ;; C 运行配置
  (quickrun-add-command "c/gcc"
    '((:command . "gcc")
      (:exec . ("%c %o -o %e %s"
                "%e %a"))
      (:remove . ("%e")))
    :default "c")
  ;; Fortran 运行配置
  (quickrun-add-command "fortran"
    '((:command . "gfortran")
      (:exec . ("%c %o -o %e %s"
                "%e %a"))
      (:remove . ("%e")))
    :default "fortran")
  ;; Octave 运行配置
  (quickrun-add-command "octave"
    '((:command . "octave")
      (:exec . ("%c --no-gui --eval \"run('%s')\""))
      (:tempfile . nil))
    :default "octave"))

;; =============================================================================
;; LeetCode 刷题配置
(use-package leetcode
  :ensure t
  :if (and (boundp 'henri-enable-leetcode) henri-enable-leetcode)
  :commands (leetcode)
  :config
  (setq leetcode-prefer-language "python")
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/leetcode/")
  (setq leetcode-coding-preference 'contest))


(provide 'init-programming)

;;; init-programming.el ends here
