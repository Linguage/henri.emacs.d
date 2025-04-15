# 为 Lisp 开发配置 Emacs 环境

Lisp 是 Emacs 的亲生语言，在 Emacs 中配置优秀的 Lisp 开发环境相对容易。以下是一个完整的 Lisp 开发环境配置指南，从基础到进阶：

## 基础配置：Common Lisp 开发环境 (SLIME)

首先创建配置文件：`~/.emacs.d/lisp/programming_languages/init-lisp.el`

```elisp
;;; init-lisp.el --- Lisp 开发环境配置 -*- lexical-binding: t -*-

;;; Commentary:
;; 提供完整的 Common Lisp、Scheme 和 Emacs Lisp 开发支持

;;; Code:

;; =============================================================================
;; 通用 Lisp 编辑功能
(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode scheme-mode slime-repl-mode) . paredit-mode)
  :config
  (show-paren-mode 1)                      ; 显示匹配的括号
  (setq show-paren-style 'parenthesis))    ; 高亮整个括号表达式

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode scheme-mode slime-repl-mode) . rainbow-delimiters-mode))

;; =============================================================================
;; Common Lisp 开发环境
(use-package slime
  :ensure t
  :defer t
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
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))

;; =============================================================================
;; Scheme 开发环境
(use-package geiser
  :ensure t
  :defer t
  :config
  (setq geiser-active-implementations '(racket guile))
  (setq geiser-repl-use-other-window t))

(use-package geiser-racket
  :ensure t
  :after geiser)

(use-package geiser-guile
  :ensure t
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
```

## 进阶配置：添加更多实用功能

以下是一些额外的进阶功能，你可以根据需要添加到上面的配置中：

### 1. 添加 Sly 作为 SLIME 的替代

```elisp
;; 可选：使用 Sly 作为 SLIME 的替代
(use-package sly
  :ensure t
  :defer t
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (setq sly-complete-symbol-function 'sly-flex-completions)
  (sly-setup '(sly-fancy sly-quicklisp sly-asdf))
  :bind (:map lisp-mode-map
              ("C-c C-s" . sly)))  ;; 使用不同的键绑定，可与 SLIME 共存
```

### 2. 添加代码片段支持

```elisp
;; Lisp 代码片段
(use-package yasnippet
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

;; 常用 Lisp 代码片段集合
(use-package common-lisp-snippets
  :ensure t
  :after yasnippet)
```

### 3. Macrostep 宏展开工具

```elisp
;; 宏展开工具
(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)
              :map lisp-mode-map
              ("C-c e" . macrostep-expand)))
```

## 使用指南

安装完上述配置后，你可以使用以下方法开发 Lisp：

### Common Lisp 开发

1. **启动 SLIME**: `M-x slime`
2. **编译函数**: 将光标放在函数上，按 `C-c C-c`
3. **加载整个文件**: `C-c C-l`
4. **检查对象**: `C-c i`
5. **在 REPL 中运行**: 切换到 REPL 窗口输入表达式

### Emacs Lisp 开发

1. **求值函数**: 将光标放在函数上，按 `C-c C-c` 
2. **求值整个缓冲区**: `C-c C-b`
3. **求值区域**: 选中区域后，按 `C-c C-r`
4. **查看函数文档**: 将光标放在函数上，按 `C-h f`

### Scheme 开发

1. **启动 Geiser**: `M-x run-geiser` 选择实现 (Racket/Guile)
2. **运行函数**: 将光标放在表达式上，按 `C-c C-e`
3. **加载文件**: `C-c C-l`

## 必要软件安装

要使此配置正常工作，需要安装以下外部软件：

### macOS 安装

```bash
# 使用 Homebrew
brew install sbcl            # Common Lisp 编译器
brew install racket          # Racket Scheme
brew install guile           # GNU Guile Scheme
```

### Ubuntu/Debian 安装

```bash
sudo apt install sbcl        # Common Lisp
sudo apt install racket      # Racket
sudo apt install guile-3.0   # GNU Guile
```

## 安装 Quicklisp (Common Lisp 包管理器)

```bash
# 下载安装程序
curl -O https://beta.quicklisp.org/quicklisp.lisp

# 运行 SBCL 和安装程序
sbcl --load quicklisp.lisp

# 在 SBCL 中执行
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(quit)
```

配置完成后，你将拥有一个功能完备的 Lisp 开发环境，可以愉快地进行 Lisp 系列语言的开发了！

找到具有 3 个许可证类型的类似代码