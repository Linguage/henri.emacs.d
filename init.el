
;; 设置Emacs的外观
(load-theme 'modus-operandi t) ; 使用Modus Operandi主题，这是一个现代化且舒适的主题
(menu-bar-mode -1) ; 关闭菜单栏
(tool-bar-mode -1) ; 关闭工具栏
(scroll-bar-mode -1) ; 关闭滚动条
(global-display-line-numbers-mode) ; 显示行号
(column-number-mode) ; 显示列号
(global-hl-line-mode t) ; 高亮当前行
(blink-cursor-mode 0) ; 关闭光标闪烁

;; 提高Emacs的性能
(setq gc-cons-threshold 100000000) ; 增加GC阈值，提升性能
(setq inhibit-startup-screen t) ; 禁用启动屏幕
(setq auto-save-default nil) ; 禁止自动保存文件
(setq backup-inhibited t) ; 禁止备份文件
(setq ring-bell-function 'ignore) ; 关闭提示音

;; 设置快捷键
(global-set-key (kbd "C-x C-b") 'ibuffer) ; 使用ibuffer代替默认的buffer列表
(global-set-key (kbd "M-x") 'counsel-M-x) ; 使用counsel提供更好的M-x功能

;; 启用更快的模式
(global-auto-revert-mode t) ; 自动重新加载文件
(electric-pair-mode) ; 自动补全括号
(global-subword-mode 1) ; 在单词内部移动光标

;; 扩展包管理
;; 扩展包链接修改之后，可以使用package-refresh-contents进行刷新
(require 'package)
(setq package-archives '(
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ))
; (setq package-archives '(("tunu_gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;                          ("tunu_nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
;                          ("tunu_melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
; (package-refresh-contents)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))


;; 一些有用的扩展包
;; 搜索三剑客：ivy, counsel,swiper
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; 自动补全设置：company
;; Install company-mode if not already installed
(unless (package-installed-p 'company)
  (package-install 'company))

;; Enable company-mode globally
(add-hook 'after-init-hook 'global-company-mode)

;; Optional: Customize company-mode settings
(setq company-idle-delay 0.2) ; Delay before suggestions are shown
(setq company-minimum-prefix-length 2) ; Minimum prefix length before suggestions are shown

;; 语法检查：flycheck
;; Install flycheck if not already installed
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))

;; Enable flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)


;; 美化配置

;; 加载主题
(load-theme 'modus-operandi t)

;; 设置字体
; (set-frame-font "Cascadia Code NF-14" nil t)
(when (member "Cascadia Code NF" (font-family-list))
  (set-face-attribute 'default nil :family "Cascadia Code NF" :height 140))
(when (member "STKaiTi" (font-family-list))
  (dolist (charset '(han kana symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset "STKaiTi" nil 'prepend)))

;; 启用模型线
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; 启用模式线
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; 设置透明度
(add-to-list 'default-frame-alist '(alpha . (95. 95)))


;; 其他个性化设置
(setq-default indent-tabs-mode nil) ; 使用空格而不是制表符缩进


;;-------------------------
;; 配置markdown语法支持
;;-------------------------
(use-package markdown-mode
  :ensure t) ;; 依赖markdown-mode包
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(provide 'init-markdown)

(use-package markdown-preview-mode
  :ensure t)
;; 使用 grip-mode 进行实时预览
;; (use-package grip-mode
;;   :ensure t
;;   :bind ("C-c C-p" . grip-mode)
;;   :config
;; ;  (setq grip-github-user "your-github-username"
;; ;       grip-github-password "your-github-token"))
;; )
;; ;; 自动启动 markdown-mode 和 grip-mode
;; (add-hook 'markdown-mode-hook 'grip-mode)



;; Install lsp-mode and lsp-ui if not already installed
(unless (package-installed-p 'lsp-mode)
  (package-install 'lsp-mode))
(unless (package-installed-p 'lsp-ui)
  (package-install 'lsp-ui))

; ;; Optional: Install lsp-markdown for enhanced Markdown support
; (unless (package-installed-p 'lsp-markdown)
;   (package-install 'lsp-markdown))

;; Enable lsp-mode and lsp-ui in markdown-mode
(add-hook 'markdown-mode-hook #'lsp)
(add-hook 'markdown-mode-hook #'lsp-ui-mode)


(use-package treesit
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :mode (("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode)
	 ("\\.go\\'" . go-ts-mode)
	 ("/go\\.mod\\'" . go-mod-ts-mode)
	 ("\\.rs\\'" . rust-ts-mode)
	 ("\\.ts\\'" . typescript-ts-mode)
	 ("\\.y[a]?ml\\'" . yaml-ts-mode))
  :config (setq treesit-font-lock-level 4)
  :init
  (setq major-mode-remap-alist
	'((sh-mode         . bash-ts-mode)
	  (c-mode          . c-ts-mode)
	  (c++-mode        . c++-ts-mode)
	  (c-or-c++-mode   . c-or-c++-ts-mode)
	  (css-mode        . css-ts-mode)
	  (js-mode         . js-ts-mode)
	  (java-mode       . java-ts-mode)
	  (js-json-mode    . json-ts-mode)
	  (makefile-mode   . cmake-ts-mode)
	  (python-mode     . python-ts-mode)
	  (ruby-mode       . ruby-ts-mode)
	  (conf-toml-mode  . toml-ts-mode)))
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

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)

;; 结束配置
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company impatient-mode grip-mode markdown-preview-mode markdown-mode rainbow-delimiters zprint-mode doom-modeline which-key counsel ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
