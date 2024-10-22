;;; init-programming.el --- configurations for Programmers
;; 启用的扩展有：
;; - company-mode：自动补全
;; - eglot：语言服务器协议支持
;; - tree-sitter：语法高亮和导航
;; - flycheck：语法检查
;; - rainbow-delimiters：括号和花括号着色


;;;;;;;;;;;;;;;;;;;;;;
;; Core Services    ;;
;;;;;;;;;;;;;;;;;;;;;;

;; =============================================================================
;; Company mode for autocompletionp
;; 自动补全设置：company
;; Install company-mode if not already installed
(unless (package-installed-p 'company)
  (package-install 'company))

;; Enable company-mode globally
(add-hook 'after-init-hook 'global-company-mode)

;; Optional: Customize company-mode settings
(setq company-idle-delay 0.2) ; Delay before suggestions are shown
(setq company-minimum-prefix-length 2) ; Minimum prefix length before suggestions are shown

;; =============================================================================
;; eglot for language server protocol support
;; Language Server (eglot - builtin since v29)
(use-package eglot
  :bind ("C-c e f" . eglot-format)
  :init
  (advice-add 'eglot-code-action-organize-imports :before #'eglot-format-buffer)
  (add-hook 'eglot-managed-mode-hook (lambda () (add-hook 'before-save-hook #'eglot-format-buffer)))
  (add-hook 'prog-mode-hook
	    (lambda () (unless (member major-mode '(emacs-lisp-mode))
			 (eglot-ensure)))))



;; =============================================================================
;; tree-sitter for syntax highlighting and navigation

; treesit：需要了解一下其原理和用法
; treesit-auto：自动安装treesit
(use-package treesit-auto
  :ensure t
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; 配置treesit
;; 这里的配置是根据官方文档进行的，并没有使用任何插件，仅仅是配置了treesit的语法高亮和导航。
;; 具体的语法高亮和导航功能需要使用插件来实现。
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
	  (julia-mode      . julia-ts-mode)
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

;; =============================================================================

;; Flycheck for syntax checking
;; 语法检查：flycheck
;; Install flycheck if not already installed
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))

;; Enable flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)
;; Rainbow delimiters for colorizing parentheses and braces

;; =============================================================================
;; 编程语言配置

; ;; 启用 eglot
; (require 'eglot)

; ;; 自动启动 eglot
; (add-hook 'c-mode-hook 'eglot-ensure)
; (add-hook 'c++-mode-hook 'eglot-ensure)
; (add-hook 'python-mode-hook 'eglot-ensure)
; (add-hook 'fortran-mode-hook 'eglot-ensure)
; (add-hook 'julia-mode-hook 'eglot-ensure)
; (add-hook 'haskell-mode-hook 'eglot-ensure)
; (add-hook 'rust-mode-hook 'eglot-ensure)


(require 'eglot)
;; C/C++
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;; Python
(add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
;; Fortran
(add-to-list 'eglot-server-programs '(fortran-mode . ("fortls")))

(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'fortran-mode-hook 'eglot-ensure)

;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp programming ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; MELPA repository
; (setq inferior-lisp-program "sbcl")

;; Load SLIME when starting Emacs
(setq inferior-lisp-program "/opt/homebrew/bin/sbcl")  ;; 根据 SBCL 的安装路径设置
; (add-to-list 'load-path "/path/to/slime/")  ;; SLIME 的安装路径
(use-package slime
  :ensure t
)
(slime-setup)

;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ programming ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package c-mode
  :ensure nil ; c-mode is built-in
  :hook ((c-mode . eglot-ensure)
         (c-mode . company-mode))
  :mode (("\\.c\\'" . c-ts-mode)))

(use-package c++-mode
  :ensure nil ; c++-mode is built-in
  :hook ((c++-mode . eglot-ensure)
         (c++-mode . company-mode))
  :mode (("\\.cpp\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.h\\'" . c++-ts-mode)))

(use-package fortran-mode
  :ensure nil ; fortran-mode is built-in
  :hook ((fortran-mode . eglot-ensure)
         (fortran-mode . company-mode))
  :mode (("\\.f\\'" . fortran-ts-mode)
         ("\\.f90\\'" . fortran-ts-mode)
         ("\\.f95\\'" . fortran-ts-mode)))

(use-package julia-mode
  :ensure t
  :hook ((julia-mode . eglot-ensure)
         (julia-mode . company-mode))
  :mode (("\\.jl\\'" . julia-ts-mode)))


(use-package quickrun
  :ensure t
  :commands (quickrun)
  :init
;; 设置默认使用 zsh 作为 shell
  (setq quickrun-shell "/bin/zsh")
  ;; C++ 配置
  (quickrun-add-command "c++/c1z"
    '((:command . "g++")
      (:exec . ("%c -std=c++1z %o -o %e %s"
                "%e %a"))
      (:remove . ("%e")))
    :default "c++")
	  ;; C 配置
  (quickrun-add-command "c/gcc"
    '((:command . "gcc")
      (:exec . ("%c %o -o %e %s"
                "%e %a"))
      (:remove . ("%e")))
    :default "c")
  ;; Fortran 配置
  (quickrun-add-command "fortran"
    '((:command . "gfortran")
      (:exec . ("%c %o -o %e %s"
                "%e %a"))
      (:remove . ("%e")))
    :default "fortran")
)

(global-set-key (kbd "<f5>") 'quickrun)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Python programming ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package pyvenv
  :ensure t
  :config
  (setenv "WORKON_HOME" "~/.conda/envs")
  (pyvenv-mode 1))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-jedi
  :ensure t
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda3/"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3/"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode t))

(defun my/python-mode-hook ()
  (conda-env-activate "base")) ; 默认激活的conda环境

(add-hook 'python-mode-hook 'my/python-mode-hook)

(defun my/python-mode-setup ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-setup)

(add-hook 'python-mode-hook 'font-lock-mode)

(use-package imenu
  :ensure t
  :config
  (setq imenu-auto-rescan t))

(use-package imenu-list
  :ensure t
  :bind (("C-' C-'" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t))

(use-package realgud
  :ensure t
  :config
  (require 'realgud))


(global-set-key (kbd "<f6>") 'realgud:pdb)
(global-set-key (kbd "<f9>") 'realgud:cmd-break)
(global-set-key (kbd "<f10>") 'realgud:cmd-step-over)
(global-set-key (kbd "<f11>") 'realgud:cmd-step)
(global-set-key (kbd "<f12>") 'realgud:cmd-next)

; (use-package highlight-indent-guides
;   :ensure t
;   :hook (python-ts-mode . highlight-indent-guides-mode)
;   :config
;   (set-face-foreground 'highlight-indent-guides-character-face "white")
;   (setq highlight-indent-guides-method 'character))

(use-package leetcode
  :ensure t
  :config
  (setq leetcode-prefer-language "python") ;; 选择你喜欢的语言，例如： "python", "cpp", "java", etc.
  (setq leetcode-save-solutions t)         ;; 保存解决方案到文件
  (setq leetcode-directory "~/leetcode/")  ;; 保存解决方案的目录
  (setq leetcode-coding-preference 'contest)) ;; 可以选择 'contest 或 'study，取决于你的用途

(provide 'init-programming)

;;; init-programming.el ends here
