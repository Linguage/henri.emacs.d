;;; init-LangServ.el --- configurations for Programmers

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

;; 启用 eglot
(require 'eglot)

;; 自动启动 eglot
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'fortran-mode-hook 'eglot-ensure)
(add-hook 'julia-mode-hook 'eglot-ensure)
(add-hook 'haskell-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)


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




;;;;;;;;;;;;;;;;;;;;;;
;; Rust Programming ;;
;;;;;;;;;;;;;;;;;;;;;;
(use-package rust-mode :ensure t)


;; Julia programming language support
;; 为.jl文件启用julia-mode(只有打开.jl文件才会启用julia-mode)
; (defun ensure-julia-mode-installed ()
;   "Ensure that `julia-mode` is installed. If not, install it."
;   (unless (package-installed-p 'julia-mode)
;     (package-refresh-contents)
;     (package-install 'julia-mode)))
; (add-to-list 'auto-mode-alist '("\\.jl\\'" . (lambda ()
;                                                (ensure-julia-mode-installed)
;                                                (julia-mode))))
(use-package julia-mode :ensure t)

(provide 'init-programming)

;;; init-programming.el ends here
