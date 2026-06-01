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
;; 彩虹括号（编程阅读；由 `init-styling'/视觉层迁出）
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; =============================================================================
;; 代码补全配置：company-mode
;; 提供智能的代码补全功能，支持多种后端补全源
(defvar henri-company-blacklisted-modes '(org-mode markdown-mode gfm-mode text-mode fundamental-mode)
  "Modes where `company-mode' should NOT activate.")

(defun henri--global-company-maybe-enable ()
  "Enable `company-mode' unless current major-mode is blacklisted."
  (unless (apply #'derived-mode-p henri-company-blacklisted-modes)
    (company-mode 1)))

(use-package company
  :ensure t
  :defer 1
  :hook (after-init . (lambda ()
                        (add-hook 'after-change-major-mode-hook
                                  #'henri--global-company-maybe-enable)))
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2))

;; -----------------------------------------------------------------------------
;; 为新语言接 LSP 时：在 `:config' 里加 `eglot-server-programs'，并把对应
;; `major-mode' 记入 `henri-eglot-auto-major-modes'（见 `init-custom.el'）。

(defun henri--eglot-ensure-if-whitelisted ()
  "Call `eglot-ensure' when `major-mode' is in `henri-eglot-auto-major-modes'."
  (when (and (boundp 'henri-eglot-auto-major-modes)
             henri-eglot-auto-major-modes
             (memq major-mode henri-eglot-auto-major-modes)
             (require 'eglot nil t)
             (boundp 'eglot-server-programs))
    (let* ((server-entry (seq-find
                          (lambda (entry)
                            (let ((modes (car entry)))
                              (if (listp modes)
                                  (memq major-mode modes)
                                (eq major-mode modes))))
                          eglot-server-programs))
           (server-command (cadr server-entry)))
      (cond
       ((null server-command)
        (message "[henri] Skip eglot: no server configured for %s" major-mode))
       ((and (stringp server-command)
             (not (executable-find server-command)))
        (message "[henri] Skip eglot: executable not found: %s" server-command))
       ((and (listp server-command)
             (stringp (car server-command))
             (not (executable-find (car server-command))))
        (message "[henri] Skip eglot: executable not found: %s" (car server-command)))
       (t
        (eglot-ensure))))))

;; =============================================================================
;; 语言服务器协议(LSP)支持：eglot
;; Emacs 29+ 内置的轻量级 LSP 客户端
(use-package eglot
  :ensure t
  :bind ("C-c e f" . eglot-format)         ; 绑定格式化快捷键
  :init
  (advice-add 'eglot-code-action-organize-imports :before #'eglot-format-buffer)
  (add-hook 'prog-mode-hook #'henri--eglot-ensure-if-whitelisted)
  :config
  ;; 配置语言服务器（含 tree-sitter 主模式，与 `henri-eglot-auto-major-modes' 一致）
  (add-to-list 'eglot-server-programs '((c++-mode c-mode c++-ts-mode c-ts-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs '(fortran-mode . ("fortls")))
  ;; 受管 buffer 保存前格式化（尊重 size 与开关）
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (and (boundp 'henri-lsp-auto-format)
                         henri-lsp-auto-format
                         (or (not (boundp 'henri-lsp-format-size-threshold))
                             (< (buffer-size) henri-lsp-format-size-threshold)))
                (add-hook 'before-save-hook #'eglot-format-buffer nil t)))))

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
  (defun henri--treesit-mode-remap (language from-mode to-mode)
    "Return a major-mode remap when LANGUAGE grammar is available."
    (when (and (fboundp 'treesit-language-available-p)
               (treesit-language-available-p language))
      (cons from-mode to-mode)))

  ;; 设置模式映射表
  (setq major-mode-remap-alist 
        (delq nil
              (list
               (henri--treesit-mode-remap 'bash 'sh-mode 'bash-ts-mode)
               (henri--treesit-mode-remap 'c 'c-mode 'c-ts-mode)
               (henri--treesit-mode-remap 'cpp 'c++-mode 'c++-ts-mode)
               (henri--treesit-mode-remap 'cpp 'c-or-c++-mode 'c-or-c++-ts-mode)
               (henri--treesit-mode-remap 'css 'css-mode 'css-ts-mode)
               (henri--treesit-mode-remap 'javascript 'js-mode 'js-ts-mode)
               (henri--treesit-mode-remap 'java 'java-mode 'java-ts-mode)
               (henri--treesit-mode-remap 'json 'js-json-mode 'json-ts-mode)
               (henri--treesit-mode-remap 'julia 'julia-mode 'julia-ts-mode)
               (henri--treesit-mode-remap 'cmake 'makefile-mode 'cmake-ts-mode)
               (henri--treesit-mode-remap 'python 'python-mode 'python-ts-mode)
               (henri--treesit-mode-remap 'ruby 'ruby-mode 'ruby-ts-mode)
               (henri--treesit-mode-remap 'toml 'conf-toml-mode 'toml-ts-mode))))
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

;; 大文件优化：`lib-files' 中的 `henri/large-file-optimizations'/`henri--prepare-for-large-files-a'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 编程语言支持配置                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lisp / Python / Octave：独立模块
(require 'init-lisp)
(require 'init-python)
(require 'init-octave)

(defun henri--julia-mode-or-ts ()
  "Prefer `julia-ts-mode' when the Julia grammar is available."
  (require 'julia-mode)
  (if (and (fboundp 'julia-ts-mode)
           (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'julia))
      (julia-ts-mode)
    (julia-mode)))

(use-package julia-mode
  :ensure t
  :mode ("\\.jl\\'" . henri--julia-mode-or-ts))

;; =============================================================================
;; 代码导航与调试工具配置

;; 代码大纲：imenu
(use-package imenu
  :ensure nil
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
  :defer t
  :config
  (require 'realgud)
  ;; 调试快捷键设置
  :bind (("C-c d p" . realgud:pdb)
         ("C-c d b" . realgud:cmd-break)
         ("C-c d o" . realgud:cmd-step-over)
         ("C-c d s" . realgud:cmd-step)
         ("C-c d n" . realgud:cmd-next)))

;; =============================================================================
;; 代码运行工具配置

;; 快速运行代码：quickrun
(use-package quickrun
  :ensure t
  :commands (quickrun)
  :bind ("C-c d r" . quickrun)
  :init
  (setq quickrun-shell henri-shell)
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
  (setq leetcode-prefer-language "c")
  (setq leetcode-save-solutions t)
  (setq leetcode-directory (directory-file-name (expand-file-name henri-leetcode-directory)))
  (setq leetcode-coding-preference 'contest)

  (defun henri-leetcode--quiet-solution-buffer ()
    "Disable noisy diagnostics in LeetCode solution buffers."
    (setq-local henri-eglot-auto-major-modes nil)
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1))
    (when (and (fboundp 'eglot-managed-p)
               (eglot-managed-p))
      (ignore-errors (eglot-shutdown))))

  (add-hook 'leetcode-solution-mode-hook #'henri-leetcode--quiet-solution-buffer)

  (defun leetcode--cookie-get-all ()
    "Get valid LeetCode cookies with `my_cookies'."
    (let* ((my-cookies-output (shell-command-to-string (leetcode--my-cookies-path)))
           (cookies-list (seq-filter (lambda (s) (not (string-empty-p s)))
                                     (s-split "\n" my-cookies-output 'OMIT-NULLS)))
           (cookies-pairs (seq-map (lambda (s)
                                     (s-split-up-to " " s 1 'OMIT-NULLS))
                                   cookies-list)))
      (seq-filter (lambda (pair)
                    (member (car pair) '("LEETCODE_SESSION" "csrftoken")))
                  cookies-pairs)))

  (defun henri-leetcode--difficulty-name (level)
    "Convert LeetCode REST difficulty LEVEL to a package difficulty string."
    (pcase level
      (1 "Easy")
      (2 "Medium")
      (3 "Hard")
      (_ "")))

  (defun henri-leetcode--rest-problem-from-alist (item)
    "Build a `leetcode-problem' from one REST API ITEM."
    (let-alist item
      (let* ((submitted (or .stat.total_submitted 0))
             (acceptance (if (zerop submitted)
                             "0.0%"
                           (format "%.1f%%"
                                   (* 100.0 (/ (float .stat.total_acs) submitted))))))
        (make-leetcode-problem
         :status     .status
         :id         (number-to-string .stat.frontend_question_id)
         :backend-id (number-to-string .stat.question_id)
         :title      .stat.question__title
         :title-slug .stat.question__title_slug
         :acceptance acceptance
         :difficulty (henri-leetcode--difficulty-name .difficulty.level)
         :paid-only  (eq .paid_only t)
         :tags       '()))))

  (aio-defun henri-leetcode--fetch-all-problems-rest ()
    "Fetch all LeetCode problems with the legacy REST endpoint."
    (let* ((url-request-method "GET")
           (url-request-extra-headers `(,leetcode--User-Agent))
           (response (aio-await (aio-url-retrieve leetcode--url-all-problems)))
           (response-status (car response))
           (response-buffer (cdr response)))
      (unwind-protect
          (if-let ((error (plist-get response-status :error)))
              (user-error "LeetCode problem list fetch failed: %S" error)
            (let-alist (with-current-buffer response-buffer
                         (goto-char url-http-end-of-headers)
                         (json-read))
              (cons .num_total
                    (sort
                     (mapcar #'henri-leetcode--rest-problem-from-alist
                             (seq-filter
                              (lambda (item)
                                (let-alist item
                                  (not (eq .stat.question__hide t))))
                              .stat_status_pairs))
                     (lambda (a b)
                       (< (string-to-number (leetcode-problem-id a))
                          (string-to-number (leetcode-problem-id b))))))))
        (when (buffer-live-p response-buffer)
          (kill-buffer response-buffer)))))

  (aio-defun leetcode-refresh-fetch ()
    "Refresh problems and update `tabulated-list-entries'.
This compatibility override uses LeetCode's full REST problem list.  The
package's original GraphQL refresh currently assumes a 4000 item response, but
the endpoint caps the returned page and can fail during activation."
    (interactive)
    (message "LeetCode refreshing question list...")
    (let* ((result (aio-await (henri-leetcode--fetch-all-problems-rest)))
           (total (car result))
           (problems (cdr result)))
      (setq leetcode--problems
            (make-leetcode-problems
             :num total
             :tag "all"
             :problems problems))
      (setq leetcode--all-tags nil)
      (setq leetcode--display-tags leetcode-prefer-tag-display)
      (leetcode-reset-filter-and-refresh))))


(provide 'init-programming)

;;; init-programming.el ends here
