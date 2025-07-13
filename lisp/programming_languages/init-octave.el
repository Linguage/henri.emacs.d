;;; init-octave.el --- Octave 开发环境配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: your-homepage
;; Keywords: octave, matlab, scientific-computing

;;; Commentary:

;; 本配置文件提供完整的 Octave/MATLAB 开发环境支持，包含以下功能：

;; 1. 核心功能
;;    - octave-mode     -- Octave 语法高亮和编辑
;;    - octave-inferior -- 交互式 Octave REPL
;;    - company-mode    -- 智能代码补全
;;    - flycheck        -- 语法检查

;; 2. 开发工具
;;    - 代码格式化      -- 自动缩进和格式化
;;    - 快速运行        -- F5 快速执行 Octave 脚本
;;    - 调试支持        -- 断点和调试功能
;;    - 帮助系统        -- 内置文档查看

;; 3. 增强功能
;;    - 语法高亮        -- 完整的 Octave 语法支持
;;    - 括号匹配        -- 智能括号配对
;;    - 代码折叠        -- 函数和代码块折叠
;;    - 自动补全        -- 函数名和变量补全

;; 使用说明：
;; 1. 确保系统已安装 Octave (brew install octave 或包管理器安装)
;; 2. 打开 .m 文件自动启用 octave-mode
;; 3. C-c C-i 启动交互式 Octave
;; 4. C-c C-r 运行当前区域
;; 5. C-c C-b 运行整个缓冲区
;; 6. F5 快速运行当前文件

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Octave 核心配置                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; =============================================================================
;; Octave 主模式配置
;; 提供 Octave/MATLAB 语法高亮、缩进和编辑功能
(use-package octave
  :ensure nil  ; 内置包
  :mode (("\\.m\\'" . octave-mode))
  :hook ((octave-mode . company-mode)
         (octave-mode . flycheck-mode)
         (octave-mode . octave-setup-environment))
  :bind (:map octave-mode-map
         ("C-c C-i" . octave-inferior-buffer)     ; 启动交互式 Octave
         ("C-c C-r" . octave-send-region)        ; 发送区域到 Octave
         ("C-c C-b" . octave-send-buffer)        ; 发送整个缓冲区
         ("C-c C-f" . octave-send-defun)         ; 发送当前函数
         ("C-c C-l" . octave-send-line)          ; 发送当前行
         ("C-c C-h" . octave-help)               ; 查看帮助
         ("C-c C-d" . octave-describe-function)  ; 描述函数
         ("M-." . octave-find-definition)        ; 跳转到定义
         ("M-," . pop-tag-mark)                  ; 返回
         ("C-c C-k" . octave-kill-process))      ; 终止 Octave 进程
  :config
  ;; 保证 Emacs 能找到 Homebrew 安装的 Octave
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
  (setq inferior-octave-program "/opt/homebrew/bin/octave")
  
  ;; 基本编辑设置
  (setq octave-auto-indent t)                   ; 自动缩进
  (setq octave-blink-matching-block t)          ; 匹配代码块闪烁
  (setq octave-block-offset 2)                  ; 代码块缩进
  (setq octave-continuation-offset 2)           ; 续行缩进
  
  ;; 语法高亮增强
  (setq octave-font-lock-texinfo-comment t)     ; 启用 Texinfo 注释高亮
  
  ;; 交互式 Octave 设置
  (setq inferior-octave-startup-args '("--no-gui" "--quiet" "--traditional"))
  (setq inferior-octave-prompt ">> ")           ; 设置提示符
  
  ;; 文件关联
  (add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
  
  ;; 环境设置函数
  (defun octave-setup-environment ()
    "设置 Octave 开发环境"
    ;; 启用行号
    (display-line-numbers-mode 1)
    ;; 启用括号匹配
    (show-paren-mode 1)
    ;; 设置注释样式
    (setq comment-start "% ")
    (setq comment-end "")
    ;; 启用自动配对
    (electric-pair-local-mode 1)
    ;; 设置缩进
    (setq tab-width 2)
    (setq indent-tabs-mode nil)))

;; =============================================================================
;; Octave 代码补全配置
;; 为 octave-mode 配置智能补全
(use-package company
  :ensure t
  :hook (octave-mode . company-mode)
  :config
  ;; 为 Octave 添加特定的补全后端
  (defun octave-company-setup ()
    "设置 Octave 的 company 补全"
    (set (make-local-variable 'company-backends)
         '((company-octave company-dabbrev-code company-keywords)
           company-files company-dabbrev)))
  
  (add-hook 'octave-mode-hook #'octave-company-setup)
  
  ;; 定义 Octave 关键字补全后端
  (defvar company-octave-keywords
    '("break" "case" "catch" "continue" "do" "else" "elseif" "end" "end_try_catch"
      "end_unwind_protect" "endfor" "endfunction" "endif" "endswitch" "endwhile"
      "for" "function" "global" "if" "otherwise" "persistent" "return" "switch"
      "try" "until" "unwind_protect" "unwind_protect_cleanup" "while"
      ;; 内置函数
      "abs" "acos" "asin" "atan" "atan2" "ceil" "cos" "exp" "fix" "floor"
      "log" "log10" "max" "min" "mod" "rand" "randn" "round" "sign" "sin"
      "sqrt" "tan" "zeros" "ones" "eye" "diag" "length" "size" "find"
      "sort" "sum" "mean" "std" "var" "plot" "figure" "hold" "title"
      "xlabel" "ylabel" "legend" "grid" "axis" "clf" "close" "subplot")
    "Octave 关键字和常用函数列表")
  
  (defun company-octave (command &optional arg &rest ignored)
    "Octave 补全后端"
    (interactive (list 'interactive))
    (case command
      (interactive (company-begin-backend 'company-octave))
      (prefix (and (eq major-mode 'octave-mode)
                   (company-grab-symbol)))
      (candidates (all-completions arg company-octave-keywords))
      (sorted t))))

;; =============================================================================
;; Octave 语法检查配置
;; 使用 flycheck 进行语法检查
(use-package flycheck
  :ensure t
  :hook (octave-mode . flycheck-mode)
  :config
  ;; 定义 Octave 语法检查器
  (flycheck-define-checker octave-syntax
    "Octave 语法检查器"
    :command ("octave" "--eval" "try; source(buffer-file-name); catch; end; quit;")
    :error-patterns
    ((error line-start "error: " (message) " near line " line)
     (warning line-start "warning: " (message) " near line " line))
    :modes octave-mode)
  
  (add-to-list 'flycheck-checkers 'octave-syntax))

;; =============================================================================
;; Octave 调试支持配置
;; 提供调试功能和断点支持
(use-package octave
  :config
  ;; 调试相关函数
  (defun octave-debug-file ()
    "在调试模式下运行当前文件"
    (interactive)
    (let ((file (buffer-file-name)))
      (when file
        (octave-send-string (format "debug %s" (file-name-sans-extension 
                                               (file-name-nondirectory file)))))))
  
  (defun octave-set-breakpoint ()
    "在当前行设置断点"
    (interactive)
    (let ((line (line-number-at-pos))
          (file (file-name-sans-extension 
                 (file-name-nondirectory (buffer-file-name)))))
      (octave-send-string (format "dbstop %s %d" file line))
      (message "在 %s 第 %d 行设置断点" file line)))
  
  (defun octave-clear-breakpoints ()
    "清除所有断点"
    (interactive)
    (octave-send-string "dbclear all")
    (message "已清除所有断点"))
  
  (defun octave-step-debug ()
    "单步调试"
    (interactive)
    (octave-send-string "dbstep"))
  
  (defun octave-continue-debug ()
    "继续执行"
    (interactive)
    (octave-send-string "dbcont"))
  
  ;; 绑定调试快捷键
  (define-key octave-mode-map (kbd "<f9>") 'octave-set-breakpoint)
  (define-key octave-mode-map (kbd "S-<f9>") 'octave-clear-breakpoints)
  (define-key octave-mode-map (kbd "<f10>") 'octave-step-debug)
  (define-key octave-mode-map (kbd "<f11>") 'octave-continue-debug)
  (define-key octave-mode-map (kbd "C-<f5>") 'octave-debug-file))

;; =============================================================================
;; Octave 代码格式化配置
;; 提供代码格式化和美化功能
(use-package octave
  :config
  (defun octave-format-buffer ()
    "格式化整个 Octave 缓冲区"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (octave-indent-line)
        (forward-line 1)))
    (message "缓冲区格式化完成"))
  
  (defun octave-format-region (start end)
    "格式化选定区域"
    (interactive "r")
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (octave-indent-line)
        (forward-line 1)))
    (message "区域格式化完成"))
  
  ;; 绑定格式化快捷键
  (define-key octave-mode-map (kbd "C-c C-q") 'octave-format-buffer)
  (define-key octave-mode-map (kbd "C-c C-u") 'octave-format-region))

;; =============================================================================
;; Octave 模板和代码片段
;; 提供常用的代码模板
(use-package octave
  :config
  (defun octave-insert-function-template ()
    "插入函数模板"
    (interactive)
    (let ((func-name (read-string "函数名: ")))
      (insert (format "function [output] = %s(input)\n" func-name))
      (insert "%% 函数说明\n")
      (insert "%%\n")
      (insert "%% 输入参数:\n")
      (insert "%%   input - 输入参数描述\n")
      (insert "%%\n")
      (insert "%% 输出参数:\n")
      (insert "%%   output - 输出参数描述\n")
      (insert "%%\n")
      (insert "%% 示例:\n")
      (insert (format "%%   result = %s(data);\n" func-name))
      (insert "\n")
      (insert "% 函数实现\n")
      (insert "output = input;\n")
      (insert "\n")
      (insert "end\n")))
  
  (defun octave-insert-script-template ()
    "插入脚本模板"
    (interactive)
    (insert "%% Octave 脚本\n")
    (insert (format "%% 创建时间: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
    (insert "%% 作者: \n")
    (insert "%% 描述: \n")
    (insert "\n")
    (insert "clear all;\n")
    (insert "close all;\n")
    (insert "clc;\n")
    (insert "\n")
    (insert "%% 主要代码\n")
    (insert "\n"))
  
  ;; 绑定模板插入快捷键
  (define-key octave-mode-map (kbd "C-c C-t f") 'octave-insert-function-template)
  (define-key octave-mode-map (kbd "C-c C-t s") 'octave-insert-script-template))

;; =============================================================================
;; Octave 增强功能配置
;; 提供额外的便利功能
(use-package octave
  :config
  ;; 显示函数参数提示
  (defun octave-show-function-help ()
    "显示当前光标处函数的帮助信息"
    (interactive)
    (let ((func (thing-at-point 'symbol)))
      (when func
        (octave-send-string (format "help %s" func)))))
  
  ;; 快速运行测试
  (defun octave-run-tests ()
    "运行当前目录下的测试文件"
    (interactive)
    (octave-send-string "runtests"))
  
  ;; 绑定增强功能快捷键
  (define-key octave-mode-map (kbd "C-c C-?") 'octave-show-function-help)
  (define-key octave-mode-map (kbd "C-c C-t t") 'octave-run-tests))

;; =============================================================================
;; Octave 项目管理配置
;; 提供项目级别的功能支持
(use-package octave
  :config
  (defun octave-setup-project ()
    "为当前目录设置 Octave 项目环境"
    (interactive)
    (let ((project-dir (read-directory-name "项目目录: " default-directory)))
      (octave-send-string (format "cd('%s')" project-dir))
      (octave-send-string "addpath(pwd)")
      (message "已设置项目目录: %s" project-dir)))
  
  (defun octave-clear-workspace ()
    "清除 Octave 工作空间"
    (interactive)
    (octave-send-string "clear all")
    (message "已清除工作空间"))
  
  ;; 绑定项目管理快捷键
  ;; 修复 C-c C-p s/c 非前缀键问题
  (defvar octave-project-prefix-map (make-sparse-keymap)
    "Octave 项目管理前缀键映射")
  (define-key octave-project-prefix-map (kbd "s") 'octave-setup-project)
  (define-key octave-project-prefix-map (kbd "c") 'octave-clear-workspace)
  (define-key octave-mode-map (kbd "C-c C-p") octave-project-prefix-map))

(provide 'init-octave)

;;; init-octave.el ends here