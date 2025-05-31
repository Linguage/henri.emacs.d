;;; fix-warnings.el --- 修复Emacs启动警告和错误 -*- lexical-binding: t -*-

;; Author: Henri
;; Version: 1.0
;; Keywords: warnings, fixes, optimization

;;; Commentary:

;; 此文件用于修复和抑制Emacs启动时的各种警告和错误，包括：
;; 1. 过时的cl包警告
;; 2. 第三方包的过时代码警告 
;; 3. exec-path-from-shell性能优化
;; 4. org-bullets过时参数警告

;;; Code:

;; =============================================================================
;; 抑制过时包警告

;; 全面抑制过时cl包相关警告
(setq byte-compile-warnings '(not cl-functions obsolete))

;; 抑制一些常见的编译警告
(with-eval-after-load 'warnings
  ;; 添加到警告抑制列表
  (add-to-list 'warning-suppress-types '(comp))
  (add-to-list 'warning-suppress-types '(bytecomp))
  (add-to-list 'warning-suppress-types '(obsolete))
  
  ;; 抑制特定的cl相关警告
  (setq warning-suppress-log-types 
        '((comp) (bytecomp) (cl-functions) (obsolete))))

;; 全局抑制过时函数和宏的警告
(setq ad-redefinition-action 'accept)  ; 接受advice重定义
(setq byte-compile-warnings '(not obsolete))  ; 不显示过时警告

;; 抑制defadvice过时警告（在legacy代码中使用）
(put 'defadvice 'byte-obsolete-info nil)

;; =============================================================================
;; 修复exec-path-from-shell性能问题

(with-eval-after-load 'exec-path-from-shell
  ;; 进一步优化设置
  (setq exec-path-from-shell-debug nil)           ; 禁用调试输出
  (setq exec-path-from-shell-shell-name "zsh")    ; 明确指定shell
  (setq exec-path-from-shell-arguments '("-l"))   ; 减少参数
  
  ;; 减少检查的环境变量数量
  (setq exec-path-from-shell-variables 
        '("PATH" "SHELL" "LANG"))
  
  ;; 设置超时时间
  (setq exec-path-from-shell-check-startup-files nil)
  
  ;; 禁用一些不必要的检查
  (advice-add 'exec-path-from-shell-initialize :around
              (lambda (orig-fun &rest args)
                (let ((inhibit-message t))  ; 抑制消息输出
                  (apply orig-fun args)))))

;; =============================================================================
;; 修复第三方包的过时警告

;; 在包加载前预先定义兼容性别名
(unless (fboundp 'cl-incf)
  (defalias 'cl-incf 'incf))
(unless (fboundp 'cl-assert)
  (defalias 'cl-assert 'assert))
(unless (fboundp 'cl-loop)
  (defalias 'cl-loop 'loop))
(unless (fboundp 'cl-return)
  (defalias 'cl-return 'return))
(unless (fboundp 'cl-search)
  (defalias 'cl-search 'search))

;; 修复ctable.el的case语句警告
(with-eval-after-load 'ctable
  ;; 使用现代advice系统替代过时的defadvice
  (advice-add 'ctable:create-table-component-buffer :around
              (lambda (orig-fun &rest args)
                "抑制ctable的case语句警告。"
                (let ((byte-compile-warnings (remove 'suspicious byte-compile-warnings)))
                  (apply orig-fun args)))))

;; 修复epc.el的过时cl函数警告
(with-eval-after-load 'epc
  ;; 确保兼容性别名存在
  (require 'cl-lib nil t))

;; 修复jedi-core.el的过时cl函数警告
(with-eval-after-load 'jedi-core
  (require 'cl-lib nil t))

;; 修复tablist.el的过时cl函数警告  
(with-eval-after-load 'tablist
  (require 'cl-lib nil t))

;; =============================================================================
;; org-bullets 包的兼容性检查（移除有问题的重定义）
;; 让 org-bullets 包自己处理模式定义

;; =============================================================================
;; 性能优化设置

;; 禁用不必要的字体锁定功能以减少警告
(setq font-lock-verbose nil)
(setq font-lock-maximum-decoration t)

;; 优化编译过程
(setq byte-compile-dynamic nil)
(setq byte-compile-dynamic-docstrings nil)

;; 减少GC频率
(setq gc-cons-threshold (* 100 1024 1024))  ; 100MB
(setq gc-cons-percentage 0.6)

;; 设置启动后的恢复钩子
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; 启动完成后恢复正常GC设置
            (run-with-idle-timer
             2 nil
             (lambda ()
               (setq gc-cons-threshold (* 16 1024 1024))
               (setq gc-cons-percentage 0.1)
               (garbage-collect)))))

;; =============================================================================
;; 抑制启动时的特定消息

;; 临时重定义警告显示函数以过滤特定警告
(defvar henri/original-display-warning nil)
(defvar henri/startup-warning-filter-active t)

(defun henri/filter-startup-warnings (orig-fun type message &optional level buffer-name)
  "过滤启动时的特定警告消息。"
  (unless (and henri/startup-warning-filter-active
               (or (string-match-p "Package cl is deprecated" (format "%s" message))
                   (string-match-p "obsolete" (format "%s" message))
                   (string-match-p "defadvice.*obsolete" (format "%s" message))
                   (string-match-p "cl-.*is an obsolete" (format "%s" message))))
    (funcall orig-fun type message level buffer-name)))

;; 启动时启用警告过滤
(setq henri/original-display-warning (symbol-function 'display-warning))
(advice-add 'display-warning :around #'henri/filter-startup-warnings)

;; 抑制一些不重要的启动消息
(advice-add 'display-startup-echo-area-message :override #'ignore)

;; 减少一些包的详细输出
(setq use-package-verbose nil)
(setq use-package-compute-statistics nil)

;; =============================================================================
;; 错误恢复机制

;; 设置错误恢复
(setq debug-on-error nil)  ; 生产环境中不启用调试
(setq debug-on-quit nil)   ; 禁用退出时调试

;; 添加错误处理钩子
(add-hook 'after-init-hook
          (lambda ()
            ;; 启动完成后恢复正常警告显示（延迟5秒）
            (run-with-timer 5 nil
                            (lambda ()
                              (setq henri/startup-warning-filter-active nil)
                              (advice-remove 'display-warning #'henri/filter-startup-warnings)))
            (message "🎉 Emacs启动完成，警告修复已应用")))

(provide 'fix-warnings)

;;; fix-warnings.el ends here
