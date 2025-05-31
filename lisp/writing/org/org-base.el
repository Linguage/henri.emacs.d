;;; org-base.el --- Org Mode 基础配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 2.0
;; Keywords: org, base, theme, beautify

;;; Commentary:

;; Org Mode 基础配置，包含：
;; - 基础设置
;; - 美化插件和主题
;; - 字体和颜色配置
;; - 基本功能配置

;;; Code:

;; =============================================================================
;; Org Mode 基础配置

(use-package org
  :ensure t
  :mode (("\\.org$" . org-mode))
  :init
  (setq org-startup-indented t)           ; 启用缩进
  (setq org-startup-with-inline-images t) ; 显示内联图片
  (setq org-startup-folded 'showeverything) ; 启动时展开所有内容
  (setq org-hide-emphasis-markers t)      ; 隐藏标记符号
  (setq org-pretty-entities t)            ; 美化特殊字符
  (setq org-fontify-quote-and-verse-blocks t) ; 美化引用和诗歌块
  :config
  (setq org-log-done 'time)              ; 记录完成时间
  (setq org-ellipsis " ▾")               ; 自定义省略号
  (setq org-src-fontify-natively t)      ; 代码块语法高亮
  (setq org-src-tab-acts-natively t)     ; 代码块中tab行为
  
  ;; 美化标题尺寸
  (set-face-attribute 'org-level-1 nil :height 1.3 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :height 1.1 :weight 'bold)
  (set-face-attribute 'org-level-4 nil :height 1.05 :weight 'bold)
  
  ;; 美化关键词
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(d)" "WAITING(w)" "|" "DONE(D)" "CANCELLED(c)")))
  
  ;; 美化TODO关键词颜色
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#ff6c6b" :weight bold))
          ("DOING" . (:foreground "#da8548" :weight bold))
          ("WAITING" . (:foreground "#ecbe7b" :weight bold))
          ("DONE" . (:foreground "#98be65" :weight bold))
          ("CANCELLED" . (:foreground "#5B6268" :weight bold)))))

;; =============================================================================
;; Org Mode 美化插件

;; 项目符号美化 - 简化的 org-bullets 配置
(use-package org-bullets
  :ensure t
  :after org
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  ;; 使用条件加载，避免函数不存在的错误
  (defun henri/enable-org-bullets ()
    "安全地启用 org-bullets。"
    (when (and (featurep 'org-bullets)
               (fboundp 'org-bullets-mode))
      (org-bullets-mode 1)))
  
  ;; 添加到钩子
  (add-hook 'org-mode-hook 'henri/enable-org-bullets))

;; 更现代的美化 - org-superstar (与 org-bullets 二选一)
(use-package org-superstar
  :ensure t
  :after org
  :config
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-item-bullet-alist
        '((?+ . ?➤)
          (?- . ?✦)
          (?* . ?◆)))
  ;; 美化标题符号
  (setq org-superstar-headline-bullets-list '("✿" "❀" "✸" "◆" "◇"))
  
  ;; 安全地启用 org-superstar
  (defun henri/enable-org-superstar ()
    "安全地启用 org-superstar。"
    (when (and (featurep 'org-superstar)
               (fboundp 'org-superstar-mode))
      (org-superstar-mode 1)))
  
  ;; 只使用一个美化插件（优先 org-superstar）
  (unless (featurep 'org-bullets)
    (add-hook 'org-mode-hook 'henri/enable-org-superstar)))

;; 美化优先级 - 手动配置（无需外部包）
(defun org-priority-show ()
  "Show priority of the current item."
  (interactive)
  (let ((priority (org-get-priority (buffer-substring-no-properties
                                     (point-at-bol) (point-at-eol)))))
    (if priority
        (message "Priority: %c" priority)
      (message "No priority set"))))

;; 手动美化优先级显示
(setq org-priority-faces
      '((?A . (:foreground "#ff6c6b" :weight bold))  ; 高优先级 - 红色
        (?B . (:foreground "#ecbe7b" :weight bold))  ; 中优先级 - 黄色
        (?C . (:foreground "#98be65" :weight bold)))) ; 低优先级 - 绿色

;; 美化标签
(setq org-tag-faces
      '(("WORK" . (:foreground "#da8548" :weight bold))
        ("HOME" . (:foreground "#98be65" :weight bold))
        ("URGENT" . (:foreground "#ff6c6b" :weight bold))
        ("SOMEDAY" . (:foreground "#5B6268" :weight bold))))

;; 代码块美化 - 直接配置，无需外部包
;; 自定义代码块美化面孔
(defface org-block-begin-line
  '((t (:foreground "#7c7c75" :background "#1e1e1e" :extend t)))
  "Face for org-mode block begin line.")

(defface org-block-end-line
  '((t (:foreground "#7c7c75" :background "#1e1e1e" :extend t)))
  "Face for org-mode block end line.")

;; 应用代码块美化
(with-eval-after-load 'org
  (set-face-attribute 'org-block nil :background "#1e1e1e" :foreground "#bbc2cf")
  (set-face-attribute 'org-block-begin-line nil :background "#2d2d2d" :foreground "#5B6268")
  (set-face-attribute 'org-block-end-line nil :background "#2d2d2d" :foreground "#5B6268"))

;; 图片下载和管理
(use-package org-download
  :ensure t
  :config
  ;; 设置拖放图片的默认目录
  (setq org-download-image-dir "./images")
  ;; 自动插入图片链接
  (add-hook 'dired-mode-hook 'org-download-enable))

;; =============================================================================
;; Org Mode 视觉增强配置

;; 美化表格
(setq org-table-header-line-p t)
(add-hook 'org-mode-hook 
          (lambda ()
            ;; 美化表格边框
            (set-face-attribute 'org-table nil :family "Monaco" :height 0.9)
            ;; 美化链接
            (set-face-attribute 'org-link nil :underline t :weight 'bold)
            ;; 美化代码
            (set-face-attribute 'org-code nil :background "#f0f0f0" :foreground "#d73502")
            (set-face-attribute 'org-verbatim nil :background "#f0f0f0" :foreground "#006400")))

;; 美化列表缩进
(setq org-list-indent-offset 2)
(setq org-adapt-indentation t)

;; 美化折叠显示
(setq org-cycle-separator-lines 1)
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

;; 启用美化的省略符号
(add-hook 'org-mode-hook 
          (lambda ()
            (push '("[ ]" . "☐") prettify-symbols-alist)
            (push '("[X]" . "☑") prettify-symbols-alist) 
            (push '("[-]" . "◐") prettify-symbols-alist)
            (prettify-symbols-mode 1)))

;; 美化日期时间戳
(setq org-time-stamp-custom-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))
(setq org-display-custom-times t)

;; 启用图片预览优化
(setq org-image-actual-width '(300))
(setq org-startup-with-inline-images t)

;; =============================================================================
;; 图标和字体美化

;; 确保 all-the-icons 已安装并配置
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :config
  ;; 检查字体是否已安装
  (unless (member "all-the-icons" (font-family-list))
    (message "Installing all-the-icons fonts...")
    (all-the-icons-install-fonts t)))

;; Org Mode 中的图标支持
(add-hook 'org-mode-hook
          (lambda ()
            (when (display-graphic-p)
              ;; 为不同的文件类型添加图标
              (setq org-agenda-category-icon-alist
                    `(("TODO" ,(list (all-the-icons-faicon "tasks")) nil nil :ascent center)
                      ("WORK" ,(list (all-the-icons-faicon "briefcase")) nil nil :ascent center)
                      ("HOME" ,(list (all-the-icons-faicon "home")) nil nil :ascent center)
                      ("URGENT" ,(list (all-the-icons-faicon "exclamation")) nil nil :ascent center))))))

;; =============================================================================
;; Doom 主题适配

;; 与 doom-themes 集成的 Org Mode 美化
(with-eval-after-load 'doom-themes
  ;; 启用 doom-themes 的 Org 配置（如果函数存在）
  (when (fboundp 'doom-themes-org-config)
    (doom-themes-org-config))
  
  ;; 自定义 Org 主题颜色（兼容性配置）
  ;; 使用标准的 custom-theme-set-faces 函数
  (custom-theme-set-faces
   'user
   ;; 标题层级颜色
   '(org-level-1 ((t (:foreground "#51afef" :weight bold :height 1.3))))
   '(org-level-2 ((t (:foreground "#c678dd" :weight bold :height 1.2))))
   '(org-level-3 ((t (:foreground "#98be65" :weight bold :height 1.1))))
   '(org-level-4 ((t (:foreground "#da8548" :weight bold :height 1.05))))
   '(org-level-5 ((t (:foreground "#5699af" :weight bold))))
   '(org-level-6 ((t (:foreground "#a9a1e1" :weight bold))))
   
   ;; 特殊块样式
   '(org-block ((t (:background "#1e1e1e" :foreground "#bbc2cf"))))
   '(org-block-begin-line ((t (:background "#2d2d2d" :foreground "#5B6268"))))
   '(org-block-end-line ((t (:background "#2d2d2d" :foreground "#5B6268"))))
   
   ;; 代码和引用
   '(org-code ((t (:background "#2d2d2d" :foreground "#98be65"))))
   '(org-quote ((t (:background "#1e1e1e" :foreground "#98be65" :slant italic))))
   
   ;; 表格美化
   '(org-table ((t (:foreground "#bbc2cf"))))
   
   ;; 链接美化
   '(org-link ((t (:foreground "#51afef" :underline t))))))

;; 备用美化方案（如果 doom-themes 不可用）
(unless (featurep 'doom-themes)
  (with-eval-after-load 'org
    ;; 手动设置 Org 面孔
    (set-face-attribute 'org-level-1 nil :foreground "#51afef" :weight 'bold :height 1.3)
    (set-face-attribute 'org-level-2 nil :foreground "#c678dd" :weight 'bold :height 1.2)
    (set-face-attribute 'org-level-3 nil :foreground "#98be65" :weight 'bold :height 1.1)
    (set-face-attribute 'org-level-4 nil :foreground "#da8548" :weight 'bold :height 1.05)
    (set-face-attribute 'org-level-5 nil :foreground "#5699af" :weight 'bold)
    (set-face-attribute 'org-level-6 nil :foreground "#a9a1e1" :weight 'bold)
    
    ;; 设置其他面孔
    (set-face-attribute 'org-block nil :background "#1e1e1e" :foreground "#bbc2cf")
    (set-face-attribute 'org-code nil :background "#2d2d2d" :foreground "#98be65")
    (set-face-attribute 'org-link nil :foreground "#51afef" :underline t)))

;; 强制重新应用 Org 主题
(add-hook 'org-mode-hook
          (lambda ()
            (when (boundp 'doom-theme)
              (run-with-timer 0.1 nil
                              (lambda ()
                                (font-lock-refresh-defaults)
                                (font-lock-fontify-buffer))))))

;; =============================================================================
;; 强制美化应用（确保在所有情况下都生效）

;; 确保在 Org Mode 启动时重新应用所有美化设置
(add-hook 'org-mode-hook
          (lambda ()
            ;; 延迟执行以确保所有包都已加载
            (run-with-timer 0.1 nil
                            (lambda ()
                              ;; 重新应用字体设置
                              (when (fboundp 'set-face-attribute)
                                (set-face-attribute 'org-level-1 nil :height 1.3 :weight 'bold)
                                (set-face-attribute 'org-level-2 nil :height 1.2 :weight 'bold)
                                (set-face-attribute 'org-level-3 nil :height 1.1 :weight 'bold))
                              
                              ;; 强制刷新字体锁定
                              (when (fboundp 'font-lock-fontify-buffer)
                                (font-lock-fontify-buffer))
                              
                              ;; 启用 prettify-symbols
                              (when (fboundp 'prettify-symbols-mode)
                                (prettify-symbols-mode 1))))))

;; 应急修复函数
(defun henri/emergency-org-fix ()
  "应急修复 Org Mode 美化显示。"
  (interactive)
  (when (eq major-mode 'org-mode)
    ;; 重新加载 org-base
    (load-file (expand-file-name "lisp/writing/org/org-base.el" user-emacs-directory))
    ;; 重启 org-mode
    (org-mode)
    ;; 刷新显示
    (font-lock-fontify-buffer)
    (message "🎨 Org Mode 美化已重新应用！")))

;; 重新加载主题函数
(defun henri/reload-org-theme ()
  "重新加载 Org Mode 主题。"
  (interactive)
  (when (eq major-mode 'org-mode)
    ;; 重新应用字体设置
    (set-face-attribute 'org-level-1 nil :height 1.3 :weight 'bold)
    (set-face-attribute 'org-level-2 nil :height 1.2 :weight 'bold) 
    (set-face-attribute 'org-level-3 nil :height 1.1 :weight 'bold)
    (set-face-attribute 'org-level-4 nil :height 1.05 :weight 'bold)
    ;; 刷新字体锁定
    (font-lock-fontify-buffer)
    (message "🎨 Org Mode 主题已重新加载！")))

;; 切换项目符号函数
(defun henri/toggle-org-bullets ()
  "安全地切换 Org Mode 项目符号美化。"
  (interactive)
  (cond
   ;; 如果 org-bullets 可用且已启用
   ((and (featurep 'org-bullets) 
         (fboundp 'org-bullets-mode)
         (bound-and-true-p org-bullets-mode))
    (org-bullets-mode -1)
    (message "🔸 Org bullets 已关闭"))
   
   ;; 如果 org-bullets 可用但未启用
   ((and (featurep 'org-bullets) 
         (fboundp 'org-bullets-mode))
    (org-bullets-mode 1)
    (message "🔹 Org bullets 已开启"))
   
   ;; 如果 org-superstar 可用且已启用
   ((and (featurep 'org-superstar)
         (fboundp 'org-superstar-mode)
         (bound-and-true-p org-superstar-mode))
    (org-superstar-mode -1)
    (message "🔸 Org superstar 已关闭"))
   
   ;; 如果 org-superstar 可用但未启用
   ((and (featurep 'org-superstar)
         (fboundp 'org-superstar-mode))
    (org-superstar-mode 1)
    (message "🔹 Org superstar 已开启"))
   
   ;; 如果都不可用
   (t
    (message "❌ 未找到可用的项目符号美化插件"))))

;; 切换折叠级别函数
(defun henri/cycle-org-startup-folded ()
  "循环切换 Org Mode 启动时的折叠级别。"
  (interactive)
  (let ((current org-startup-folded)
        (levels '(showeverything content overview)))
    (setq org-startup-folded 
          (or (cadr (member current levels))
              (car levels)))
    (message "📁 Org 启动折叠级别: %s" 
             (pcase org-startup-folded
               ('showeverything "展开所有内容")
               ('content "显示标题，折叠内容") 
               ('overview "只显示顶级标题")
               (_ "未知")))
    ;; 如果在 org-mode 缓冲区中，立即应用新设置
    (when (eq major-mode 'org-mode)
      (pcase org-startup-folded
        ('showeverything (org-show-all))
        ('content (org-content))
        ('overview (org-overview))))))

;; 手动折叠控制函数
(defun henri/org-show-all ()
  "展开当前 Org 文件的所有内容。"
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-show-all)
    (message "📖 已展开所有内容")))

(defun henri/org-overview ()
  "只显示当前 Org 文件的顶级标题。"
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-overview)
    (message "📋 只显示顶级标题")))

(defun henri/org-content ()
  "显示当前 Org 文件的所有标题但折叠内容。"
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-content)
    (message "📄 显示标题，折叠内容")))

;; 全局快捷键
(global-set-key (kbd "C-c o f") 'henri/emergency-org-fix)
(global-set-key (kbd "C-c o v") 'henri/cycle-org-startup-folded)

;; 快捷键绑定
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") 'henri/reload-org-theme)
  (define-key org-mode-map (kbd "C-c C-b") 'henri/toggle-org-bullets)
  (define-key org-mode-map (kbd "C-c v s") 'henri/org-show-all)
  (define-key org-mode-map (kbd "C-c v o") 'henri/org-overview)
  (define-key org-mode-map (kbd "C-c v c") 'henri/org-content))

(provide 'org-base)

;;; org-base.el ends here
