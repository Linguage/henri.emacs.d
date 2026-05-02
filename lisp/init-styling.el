;;; init-styling.el --- Emacs 界面外观定制 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: faces, frames, themes

;;; Commentary:

;; 本配置文件提供 Emacs 的界面美化和外观定制，包含以下主要模块：

;; 1. 基础界面设置
;;    - 窗口布局      -- 大小和位置
;;    - 界面元素      -- 菜单栏、工具栏等
;;    - 显示增强      -- 行号、列号等

;; 2. 性能优化
;;    - GC 优化       -- 垃圾回收阈值
;;    - 启动优化      -- 禁用不必要功能

;; 3. 主题美化
;;    - doom-themes   -- 现代化主题支持
;;    - doom-modeline -- 美化状态栏
;;    - 彩虹括号      -- 优化代码可读性

;; 4. 字体配置
;;    - 等宽编程字体   -- JetBrains Mono
;;    - 中文字体适配   -- 自动配置

;;; Code:

;; =============================================================================
;; 基础界面设置

;; 窗口布局配置
;; 初始 frame 尺寸在 early-init.el 中设置，避免启动后再跳动。
(add-to-list 'default-frame-alist '(top . 0.5))    ; 窗口垂直居中
(add-to-list 'default-frame-alist '(left . 0.5))   ; 窗口水平居中
(add-to-list 'default-frame-alist '(alpha . (95 . 95))) ; 设置透明度

;; 界面元素配置（菜单栏等在 early-init.el 的 GUI 分支已处理）
(global-display-line-numbers-mode)         ; 显示行号
(column-number-mode)                       ; 显示列号
(global-hl-line-mode t)                    ; 高亮当前行

;; 性能：GC 由 early-init.el 提高启动阈值，init.el 启动钩子恢复。
;; 此处不再重复设置 `gc-cons-threshold'。

(setq inhibit-startup-screen t)            ; 禁用启动屏幕
(setq auto-save-default nil)               ; 禁用自动保存
(setq backup-inhibited t)                  ; 禁用备份文件
(setq ring-bell-function 'ignore)          ; 禁用提示音

;; 编辑优化
(global-auto-revert-mode t)                ; 自动重载文件
(electric-pair-mode 1)                     ; 自动补全括号
(global-subword-mode 1)                    ; 驼峰语法支持

;; =============================================================================
;; 主题美化配置（`henri-theme-mode` 等见 init-custom.el）

(use-package doom-themes
  :ensure t
  :demand t
  :config
  (when (fboundp 'doom-themes-org-config)
    (doom-themes-org-config))
  (henri/apply-current-theme))

;; 彩虹括号（仅此一处；lisp 语言模块不再重复）
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 状态栏美化
;; 安装并配置 doom-modeline
(use-package nerd-icons
  :ensure t
  :demand t)

(use-package doom-modeline
  :ensure t
  :demand t
  :after nerd-icons
  :init
  (setq doom-modeline-height 28
        doom-modeline-bar-width 3
        doom-modeline-window-width-limit 85
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-buffer-encoding nil
        doom-modeline-indent-info nil
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-irc nil
        doom-modeline-lsp nil
        doom-modeline-env-version nil
        doom-modeline-check-simple-format t
        doom-modeline-vcs-max-length 12)
  (doom-modeline-mode 1)
  :config
  ;; 设置 doom-modeline 主题
  ; (setq doom-modeline-theme 'doom-modeline) ; 默认主题
  (setq doom-modeline-theme 'doom-modeline-light) ; 亮色主题
  ; (setq doom-modeline-theme 'doom-modeline-dark) ; 暗色主题
  )


;; =============================================================================
;; 字体配置
;; windows系统需要安装NerdFontsSymbolsOnly字体，否则会出现图标显示异常


(use-package all-the-icons
  :ensure t
  :config
  (unless (member "all-the-icons" (font-family-list))
    (when (display-graphic-p)
      (message "[henri] all-the-icons fonts not found; run M-x all-the-icons-install-fonts if icons look wrong."))))


;; 字体与缩放：参见 `lib-fonts'（`henri/set-font`、`henri/font-size-adjust'）。
;; 应用字体设置
(when window-system
  (henri/set-font))

;; 字体缩放（运行时；前缀 N 如 C-u N C-= 一次调整 N 档）
(global-set-key (kbd "C-=") #'henri/font-size-adjust)
(global-set-key (kbd "C--")
                (lambda (n) (interactive "p")
                  (henri/font-size-adjust (- (or n 1)))))
(global-set-key (kbd "C-c F r") #'henri/font-size-reset)
(global-set-key (kbd "C-c F b") #'henri-big-font-mode)

;; =============================================================================
;; 标签页 (Centaur Tabs) — 辅助函数与 `use-package' 同块，避免声明耦合

(when (and (boundp 'henri-enable-centaur-tabs) henri-enable-centaur-tabs)
  (use-package centaur-tabs
    :ensure t
    :defer t
    :if (display-graphic-p)
    :preface
    (defun henri/setup-centaur-tabs ()
      "安全地设置和启动 centaur-tabs。"
      (interactive)
      (run-with-idle-timer
       2 nil
       (lambda ()
         (when (and (boundp 'henri-enable-centaur-tabs)
                    henri-enable-centaur-tabs
                    (display-graphic-p)
                    (not (bound-and-true-p centaur-tabs-mode)))
           (require 'centaur-tabs nil t)
           (centaur-tabs-mode 1)
           (message "Centaur-tabs 已启用")
           (henri/setup-tabs-mouse-support)
           (henri/maybe-enable-tabs-icons)))))

    (defun henri/setup-tabs-mouse-support ()
      "设置标签页的鼠标滚轮切换。"
      (global-set-key [mouse-4] 'centaur-tabs-backward)
      (global-set-key [mouse-5] 'centaur-tabs-forward))

    (defun henri/maybe-enable-tabs-icons ()
      "在图标字体可用时启用 centaur-tabs 图标。"
      (when (and (featurep 'all-the-icons)
                 (display-graphic-p))
        (condition-case err
            (when (and (> (length (font-family-list)) 0)
                       (or (member "all-the-icons" (font-family-list))
                           (member "All The Icons" (font-family-list))))
              (setq centaur-tabs-set-icons t)
              (when (bound-and-true-p centaur-tabs-mode)
                (centaur-tabs-mode -1)
                (centaur-tabs-mode 1))
              (message "Centaur-tabs 图标已启用"))
          (error
           (message "图标启用失败: %s" err)))))

    (defun henri/centaur-tabs-theme ()
      "使 centaur-tabs 与当前 doom 主题对齐。"
      (when (and (display-graphic-p)
                 (bound-and-true-p centaur-tabs-mode))
        (condition-case nil
            (centaur-tabs-headline-match)
          (error nil))))
    :bind (("C-<prior>" . centaur-tabs-backward)
           ("C-<next>" . centaur-tabs-forward))
    :init
    (when (display-graphic-p)
      (add-hook 'henri-first-buffer-hook #'henri/setup-centaur-tabs))
    :config
    (setq centaur-tabs-style "bar"
          centaur-tabs-height 32
          centaur-tabs-set-icons nil
          centaur-tabs-show-new-tab-button t
          centaur-tabs-set-close-button t
          centaur-tabs-close-button "×"
          centaur-tabs-new-tab-button "+"
          centaur-tabs-set-modified-marker t
          centaur-tabs-modified-marker "●"
          centaur-tabs-adjust-buffer-order t
          centaur-tabs-enable-buffer-alphabetical-reordering t
          centaur-tabs-enable-ido-completion nil)
    (defun centaur-tabs-buffer-groups ()
      "自定义缓冲区分组规则。"
      (list
       (cond
        ((derived-mode-p 'org-mode) "Org")
        ((or (derived-mode-p 'prog-mode)
             (derived-mode-p 'python-mode)
             (derived-mode-p 'emacs-lisp-mode)
             (derived-mode-p 'js-mode)
             (derived-mode-p 'c-mode)
             (derived-mode-p 'java-mode))
         "Programming")
        ((or (derived-mode-p 'text-mode)
             (derived-mode-p 'markdown-mode)
             (derived-mode-p 'latex-mode))
         "Text")
        ((derived-mode-p 'dired-mode) "Dired")
        ((string-equal "*" (substring (buffer-name) 0 1)) "System")
        (t "General"))))
    (defun centaur-tabs-hide-tab (x)
      "隐藏不需要显示标签页的缓冲区（共用 `henri-buffer-real-p'）。"
      (not (henri-buffer-real-p x)))
    (add-hook 'doom-themes-after-load-theme-hook #'henri/centaur-tabs-theme)))

(provide 'init-styling)

;;; init-styling.el ends here
