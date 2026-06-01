;;; org-base.el --- Org Mode 基础配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 3.0
;; Keywords: org, base, theme, beautify

;;; Commentary:

;; Org Mode 基础配置，包含：
;; - 基础设置
;; - 美化插件（org-bullets）
;; - 字体和颜色配置（统一由 henri/apply-org-faces 管理）
;; - 基本功能配置

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'visual-fonts)

;; Org CJK 字体候选、`henri/org-setup-body-font'、诊断等见 `visual-fonts'.

;; =============================================================================
;; Org Mode 基础配置

(use-package org
  :ensure t
  :mode (("\\.org$" . org-mode))
  :init
  (setq org-startup-indented t)
  (setq org-startup-with-inline-images t)
  (setq org-startup-folded 'showeverything)
  (setq org-hide-emphasis-markers t)
  (setq org-pretty-entities t)
  (setq org-fontify-quote-and-verse-blocks t)
  :config
  (setq org-log-done 'time)
  (setq org-ellipsis " ▾")
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

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

;; 项目符号美化
(defun henri/enable-org-bullets ()
  "安全地启用 org-bullets。"
  (when (and (featurep 'org-bullets)
             (fboundp 'org-bullets-mode))
    (org-bullets-mode 1)))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . henri/enable-org-bullets)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; 美化优先级
(defun henri/org-priority-show ()
  "Show priority of the current item."
  (interactive)
  (let ((priority (org-get-priority (buffer-substring-no-properties
                                     (line-beginning-position) (line-end-position)))))
    (if priority
        (message "Priority: %c" priority)
      (message "No priority set"))))

(setq org-priority-faces
      '((?A . (:foreground "#ff6c6b" :weight bold))
        (?B . (:foreground "#ecbe7b" :weight bold))
        (?C . (:foreground "#98be65" :weight bold))))

;; 美化标签
(setq org-tag-faces
      '(("WORK" . (:foreground "#da8548" :weight bold))
        ("HOME" . (:foreground "#98be65" :weight bold))
        ("URGENT" . (:foreground "#ff6c6b" :weight bold))
        ("SOMEDAY" . (:foreground "#5B6268" :weight bold))))

;; 图片下载和管理
(use-package org-download
  :ensure t
  :config
  (setq org-download-image-dir "./images")
  (add-hook 'dired-mode-hook 'org-download-enable))

;; =============================================================================
;; Org Face 统一配置（唯一入口）

(defun henri/apply-org-faces ()
  "Apply Henri's preferred Org faces.
Called once after Org loads, and again after every theme change via
`load-theme' advice.  All face customisation should live here;
do NOT scatter set-face-attribute calls elsewhere."
  (custom-theme-set-faces
   'user
   '(org-document-title ((t (:foreground "#2f3542" :weight bold :height 1.45))))
   '(org-level-1 ((t (:foreground "#2563eb" :weight bold :height 1.35))))
   '(org-level-2 ((t (:foreground "#7c3aed" :weight bold :height 1.24))))
   '(org-level-3 ((t (:foreground "#15803d" :weight bold :height 1.15))))
   '(org-level-4 ((t (:foreground "#c2410c" :weight bold :height 1.08))))
   '(org-level-5 ((t (:foreground "#0f766e" :weight bold))))
   '(org-level-6 ((t (:foreground "#9333ea" :weight bold))))
   '(org-block ((t (:inherit fixed-pitch :background "#f6f8fa" :foreground "#24292f" :extend t))))
   '(org-block-begin-line ((t (:background "#eef2f7" :foreground "#64748b" :extend t))))
   '(org-block-end-line ((t (:background "#eef2f7" :foreground "#64748b" :extend t))))
   '(org-code ((t (:inherit fixed-pitch :background "#eef2f7" :foreground "#b42318"))))
   '(org-verbatim ((t (:inherit fixed-pitch :background "#eef2f7" :foreground "#166534"))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#475569"))))
   '(org-link ((t (:foreground "#2563eb" :underline t :weight bold))))
   '(org-quote ((t (:slant italic :foreground "#475569" :background "#f8fafc" :extend t))))))

;; 初始加载 + 每次切换主题后重新应用
(with-eval-after-load 'org
  (henri/apply-org-faces))

(advice-add 'load-theme
            :after
            (lambda (&rest _)
              (when (featurep 'org)
                (henri/apply-org-faces))))

;; =============================================================================
;; 视觉增强

(setq org-table-header-line-p t)
(setq org-list-indent-offset 2)
(setq org-adapt-indentation t)
(setq org-cycle-separator-lines 1)
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

;; 美化 checkbox 符号
(with-eval-after-load 'org
  (add-hook 'org-mode-hook
            (lambda ()
              (dolist (pair '(("[ ]" . "☐") ("[X]" . "☑") ("[-]" . "◐")))
                (add-to-list 'prettify-symbols-alist pair))
              (prettify-symbols-mode 1))))

;; 注：`org-display-custom-times' 默认关闭，避免显示值与编辑值不一致；
;; 如需带星期的时间戳，可 M-x customize-set-variable RET org-display-custom-times.
(setq org-time-stamp-custom-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))

(setq org-image-actual-width '(300))

;; =============================================================================
;; Org 字数统计

(defcustom henri-org-word-count-enable-mode-line t
  "Non-nil means show a lightweight Org word count in the mode line."
  :type 'boolean
  :group 'henri-writing)

(defcustom henri-org-word-count-idle-delay 1.5
  "Idle seconds before refreshing Org word count after edits."
  :type 'number
  :group 'henri-writing)

(defcustom henri-org-word-count-max-buffer-size 500000
  "Maximum Org buffer size to count automatically for the mode line.
Buffers larger than this still support manual `henri/org-word-count', but the
mode-line updater will skip them to avoid UI stalls."
  :type 'integer
  :group 'henri-writing)

(defvar-local henri-org-word-count--total nil
  "Cached Org word count for the current buffer.")

(defvar-local henri-org-word-count--timer nil
  "Idle timer used to refresh Org word count.")

(defun henri-org-word-count--cjk-char-p (char)
  "Return non-nil when CHAR is in a common CJK unified ideograph range."
  (or (and (>= char #x4e00) (<= char #x9fff))
      (and (>= char #x3400) (<= char #x4dbf))
      (and (>= char #x20000) (<= char #x2a6df))))

(defun henri-org-word-count--count (&optional respect-size-limit)
  "Return (TOTAL CJK ENGLISH) for the current Org buffer.
When RESPECT-SIZE-LIMIT is non-nil, return nil for very large buffers."
  (when (or (not respect-size-limit)
            (<= (buffer-size) henri-org-word-count-max-buffer-size))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((cjk 0)
              (english 0))
          (while (not (eobp))
            (cond
             ((henri-org-word-count--cjk-char-p (following-char))
              (cl-incf cjk)
              (forward-char 1))
             ((looking-at "[[:alnum:]_$]+")
              (cl-incf english)
              (goto-char (match-end 0)))
             (t
              (forward-char 1))))
          (list (+ cjk english) cjk english))))))

(defun henri-org-word-count--mode-line ()
  "Return the Org word-count lighter for the current buffer."
  (cond
   (henri-org-word-count--total
    (propertize
     (format " 字数:%d" henri-org-word-count--total)
     'help-echo "Org 字数统计：中文字符 + 英文词数"))
   ((> (buffer-size) henri-org-word-count-max-buffer-size)
    (propertize " 字数:大文件" 'help-echo "Buffer 太大，已跳过自动字数统计"))
   (t " 字数:...")))

(defun henri-org-word-count--refresh (&optional respect-size-limit)
  "Refresh cached Org word count.
When RESPECT-SIZE-LIMIT is non-nil, skip very large buffers."
  (let ((counts (henri-org-word-count--count respect-size-limit)))
    (setq henri-org-word-count--total (car counts))
    (force-mode-line-update)))

(defun henri-org-word-count--schedule (&rest _)
  "Schedule a delayed Org word-count refresh for the current buffer."
  (when (and henri-org-word-count-mode
             (derived-mode-p 'org-mode))
    (when (timerp henri-org-word-count--timer)
      (cancel-timer henri-org-word-count--timer))
    (setq henri-org-word-count--timer
          (run-with-idle-timer
           henri-org-word-count-idle-delay nil
           (lambda (buffer)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when henri-org-word-count-mode
                   (henri-org-word-count--refresh t)))))
           (current-buffer)))))

;;;###autoload
(defun henri/org-word-count ()
  "Count words in the current Org buffer and show details in the minibuffer.
Chinese characters count individually; English and numeric runs count as words."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "当前不是 org-mode"))
  (pcase-let ((`(,total ,cjk ,english) (henri-org-word-count--count nil)))
    (setq henri-org-word-count--total total)
    (force-mode-line-update)
    (message "Org 字数: %d（中文字符: %d，英文/数字词: %d）"
             total cjk english)))

(define-minor-mode henri-org-word-count-mode
  "Show a lightweight Org word count in the mode line."
  :lighter (:eval (henri-org-word-count--mode-line))
  (if henri-org-word-count-mode
      (progn
        (add-hook 'after-change-functions
                  #'henri-org-word-count--schedule nil t)
        (henri-org-word-count--refresh t))
    (remove-hook 'after-change-functions
                 #'henri-org-word-count--schedule t)
    (when (timerp henri-org-word-count--timer)
      (cancel-timer henri-org-word-count--timer))
    (setq henri-org-word-count--timer nil
          henri-org-word-count--total nil)))

(defun henri-org-word-count-enable-mode-line ()
  "Enable Org word-count mode-line display when configured."
  (when henri-org-word-count-enable-mode-line
    (henri-org-word-count-mode 1)))

(add-hook 'org-mode-hook #'henri-org-word-count-enable-mode-line)

;; =============================================================================
;; 图标支持

(require 'all-the-icons nil t)

(add-hook 'org-mode-hook
          (lambda ()
            (when (and (display-graphic-p) (featurep 'all-the-icons))
              (setq org-agenda-category-icon-alist
                    `(("TODO" ,(list (all-the-icons-faicon "tasks")) nil nil :ascent center)
                      ("WORK" ,(list (all-the-icons-faicon "briefcase")) nil nil :ascent center)
                      ("HOME" ,(list (all-the-icons-faicon "home")) nil nil :ascent center)
                      ("URGENT" ,(list (all-the-icons-faicon "exclamation")) nil nil :ascent center))))))

;; =============================================================================
;; 交互函数

(defun henri/toggle-org-bullets ()
  "切换 Org Mode 项目符号美化。"
  (interactive)
  (cond
   ((and (featurep 'org-bullets)
         (fboundp 'org-bullets-mode)
         (bound-and-true-p org-bullets-mode))
    (org-bullets-mode -1)
    (message "Org bullets 已关闭"))
   ((and (featurep 'org-bullets)
         (fboundp 'org-bullets-mode))
    (org-bullets-mode 1)
    (message "Org bullets 已开启"))
   (t
    (message "未找到可用的项目符号美化插件"))))

(defun henri/cycle-org-startup-folded ()
  "循环切换 Org Mode 启动时的折叠级别。"
  (interactive)
  (let ((current org-startup-folded)
        (levels '(showeverything content overview)))
    (setq org-startup-folded
          (or (cadr (member current levels))
              (car levels)))
    (message "Org 启动折叠级别: %s"
             (pcase org-startup-folded
               ('showeverything "展开所有内容")
               ('content "显示标题，折叠内容")
               ('overview "只显示顶级标题")
               (_ "未知")))
    (when (eq major-mode 'org-mode)
      (pcase org-startup-folded
        ('showeverything (henri/org-show-all))
        ('content (henri/org-content))
        ('overview (henri/org-overview))))))

(defun henri/org-show-all ()
  "展开当前 Org 文件的所有内容。
优先使用 Org 9.6+ 的 `org-fold-show-all'，回退到旧的 `org-show-all'。"
  (interactive)
  (when (eq major-mode 'org-mode)
    (cond ((fboundp 'org-fold-show-all) (org-fold-show-all))
          ((fboundp 'org-show-all) (org-show-all)))))

(defun henri/org-overview ()
  "只显示当前 Org 文件的顶级标题。"
  (interactive)
  (when (eq major-mode 'org-mode)
    (cond ((fboundp 'org-cycle-overview) (org-cycle-overview))
          ((fboundp 'org-overview) (org-overview)))))

(defun henri/org-content ()
  "显示当前 Org 文件的所有标题但折叠内容。"
  (interactive)
  (when (eq major-mode 'org-mode)
    (cond ((fboundp 'org-cycle-content) (org-cycle-content))
          ((fboundp 'org-content) (org-content)))))

(defun henri/org-insert-checkbox ()
  "Insert a new Org checkbox list item.
When point is already in a plain list, keep the current list level.
Otherwise start a new `- [ ]' item at point or on the next line."
  (interactive)
  (unless (org-insert-item t)
    (unless (looking-at-p "[ \t]*$")
      (end-of-line)
      (insert "\n"))
    (delete-horizontal-space)
    (insert "- [ ] ")))

;; =============================================================================
;; 快捷键

(global-set-key (kbd "C-c o v") 'henri/cycle-org-startup-folded)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-S-<return>") 'org-insert-todo-heading)
  (define-key org-mode-map (kbd "C-c m x") 'henri/org-insert-checkbox)
  (define-key org-mode-map (kbd "C-c m w") 'henri/org-word-count)
  (define-key org-mode-map (kbd "C-c m v b") 'henri/toggle-org-bullets)
  (define-key org-mode-map (kbd "C-c m v s") 'henri/org-show-all)
  (define-key org-mode-map (kbd "C-c m v o") 'henri/org-overview)
  (define-key org-mode-map (kbd "C-c m v c") 'henri/org-content))

(provide 'org-base)

;;; org-base.el ends here
