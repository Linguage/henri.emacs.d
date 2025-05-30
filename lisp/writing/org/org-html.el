;;; org-html.el --- Org Mode HTML 导出配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Keywords: org, html, export, themes

;;; Commentary:

;; Org Mode HTML 导出配置，包含：
;; - org-html-themes 集成
;; - 默认主题设置（ReadTheOrg）
;; - 可选主题切换功能
;; - HTML 导出优化
;; - 自定义样式支持

;;; Code:

;; =============================================================================
;; HTML 导出基础配置

(require 'ox-html)

;; 设置默认的 HTML 导出选项
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)
(setq org-html-use-infojs nil)

;; 启用代码高亮
(setq org-html-htmlize-output-type 'css)
(setq org-html-htmlize-font-prefix "org-")

;; 设置表格样式
(setq org-html-table-default-attributes 
      '(:border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides"))

;; =============================================================================
;; org-html-themes 配置

;; 主题配置变量
;; 获取本地主题目录路径
(defvar my/org-html-themes-dir 
  (file-name-as-directory 
   (expand-file-name "org-html-themes" 
                     (file-name-directory (or load-file-name buffer-file-name))))
  "本地 org-html-themes 目录路径。")

(defvar my/org-html-themes-list
  `(("ReadTheOrg" . ,(expand-file-name "org/theme-readtheorg.setup" my/org-html-themes-dir))
    ("Bigblow" . ,(expand-file-name "org/theme-bigblow.setup" my/org-html-themes-dir))
    ("ReadTheOrg (在线)" . "https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup")
    ("Bigblow (在线)" . "https://fniessen.github.io/org-html-themes/org/theme-bigblow.setup"))
  "可用的 org-html-themes 主题列表。
每个元素是一个 cons cell，格式为 (主题名称 . setup文件路径)。
默认使用本地主题，如果本地主题不存在，可以使用在线版本。")

(defvar my/org-html-default-theme "ReadTheOrg"
  "默认使用的 HTML 主题名称。")

;; =============================================================================
;; 简化主题映射系统

;; 主题编号映射表
(defvar my/org-html-theme-shortcuts
  '(("default" . "ReadTheOrg")
    ("1" . "ReadTheOrg")
    ("rto" . "ReadTheOrg")
    ("readtheorg" . "ReadTheOrg")
    ("2" . "Bigblow") 
    ("bb" . "Bigblow")
    ("bigblow" . "Bigblow"))
  "主题快捷方式映射表。
支持数字编号、缩写和全名调用主题。")

(defun my/org-html-apply-theme-by-shortcut (shortcut)
  "使用快捷方式应用主题。
SHORTCUT 可以是数字编号、缩写或主题名称。
例如：'1', 'rto', 'readtheorg', 'default' 等。"
  (interactive "sHTML主题 (1=ReadTheOrg, 2=Bigblow, default/rto/bb): ")
  (let* ((normalized-shortcut (downcase (string-trim shortcut)))
         (theme-name (cdr (assoc normalized-shortcut my/org-html-theme-shortcuts))))
    (if theme-name
        (progn
          (my/org-html-set-theme theme-name)
          (message "已应用主题: %s (快捷方式: %s)" theme-name shortcut))
      (message "未知的主题快捷方式: %s" shortcut))))

(defun my/org-html-show-theme-shortcuts ()
  "显示所有可用的主题快捷方式。"
  (interactive)
  (message "HTML主题快捷方式:\n1/rto/readtheorg/default -> ReadTheOrg\n2/bb/bigblow -> Bigblow"))

;; 更简单的主题应用函数
(defun my/org-html-theme-1 ()
  "应用主题1: ReadTheOrg"
  (interactive)
  (my/org-html-set-theme "ReadTheOrg")
  (message "已应用主题1: ReadTheOrg"))

(defun my/org-html-theme-2 ()
  "应用主题2: Bigblow"
  (interactive)
  (my/org-html-set-theme "Bigblow")
  (message "已应用主题2: Bigblow"))

(defun my/org-html-theme-default ()
  "应用默认主题"
  (interactive)
  (my/org-html-apply-default-theme)
  (message "已应用默认主题: %s" my/org-html-default-theme))

;; =============================================================================
;; 主题管理函数

(defun my/org-html-get-theme-setup-file (theme-name)
  "获取指定主题的 setup 文件路径。
THEME-NAME 是主题名称。如果本地文件不存在，返回在线版本。"
  (let ((local-file (cdr (assoc theme-name my/org-html-themes-list))))
    (if (and local-file 
             (not (string-prefix-p "https://" local-file))
             (file-exists-p local-file))
        local-file
      ;; 如果本地文件不存在，尝试使用在线版本
      (cdr (assoc (concat theme-name " (在线)") my/org-html-themes-list)))))

(defun my/org-html-check-local-themes ()
  "检查本地主题文件是否存在。"
  (interactive)
  (let ((readtheorg-local (expand-file-name "org/theme-readtheorg.setup" my/org-html-themes-dir))
        (bigblow-local (expand-file-name "org/theme-bigblow.setup" my/org-html-themes-dir)))
    (if (and (file-exists-p readtheorg-local)
             (file-exists-p bigblow-local))
        (progn
          (message "✅ 本地主题文件已就绪!")
          (message "ReadTheOrg: %s" readtheorg-local)
          (message "Bigblow: %s" bigblow-local)
          t)
      (progn
        (message "❌ 本地主题文件缺失，请运行安装脚本: ./install-themes.sh")
        (message "主题目录: %s" my/org-html-themes-dir)
        nil))))

(defun my/org-html-set-theme (theme-name)
  "为当前 Org 文件设置 HTML 主题。
THEME-NAME 是要设置的主题名称。"
  (interactive 
   (list (completing-read "选择 HTML 主题: " 
                          (mapcar #'car my/org-html-themes-list)
                          nil t)))
  (let ((setup-file (my/org-html-get-theme-setup-file theme-name)))
    (if setup-file
        (progn
          (save-excursion
            (goto-char (point-min))
            ;; 删除现有的 SETUPFILE 行
            (while (re-search-forward "^#\\+SETUPFILE:.*org-html-themes.*$" nil t)
              (delete-region (line-beginning-position) (1+ (line-end-position))))
            ;; 在文件开头插入新的 SETUPFILE
            (goto-char (point-min))
            (if (looking-at "^#\\+TITLE:")
                (forward-line 1)
              (goto-char (point-min)))
            (insert (format "#+SETUPFILE: %s\n" setup-file)))
          (message "已设置 HTML 主题为: %s" theme-name))
      (message "未找到主题: %s" theme-name))))

(defun my/org-html-apply-default-theme ()
  "为当前 Org 文件应用默认的 HTML 主题。"
  (interactive)
  (my/org-html-set-theme my/org-html-default-theme))

(defun my/org-html-remove-theme ()
  "移除当前 Org 文件的 HTML 主题设置。"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#\\+SETUPFILE:.*org-html-themes.*$" nil t)
      (delete-region (line-beginning-position) (1+ (line-end-position))))
    (message "已移除 HTML 主题设置")))

;; =============================================================================
;; 导出增强功能

(defun my/org-html-export-and-open ()
  "导出当前 Org 文件为 HTML 并在浏览器中打开。"
  (interactive)
  (let ((html-file (org-html-export-to-html)))
    (when html-file
      (browse-url (concat "file://" (expand-file-name html-file)))
      (message "HTML 文件已导出并在浏览器中打开: %s" html-file))))

(defun my/org-html-export-with-theme (theme-name)
  "使用指定主题导出当前 Org 文件为 HTML。
THEME-NAME 是要使用的主题名称。"
  (interactive 
   (list (completing-read "选择导出主题: " 
                          (mapcar #'car my/org-html-themes-list)
                          nil t nil nil my/org-html-default-theme)))
  (let ((original-content (buffer-string))
        (setup-file (my/org-html-get-theme-setup-file theme-name)))
    (if setup-file
        (save-excursion
          ;; 临时添加主题设置
          (goto-char (point-min))
          (if (looking-at "^#\\+TITLE:")
              (forward-line 1)
            (goto-char (point-min)))
          (insert (format "#+SETUPFILE: %s\n" setup-file))
          ;; 导出
          (let ((html-file (org-html-export-to-html)))
            ;; 恢复原始内容
            (erase-buffer)
            (insert original-content)
            ;; 打开导出的文件
            (when html-file
              (browse-url (concat "file://" (expand-file-name html-file)))
              (message "使用 %s 主题导出完成: %s" theme-name html-file))))
      (message "未找到主题: %s" theme-name))))

;; =============================================================================
;; 自动主题应用

(defun my/org-html-auto-apply-theme ()
  "自动为新的 Org 文件应用默认主题。"
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name)
             (not (string-match "SETUPFILE:.*org-html-themes" (buffer-string))))
    (save-excursion
      (goto-char (point-min))
      (when (or (looking-at "^#\\+TITLE:")
                (re-search-forward "^#\\+TITLE:" nil t))
        (end-of-line)
        (newline)
        (insert (format "#+SETUPFILE: %s" 
                        (my/org-html-get-theme-setup-file my/org-html-default-theme)))))))

;; 可以选择性启用自动应用主题
;; (add-hook 'org-mode-hook 'my/org-html-auto-apply-theme)

;; =============================================================================
;; 主题下载和本地化

(defun my/org-html-download-themes ()
  "下载 org-html-themes 到本地 org 目录。"
  (interactive)
  (if (file-exists-p my/org-html-themes-dir)
      (message "主题目录已存在: %s" my/org-html-themes-dir)
    (let ((install-script (expand-file-name "install-themes.sh" 
                                           (file-name-directory my/org-html-themes-dir))))
      (if (file-exists-p install-script)
          (progn
            (message "运行安装脚本...")
            (shell-command (format "cd %s && bash install-themes.sh" 
                                 (file-name-directory install-script))))
        (progn
          (message "正在下载 org-html-themes...")
          (shell-command 
           (format "cd %s && git clone https://github.com/fniessen/org-html-themes.git" 
                   (file-name-directory my/org-html-themes-dir)))
          (if (file-exists-p my/org-html-themes-dir)
              (message "org-html-themes 下载完成: %s" my/org-html-themes-dir)
            (message "下载失败，请手动克隆仓库")))))))

(defun my/org-html-install-themes ()
  "安装主题的便捷函数。"
  (interactive)
  (my/org-html-download-themes)
  (my/org-html-check-local-themes))

(defun my/org-html-use-local-themes ()
  "强制使用本地的 org-html-themes。"
  (interactive)
  (if (my/org-html-check-local-themes)
      (progn
        (setq my/org-html-themes-list
              `(("ReadTheOrg" . ,(expand-file-name "org/theme-readtheorg.setup" my/org-html-themes-dir))
                ("Bigblow" . ,(expand-file-name "org/theme-bigblow.setup" my/org-html-themes-dir))))
        (message "已切换到本地主题"))
    (message "本地主题不可用，请先运行 my/org-html-install-themes")))

(defun my/org-html-use-online-themes ()
  "切换到使用在线的 org-html-themes。"
  (interactive)
  (setq my/org-html-themes-list
        '(("ReadTheOrg" . "https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup")
          ("Bigblow" . "https://fniessen.github.io/org-html-themes/org/theme-bigblow.setup")))
  (message "已切换到在线主题"))

;; =============================================================================
;; 自定义样式支持

(defun my/org-html-add-custom-css ()
  "为当前 Org 文件添加自定义 CSS 样式。"
  (interactive)
  (let ((css-code (read-string "输入 CSS 代码: ")))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+HTML_HEAD:" nil t)
          (progn
            (end-of-line)
            (newline))
        (goto-char (point-min))
        (if (looking-at "^#\\+TITLE:")
            (progn
              (forward-line 1)
              (while (looking-at "^#\\+")
                (forward-line 1)))
          (goto-char (point-min))))
      (insert (format "#+HTML_HEAD: <style>%s</style>\n" css-code))
      (message "已添加自定义 CSS 样式"))))

;; =============================================================================
;; 快速预设样式

(defun my/org-html-add-dark-code-style ()
  "为代码块添加深色背景样式。"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\+HTML_HEAD:" nil t)
        (progn
          (end-of-line)
          (newline))
      (goto-char (point-min))
      (if (looking-at "^#\\+TITLE:")
          (progn
            (forward-line 1)
            (while (looking-at "^#\\+")
              (forward-line 1)))
        (goto-char (point-min))))
    (insert "#+HTML_HEAD: <style>pre.src{background:#343131;color:white;}</style>\n")
    (message "已添加深色代码块样式")))

(defun my/org-html-expand-content-width ()
  "扩展 ReadTheOrg 主题的内容宽度限制。"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\+HTML_HEAD:" nil t)
        (progn
          (end-of-line)
          (newline))
      (goto-char (point-min))
      (if (looking-at "^#\\+TITLE:")
          (progn
            (forward-line 1)
            (while (looking-at "^#\\+")
              (forward-line 1)))
        (goto-char (point-min))))
    (insert "#+HTML_HEAD: <style>#content{max-width:1800px;} p{max-width:800px;} li{max-width:800px;}</style>\n")
    (message "已扩展内容宽度限制")))

;; =============================================================================
;; 快捷键设置

(with-eval-after-load 'org
  ;; 原有的快捷键
  (define-key org-mode-map (kbd "C-c h t") 'my/org-html-set-theme)
  (define-key org-mode-map (kbd "C-c h d") 'my/org-html-apply-default-theme)
  (define-key org-mode-map (kbd "C-c h e") 'my/org-html-export-and-open)
  (define-key org-mode-map (kbd "C-c h w") 'my/org-html-export-with-theme)
  (define-key org-mode-map (kbd "C-c h c") 'my/org-html-add-custom-css)
  (define-key org-mode-map (kbd "C-c h r") 'my/org-html-remove-theme)
  (define-key org-mode-map (kbd "C-c h i") 'my/org-html-install-themes)
  (define-key org-mode-map (kbd "C-c h k") 'my/org-html-check-local-themes)
  
  ;; 新增的简化快捷键
  (define-key org-mode-map (kbd "C-c h s") 'my/org-html-apply-theme-by-shortcut)  ; 快捷方式
  (define-key org-mode-map (kbd "C-c h ?") 'my/org-html-show-theme-shortcuts)     ; 显示帮助
  (define-key org-mode-map (kbd "C-c h 1") 'my/org-html-theme-1)                 ; 主题1
  (define-key org-mode-map (kbd "C-c h 2") 'my/org-html-theme-2)                 ; 主题2
  (define-key org-mode-map (kbd "C-c h 0") 'my/org-html-theme-default))          ; 默认主题

;; =============================================================================
;; 导出钩子

(defun my/org-html-export-hook (backend)
  "HTML 导出时的钩子函数。
BACKEND 是导出后端，由钩子提供。"
  ;; 可以在这里添加导出时的自动处理逻辑
  ;; 例如：只在 HTML 导出时执行某些操作
  (when (eq backend 'html)
    ;; 这里可以添加 HTML 导出特定的处理逻辑
    ))

(add-hook 'org-export-before-processing-hook 'my/org-html-export-hook)

(provide 'org-html)

;;; org-html.el ends here

;; =============================================================================
;; 全局主题应用（可在任何地方使用）

;;;###autoload
(defun org-html-theme (shortcut)
  "全局HTML主题应用命令。
可以在任何地方使用 M-x org-html-theme 调用。
SHORTCUT 支持以下格式：
- 数字: 1 (ReadTheOrg), 2 (Bigblow)
- 缩写: rto, bb, default
- 全名: readtheorg, bigblow"
  (interactive "sHTML主题 (1=ReadTheOrg, 2=Bigblow, rto/bb/default): ")
  (if (eq major-mode 'org-mode)
      (my/org-html-apply-theme-by-shortcut shortcut)
    (message "请在 Org Mode 文件中使用此命令")))

;;;###autoload 
(defun org-html-theme-1 ()
  "全局应用HTML主题1: ReadTheOrg"
  (interactive)
  (if (eq major-mode 'org-mode)
      (my/org-html-theme-1)
    (message "请在 Org Mode 文件中使用此命令")))

;;;###autoload
(defun org-html-theme-2 ()
  "全局应用HTML主题2: Bigblow"
  (interactive)
  (if (eq major-mode 'org-mode)
      (my/org-html-theme-2) 
    (message "请在 Org Mode 文件中使用此命令")))

;;;###autoload
(defun org-html-theme-default ()
  "全局应用默认HTML主题"
  (interactive)
  (if (eq major-mode 'org-mode)
      (my/org-html-theme-default)
    (message "请在 Org Mode 文件中使用此命令")))

;;;###autoload
(defun org-html-export-quick ()
  "快速HTML导出并打开浏览器"
  (interactive)
  (if (eq major-mode 'org-mode)
      (my/org-html-export-and-open)
    (message "请在 Org Mode 文件中使用此命令")))
