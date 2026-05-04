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

(defconst henri/org-html-themes-root-placeholder "@@henri-org-html-themes-root@@"
  "In local theme #+HTML_HEAD lines; replaced in final HTML output (see
`henri/org-html--substitute-themes-root-in-output').")

(defun henri/org-html--substitute-themes-root-in-output (output backend _channel)
  "Replace `henri/org-html-themes-root-placeholder' with `henri-org-html-themes-directory'.
SETUPFILE is merged after `org-export-before-processing-hook', so replacement
runs on the final HTML string via `org-export-filter-final-output-functions'."
  (if (org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string
       (regexp-quote henri/org-html-themes-root-placeholder)
       (directory-file-name (expand-file-name henri-org-html-themes-directory))
       output t t)
    output))

(add-hook 'org-export-filter-final-output-functions
          #'henri/org-html--substitute-themes-root-in-output)

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
(defvar henri/org-html-themes-dir
  (file-name-as-directory
   (expand-file-name henri-org-html-themes-directory))
  "本地 org-html-themes 目录路径（默认在配置仓库内）。")

(defvar henri/org-html-themes-list
  `(("Henri Journal" . ,(expand-file-name "org/theme-henri-journal.setup" henri/org-html-themes-dir))
    ("Henri Bearblog" . ,(expand-file-name "org/theme-henri-bearblog.setup" henri/org-html-themes-dir))
    ("Henri" . ,(expand-file-name "org/theme-henri.setup" henri/org-html-themes-dir))
    ("ReadTheOrg" . ,(expand-file-name "org/theme-readtheorg.setup" henri/org-html-themes-dir))
    ("ReadTheOrg Local" . ,(expand-file-name "org/theme-readtheorg-local.setup" henri/org-html-themes-dir))
    ("Bigblow" . ,(expand-file-name "org/theme-bigblow.setup" henri/org-html-themes-dir))
    ("Bigblow Local" . ,(expand-file-name "org/theme-bigblow-local.setup" henri/org-html-themes-dir))
    ("ReadTheOrg (在线)" . "https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup")
    ("Bigblow (在线)" . "https://fniessen.github.io/org-html-themes/org/theme-bigblow.setup"))
  "可用的 org-html-themes 主题列表。
每个元素是一个 cons cell，格式为 (主题名称 . setup文件路径)。
默认使用本地主题，如果本地主题不存在，可以使用在线版本。")

(defvar henri/org-html-default-theme "Henri Journal"
  "默认使用的 HTML 主题名称。")

(defun henri/org-html-default-setupfile ()
  "Return the setup file for the default HTML theme."
  (henri/org-html-get-theme-setup-file henri/org-html-default-theme))

(defun henri/org-html-theme-present-p ()
  "Return non-nil if the current Org buffer already declares an HTML theme."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^[[:space:]]*#\\+SETUPFILE:.*\\(org-html-themes\\|theme-henri\\).*" nil t)))

(defun henri/org-html-insert-setupfile (setup-file)
  "Insert SETUP-FILE into current Org buffer metadata."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "^#\\+TITLE:")
        (forward-line 1)
      (goto-char (point-min)))
    (while (looking-at "^#\\+\\(AUTHOR\\|DATE\\|EMAIL\\|LANGUAGE\\|LATEX_CLASS\\|OPTIONS\\|STARTUP\\):")
      (forward-line 1))
    (insert (format "#+SETUPFILE: %s\n" setup-file))))

(defun henri/org-html-ensure-default-theme ()
  "Ensure the current Org buffer has the default HTML theme setupfile."
  (let ((setup-file (henri/org-html-default-setupfile)))
    (when (and setup-file (not (henri/org-html-theme-present-p)))
      (henri/org-html-insert-setupfile setup-file))))

;; =============================================================================
;; 简化主题映射系统

;; 主题编号映射表
(defvar henri/org-html-theme-shortcuts
  '(("default" . "Henri Journal")
    ("journal" . "Henri Journal")
    ("hj" . "Henri Journal")
    ("0" . "Henri Journal")
    ("bear" . "Henri Bearblog")
    ("bearblog" . "Henri Bearblog")
    ("hb" . "Henri Bearblog")
    ("h" . "Henri")
    ("henri" . "Henri")
    ("1" . "ReadTheOrg")
    ("rto" . "ReadTheOrg")
    ("readtheorg" . "ReadTheOrg")
    ("local" . "ReadTheOrg Local")
    ("rto-local" . "ReadTheOrg Local")
    ("2" . "Bigblow") 
    ("bb" . "Bigblow")
    ("bigblow" . "Bigblow"))
  "主题快捷方式映射表。
支持数字编号、缩写和全名调用主题。")

(defun henri/org-html-apply-theme-by-shortcut (shortcut)
  "使用快捷方式应用主题。
SHORTCUT 可以是数字编号、缩写或主题名称。
例如：'1', 'rto', 'readtheorg', 'default' 等。"
  (interactive "sHTML主题 (0=Henri Journal, bear=Henri Bearblog, 1=ReadTheOrg, 2=Bigblow): ")
  (let* ((normalized-shortcut (downcase (string-trim shortcut)))
         (theme-name (cdr (assoc normalized-shortcut henri/org-html-theme-shortcuts))))
    (if theme-name
        (progn
          (henri/org-html-set-theme theme-name)
          (message "已应用主题: %s (快捷方式: %s)" theme-name shortcut))
      (message "未知的主题快捷方式: %s" shortcut))))

(defun henri/org-html-show-theme-shortcuts ()
  "显示所有可用的主题快捷方式。"
  (interactive)
  (message "HTML主题快捷方式:\n0/default/journal/hj -> Henri Journal\nbear/hb -> Henri Bearblog\nh/henri -> Henri\n1/rto/readtheorg -> ReadTheOrg\nlocal/rto-local -> ReadTheOrg Local\n2/bb/bigblow -> Bigblow"))

;; 更简单的主题应用函数
(defun henri/org-html-theme-1 ()
  "应用主题1: ReadTheOrg"
  (interactive)
  (henri/org-html-set-theme "ReadTheOrg")
  (message "已应用主题1: ReadTheOrg"))

(defun henri/org-html-theme-2 ()
  "应用主题2: Bigblow"
  (interactive)
  (henri/org-html-set-theme "Bigblow")
  (message "已应用主题2: Bigblow"))

(defun henri/org-html-theme-default ()
  "应用默认主题"
  (interactive)
  (henri/org-html-apply-default-theme)
  (message "已应用默认主题: %s" henri/org-html-default-theme))

;; =============================================================================
;; 主题管理函数

(defun henri/org-html-get-theme-setup-file (theme-name)
  "获取指定主题的 setup 文件路径。
THEME-NAME 是主题名称。如果本地文件不存在，返回在线版本。"
  (let ((local-file (cdr (assoc theme-name henri/org-html-themes-list))))
    (if (and local-file 
             (not (string-prefix-p "https://" local-file))
             (file-exists-p local-file))
        local-file
      ;; 如果本地文件不存在，尝试使用在线版本
      (cdr (assoc (concat theme-name " (在线)") henri/org-html-themes-list)))))

(defun henri/org-html-check-local-themes ()
  "检查本地主题文件是否存在。"
  (interactive)
  (let ((readtheorg-local (expand-file-name "org/theme-readtheorg.setup" henri/org-html-themes-dir))
        (bigblow-local (expand-file-name "org/theme-bigblow.setup" henri/org-html-themes-dir)))
    (if (and (file-exists-p readtheorg-local)
             (file-exists-p bigblow-local))
        (progn
          (message "✅ 本地主题文件已就绪!")
          (message "ReadTheOrg: %s" readtheorg-local)
          (message "Bigblow: %s" bigblow-local)
          t)
      (progn
        (message "❌ 本地主题文件缺失，请运行安装脚本: ./install-themes.sh")
        (message "主题目录: %s" henri/org-html-themes-dir)
        nil))))

(defun henri/org-html-set-theme (theme-name)
  "为当前 Org 文件设置 HTML 主题。
THEME-NAME 是要设置的主题名称。"
  (interactive 
   (list (completing-read "选择 HTML 主题: " 
                          (mapcar #'car henri/org-html-themes-list)
                          nil t)))
  (let ((setup-file (henri/org-html-get-theme-setup-file theme-name)))
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

(defun henri/org-html-apply-default-theme ()
  "为当前 Org 文件应用默认的 HTML 主题。"
  (interactive)
  (henri/org-html-set-theme henri/org-html-default-theme))

(defun henri/org-html-remove-theme ()
  "移除当前 Org 文件的 HTML 主题设置。"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#\\+SETUPFILE:.*org-html-themes.*$" nil t)
      (delete-region (line-beginning-position) (1+ (line-end-position))))
    (message "已移除 HTML 主题设置")))

;; =============================================================================
;; 导出增强功能

(defun henri/org-html-export-and-open ()
  "导出当前 Org 文件为 HTML 并在浏览器中打开。"
  (interactive)
  (let ((html-file (org-html-export-to-html)))
    (when html-file
      (browse-url (concat "file://" (expand-file-name html-file)))
      (message "HTML 文件已导出并在浏览器中打开: %s" html-file))))

(defun henri/org-html-export-with-theme (theme-name)
  "使用指定主题导出当前 Org 文件为 HTML，不修改源 buffer。
THEME-NAME 是要使用的主题名称。
做法：在独立的临时 buffer 内复制源文本并注入 SETUPFILE，源 buffer
内容与 marker / overlay / undo 历史均不受影响。"
  (interactive
   (list (completing-read "选择导出主题: "
                          (mapcar #'car henri/org-html-themes-list)
                          nil t nil nil henri/org-html-default-theme)))
  (let ((setup-file (henri/org-html-get-theme-setup-file theme-name)))
    (if (not setup-file)
        (message "未找到主题: %s" theme-name)
      (let* ((src-buf (current-buffer))
             (src-file buffer-file-name)
             (export-buf (generate-new-buffer " *henri-org-html-export*")))
        (unwind-protect
            (with-current-buffer export-buf
              (insert-buffer-substring src-buf)
              ;; SETUPFILE 用相对路径时需要 buffer-file-name 提供 base
              (when src-file (setq buffer-file-name src-file))
              (let ((delay-mode-hooks t)) (org-mode))
              (goto-char (point-min))
              ;; 移除已有的 org-html-themes SETUPFILE 行
              (while (re-search-forward
                      "^#\\+SETUPFILE:.*org-html-themes.*$" nil t)
                (delete-region (line-beginning-position)
                               (1+ (line-end-position))))
              ;; 在 #+TITLE: 之后插入新的 SETUPFILE
              (goto-char (point-min))
              (when (looking-at "^#\\+TITLE:") (forward-line 1))
              (insert (format "#+SETUPFILE: %s\n" setup-file))
              ;; 导出
              (let ((html-file (org-html-export-to-html)))
                (when html-file
                  (browse-url (concat "file://" (expand-file-name html-file)))
                  (message "使用 %s 主题导出完成: %s"
                           theme-name html-file))))
          (when (buffer-live-p export-buf)
            ;; 防止误触发 ask-user-about-supersession-threat
            (with-current-buffer export-buf
              (set-buffer-modified-p nil))
            (kill-buffer export-buf)))))))

;; =============================================================================
;; 主题下载和本地化

(defun henri/org-html-download-themes ()
  "下载 org-html-themes 到本地 org 目录。"
  (interactive)
  (if (file-exists-p henri/org-html-themes-dir)
      (message "主题目录已存在: %s" henri/org-html-themes-dir)
    (let ((install-script (expand-file-name "install-themes.sh" 
                                           (file-name-directory henri/org-html-themes-dir))))
      (if (file-exists-p install-script)
          (progn
            (message "运行安装脚本...")
            (shell-command (format "cd %s && bash install-themes.sh" 
                                 (file-name-directory install-script))))
        (progn
          (message "正在下载 org-html-themes...")
          (shell-command 
           (format "cd %s && git clone https://github.com/fniessen/org-html-themes.git" 
                   (file-name-directory henri/org-html-themes-dir)))
          (if (file-exists-p henri/org-html-themes-dir)
              (message "org-html-themes 下载完成: %s" henri/org-html-themes-dir)
            (message "下载失败，请手动克隆仓库")))))))

(defun henri/org-html-install-themes ()
  "安装主题的便捷函数。"
  (interactive)
  (henri/org-html-download-themes)
  (henri/org-html-check-local-themes))

(defun henri/org-html-use-local-themes ()
  "强制使用本地的 org-html-themes。"
  (interactive)
  (if (henri/org-html-check-local-themes)
      (progn
        (setq henri/org-html-themes-list
              `(("ReadTheOrg" . ,(expand-file-name "org/theme-readtheorg.setup" henri/org-html-themes-dir))
                ("Bigblow" . ,(expand-file-name "org/theme-bigblow.setup" henri/org-html-themes-dir))))
        (message "已切换到本地主题"))
    (message "本地主题不可用，请先运行 henri/org-html-install-themes")))

(defun henri/org-html-use-online-themes ()
  "切换到使用在线的 org-html-themes。"
  (interactive)
  (setq henri/org-html-themes-list
        '(("ReadTheOrg" . "https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup")
          ("Bigblow" . "https://fniessen.github.io/org-html-themes/org/theme-bigblow.setup")))
  (message "已切换到在线主题"))

;; =============================================================================
;; 自定义样式支持

(defun henri/org-html-add-custom-css ()
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

(defun henri/org-html-add-dark-code-style ()
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

(defun henri/org-html-expand-content-width ()
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
  (define-key org-mode-map (kbd "C-c m h t") 'henri/org-html-set-theme)
  (define-key org-mode-map (kbd "C-c m h d") 'henri/org-html-apply-default-theme)
  (define-key org-mode-map (kbd "C-c m h e") 'henri/org-html-export-and-open)
  (define-key org-mode-map (kbd "C-c m h w") 'henri/org-html-export-with-theme)
  (define-key org-mode-map (kbd "C-c m h c") 'henri/org-html-add-custom-css)
  (define-key org-mode-map (kbd "C-c m h r") 'henri/org-html-remove-theme)
  (define-key org-mode-map (kbd "C-c m h i") 'henri/org-html-install-themes)
  (define-key org-mode-map (kbd "C-c m h k") 'henri/org-html-check-local-themes)
  
  (define-key org-mode-map (kbd "C-c m h s") 'henri/org-html-apply-theme-by-shortcut)
  (define-key org-mode-map (kbd "C-c m h ?") 'henri/org-html-show-theme-shortcuts)
  (define-key org-mode-map (kbd "C-c m h 1") 'henri/org-html-theme-1)
  (define-key org-mode-map (kbd "C-c m h 2") 'henri/org-html-theme-2)
  (define-key org-mode-map (kbd "C-c m h 0") 'henri/org-html-theme-default))

;; =============================================================================
;; 全局主题应用（可在任何地方使用）

;;;###autoload
(defun henri/org-html-theme (shortcut)
  "全局HTML主题应用命令。
可以在任何地方使用 M-x henri/org-html-theme 调用。
SHORTCUT 支持以下格式：
- 数字: 1 (ReadTheOrg), 2 (Bigblow)
- 缩写: rto, bb, default
- 全名: readtheorg, bigblow"
  (interactive "sHTML主题 (1=ReadTheOrg, 2=Bigblow, rto/bb/default): ")
  (if (eq major-mode 'org-mode)
      (henri/org-html-apply-theme-by-shortcut shortcut)
    (message "请在 Org Mode 文件中使用此命令")))

;;;###autoload
(defun henri/org-html-export-quick ()
  "快速HTML导出并打开浏览器"
  (interactive)
  (if (eq major-mode 'org-mode)
      (henri/org-html-export-and-open)
    (message "请在 Org Mode 文件中使用此命令")))

(provide 'org-html)

;;; org-html.el ends here
