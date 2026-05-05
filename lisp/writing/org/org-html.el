;;; org-html.el --- Org Mode HTML 导出配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Keywords: org, html, export, themes

;;; Commentary:

;; Org Mode HTML 导出配置，包含：
;; - org-html-themes 集成
;; - 默认主题设置（Henri Notes）
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
  "Replace local theme root placeholders in HTML OUTPUT.
SETUPFILE is merged after `org-export-before-processing-hook', so replacement
runs on the final HTML string via `org-export-filter-final-output-functions'."
  (if (org-export-derived-backend-p backend 'html)
      (let ((result output))
        (setq result
              (replace-regexp-in-string
               (regexp-quote henri/org-html-themes-root-placeholder)
               (directory-file-name (expand-file-name henri-org-html-themes-directory))
               result t t))
        result)
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
  `(("Henri Notes" . ,(expand-file-name "org/theme-henri-notes.setup" henri/org-html-themes-dir))
    ("Henri Journal" . ,(expand-file-name "org/theme-henri-journal.setup" henri/org-html-themes-dir))
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

(defvar henri/org-html-default-theme "Henri Notes"
  "默认使用的 HTML 主题名称。")

(defun henri/org-html-default-setupfile ()
  "Return the setup file for the default HTML theme."
  (henri/org-html-get-theme-setup-file henri/org-html-default-theme))

(defun henri/org-html--read-theme-keyword ()
  "Return the theme name from #+HENRI_HTML_THEME: in the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           "^#\\+HENRI_HTML_THEME:[[:space:]]*\\(.+\\)$" nil t)
      (string-trim (match-string 1)))))

(defun henri/org-html-theme-present-p ()
  "Return non-nil if the current Org buffer declares a valid HTML theme.
Checks for a #+HENRI_HTML_THEME: keyword first, then falls back to a valid
existing #+SETUPFILE: line (for backward compatibility with older files)."
  (or (henri/org-html--read-theme-keyword)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward
               "^[[:space:]]*#\\+SETUPFILE:[[:space:]]*\\(.*\\(?:org-html-themes\\|theme-henri\\).*\\)$" nil t)
          (let ((path (string-trim (match-string 1))))
            (or (string-prefix-p "https://" path)
                (string-prefix-p "http://" path)
                (file-exists-p path)))))))

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

(defun henri/org-html--setupfile-to-local-theme (path)
  "Try to resolve a broken setupfile PATH to a valid local equivalent.
Extracts the theme filename (e.g. theme-henri-journal.setup) and looks it
up under `henri/org-html-themes-dir'.  Returns the valid local path, or nil."
  (when (string-match "\\(theme-[^/]*\\.setup\\)\\'" path)
    (let ((local (expand-file-name (concat "org/" (match-string 1 path))
                                   henri/org-html-themes-dir)))
      (and (file-exists-p local) local))))

(defun henri/org-html--journal-file-p ()
  "Return non-nil when the exported file is a Journal monthly file."
  (and buffer-file-name
       (string-match-p "\\`journal-[0-9]\\{4\\}-[0-9]\\{2\\}\\.org\\'"
                       (file-name-nondirectory buffer-file-name))))

(defun henri/org-html-ensure-default-theme-for-export (backend)
  "Use the appropriate HTML theme for Org HTML exports.

This runs in Org's temporary export buffer; the source file is not changed.

Theme resolution order:
1. #+HENRI_HTML_THEME: keyword — resolved to a local setup file at export
   time, fully portable across machines.
2. Existing #+SETUPFILE: pointing to org-html-themes / theme-henri — kept
   if the file exists or is a remote URL; stale paths are repaired or removed.
3. Journal monthly files automatically receive the Henri Journal theme.
4. Everything else receives the default Henri Notes theme."
  (when (org-export-derived-backend-p backend 'html)
    ;; Phase 1: resolve #+HENRI_HTML_THEME: to an actual #+SETUPFILE:.
    (let ((declared-theme (henri/org-html--read-theme-keyword)))
      (when declared-theme
        ;; Remove any stale #+SETUPFILE: lines from org-html-themes so the
        ;; keyword takes precedence.
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward
                  "^#\\+SETUPFILE:.*\\(?:org-html-themes\\|theme-henri\\).*$" nil t)
            (delete-region (line-beginning-position) (1+ (line-end-position)))))
        (let ((setup-file (henri/org-html-get-theme-setup-file declared-theme)))
          (when setup-file
            (henri/org-html-insert-setupfile setup-file)))))
    ;; Phase 2: repair broken legacy #+SETUPFILE: lines (files without keyword).
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^[[:space:]]*#\\+SETUPFILE:[[:space:]]*\\(.*\\(?:org-html-themes\\|theme-henri\\).*\\)$" nil t)
        (let* ((path (string-trim (match-string 1)))
               (valid (or (string-prefix-p "https://" path)
                          (string-prefix-p "http://" path)
                          (file-exists-p path))))
          (unless valid
            (let ((replacement (henri/org-html--setupfile-to-local-theme path)))
              (delete-region (line-beginning-position) (line-end-position))
              (if replacement
                  (insert (format "#+SETUPFILE: %s" replacement))
                (delete-region (1- (point)) (point))))))))
    ;; Phase 3: if no theme at all, inject the default (or Journal theme).
    (let ((theme-name (if (henri/org-html--journal-file-p)
                          "Henri Journal"
                        henri/org-html-default-theme)))
      (when (not (henri/org-html-theme-present-p))
        (let ((setup-file (henri/org-html-get-theme-setup-file theme-name)))
          (when setup-file
            (henri/org-html-insert-setupfile setup-file)))))))

(add-hook 'org-export-before-processing-hook
          #'henri/org-html-ensure-default-theme-for-export)

(defconst henri/org-html-file-metadata-properties
  '("ID" "CREATED" "UPDATED" "ROAM_REFS" "ROAM_ALIASES" "CUSTOM_ID"
    "FILETAGS" "LAST_MODIFIED" "REF")
  "File-level Org metadata properties hidden from HTML body exports.")

(defun henri/org-html--metadata-property-line-p ()
  "Return non-nil when point is on a file-level metadata property line."
  (and (looking-at "^:\\([A-Z0-9_]+\\):[ \t]*")
       (member (match-string 1) henri/org-html-file-metadata-properties)))

(defun henri/org-html--delete-following-blank-lines ()
  "Delete blank lines following point."
  (while (looking-at "^[ \t]*$")
    (delete-region (line-beginning-position) (line-beginning-position 2))))

(defun henri/org-html-remove-file-metadata-for-export (backend)
  "Remove file-level Org-roam metadata from temporary HTML export buffers.
The source Org file is not changed.  This hides top-level property drawers and
legacy bare property lines such as :ID:, :CREATED: and :UPDATED: from HTML
body output while keeping those values available in the Org source."
  (when (org-export-derived-backend-p backend 'html)
    (save-excursion
      (goto-char (point-min))
      (let ((continue t))
        (while continue
          (cond
           ((looking-at "^[ \t]*$")
            (forward-line 1))
           ((looking-at "^#\\+")
            (forward-line 1))
           ((looking-at "^:PROPERTIES:[ \t]*$")
            (let ((start (point)))
              (if (re-search-forward "^:END:[ \t]*$" nil t)
                  (progn
                    (delete-region start (line-beginning-position 2))
                    (henri/org-html--delete-following-blank-lines))
                (setq continue nil))))
           ((henri/org-html--metadata-property-line-p)
            (while (henri/org-html--metadata-property-line-p)
              (delete-region (line-beginning-position) (line-beginning-position 2)))
            (henri/org-html--delete-following-blank-lines))
           (t
            (setq continue nil))))))))

(add-hook 'org-export-before-processing-hook
          #'henri/org-html-remove-file-metadata-for-export)

;; =============================================================================
;; 简化主题映射系统

;; 主题编号映射表
(defvar henri/org-html-theme-shortcuts
  '(("default" . "Henri Notes")
    ("notes" . "Henri Notes")
    ("hn" . "Henri Notes")
    ("0" . "Henri Notes")
    ("journal" . "Henri Journal")
    ("hj" . "Henri Journal")
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
  (interactive "sHTML主题 (0=Henri Notes, journal=Henri Journal, bear=Henri Bearblog, 1=ReadTheOrg, 2=Bigblow): ")
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
  (message "HTML主题快捷方式:\n0/default/notes/hn -> Henri Notes\njournal/hj -> Henri Journal\nbear/hb -> Henri Bearblog\nh/henri -> Henri\n1/rto/readtheorg -> ReadTheOrg\nlocal/rto-local -> ReadTheOrg Local\n2/bb/bigblow -> Bigblow"))

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
  "Set the HTML theme for the current Org file to THEME-NAME.

Writes a portable #+HENRI_HTML_THEME: keyword to the source file.  The
actual #+SETUPFILE: path is resolved at export time from the current
machine's `henri-org-html-themes-directory', so the source file remains
portable across devices."
  (interactive
   (list (completing-read "选择 HTML 主题: "
                          (mapcar #'car henri/org-html-themes-list)
                          nil t)))
  (save-excursion
    (goto-char (point-min))
    ;; Remove existing #+HENRI_HTML_THEME: line.
    (while (re-search-forward "^#\\+HENRI_HTML_THEME:.*$" nil t)
      (delete-region (line-beginning-position) (1+ (line-end-position))))
    ;; Remove legacy #+SETUPFILE: lines referencing org-html-themes.
    (goto-char (point-min))
    (while (re-search-forward "^#\\+SETUPFILE:.*\\(?:org-html-themes\\|theme-henri\\).*$" nil t)
      (delete-region (line-beginning-position) (1+ (line-end-position))))
    ;; Insert the portable keyword after other #+KEYWORD: lines.
    (goto-char (point-min))
    (when (looking-at "^#\\+TITLE:") (forward-line 1))
    (while (looking-at "^#\\+\\(AUTHOR\\|DATE\\|EMAIL\\|LANGUAGE\\|LATEX_CLASS\\|OPTIONS\\|STARTUP\\):")
      (forward-line 1))
    (insert (format "#+HENRI_HTML_THEME: %s\n" theme-name)))
  (message "已设置 HTML 主题为: %s" theme-name))

(defun henri/org-html-apply-default-theme ()
  "Apply the default HTML theme to the current Org file.
Writes a portable #+HENRI_HTML_THEME: keyword; no absolute paths in source."
  (interactive)
  (henri/org-html-set-theme henri/org-html-default-theme))

(defun henri/org-html-remove-theme ()
  "Remove the HTML theme setting from the current Org file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#\\+HENRI_HTML_THEME:.*$" nil t)
      (delete-region (line-beginning-position) (1+ (line-end-position))))
    (goto-char (point-min))
    (while (re-search-forward "^#\\+SETUPFILE:.*\\(?:org-html-themes\\|theme-henri\\).*$" nil t)
      (delete-region (line-beginning-position) (1+ (line-end-position))))
    (message "已移除 HTML 主题设置")))

;; =============================================================================
;; 导出增强功能

(defun henri/org-html-export-and-open ()
  "Export current Org file with the default HTML theme and open it."
  (interactive)
  (henri/org-html-export-with-theme henri/org-html-default-theme))

(defun henri/org-html-export-with-theme (theme-name)
  "Export current Org file as HTML with THEME-NAME, without modifying the source.
Works in a temporary buffer; the source buffer is not changed."
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
              (when src-file (setq buffer-file-name src-file))
              (let ((delay-mode-hooks t)) (org-mode))
              (goto-char (point-min))
              ;; Remove #+HENRI_HTML_THEME: and legacy #+SETUPFILE: lines.
              (while (re-search-forward "^#\\+HENRI_HTML_THEME:.*$" nil t)
                (delete-region (line-beginning-position)
                               (1+ (line-end-position))))
              (goto-char (point-min))
              (while (re-search-forward
                      "^#\\+SETUPFILE:.*\\(?:org-html-themes\\|theme-henri\\).*$" nil t)
                (delete-region (line-beginning-position)
                               (1+ (line-end-position))))
              ;; Insert the resolved SETUPFILE.
              (goto-char (point-min))
              (when (looking-at "^#\\+TITLE:") (forward-line 1))
              (insert (format "#+SETUPFILE: %s\n" setup-file))
              ;; Export.
              (let ((html-file (org-html-export-to-html)))
                (when html-file
                  (browse-url (concat "file://" (expand-file-name html-file)))
                  (message "使用 %s 主题导出完成: %s"
                           theme-name html-file))))
          (when (buffer-live-p export-buf)
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
              `(("Henri Notes" . ,(expand-file-name "org/theme-henri-notes.setup" henri/org-html-themes-dir))
                ("Henri Journal" . ,(expand-file-name "org/theme-henri-journal.setup" henri/org-html-themes-dir))
                ("Henri Bearblog" . ,(expand-file-name "org/theme-henri-bearblog.setup" henri/org-html-themes-dir))
                ("Henri" . ,(expand-file-name "org/theme-henri.setup" henri/org-html-themes-dir))
                ("ReadTheOrg" . ,(expand-file-name "org/theme-readtheorg.setup" henri/org-html-themes-dir))
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
