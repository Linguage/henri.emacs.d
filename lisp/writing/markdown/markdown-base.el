;;; markdown-base.el --- Markdown 写作子模块 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: writing, markdown

;;; Commentary:

;; 本文件是 writing 模块下的 Markdown 子模块，承载与 Markdown 写作相关
;; 的全部基础配置：
;;
;;   - markdown-mode  -- Markdown 语法支持
;;   - 预览：
;;     * `henri/markdown-preview-eww'    -- pandoc → 临时 HTML → 内置 EWW
;;     * `henri/markdown-preview-browser'-- pandoc → 临时 HTML → 系统浏览器
;;     * `markdown-live-preview-mode'    -- markdown-mode 自带实时预览
;;     * `grip-mode'                     -- GitHub 风格（需 grip CLI）
;;
;; 由 `init-writing.el' 通过 `(require 'markdown-base)' 加载。

;;; Code:

(require 'lib-system)
(require 'browse-url)
(require 'subr-x)
(require 'url-util)

;; -----------------------------------------------------------------------------
;; 依赖体检

(defun henri/markdown-check-preview-deps ()
  "报告 pandoc / grip 是否在 PATH 中（用于 Markdown 预览）。"
  (interactive)
  (let ((p (henri/executable-p "pandoc"))
        (g (henri/executable-p "grip")))
    (message "[Markdown 预览依赖] pandoc: %s | grip: %s | henri-enable-grip: %S | exec-path 已注入: %s"
             (if p (format "OK (%s)" (executable-find "pandoc")) "缺失（brew install pandoc）")
             (if g (format "OK (%s)" (executable-find "grip")) "缺失（pip install grip）")
             (if (boundp 'henri-enable-grip) henri-enable-grip 'unbound)
             (if (member "/opt/homebrew/bin" exec-path) "是" "否（重启 Emacs 让 exec-path-from-shell 生效）"))))

;; -----------------------------------------------------------------------------
;; HTML 预览主题系统

(defconst henri/markdown-themes-dir
  (expand-file-name "themes/" (file-name-directory (or load-file-name buffer-file-name)))
  "Markdown 预览/导出 CSS 主题目录。")

(defvar henri/markdown-themes-list
  `(("Henri Journal"  . ,(expand-file-name "henri-journal.css" henri/markdown-themes-dir))
    ("Henri Bearblog" . ,(expand-file-name "henri-bearblog.css" henri/markdown-themes-dir))
    ("GFM"            . ,(expand-file-name "henri-gfm.css" henri/markdown-themes-dir))
    ("None"           . nil))
  "可用的 Markdown HTML 主题列表。每个元素为 (名称 . CSS路径)。")

(defvar henri/markdown-preview-theme "Henri Journal"
  "当前 Markdown 预览/导出使用的 HTML 主题名称。")

(defun henri/markdown--theme-css-path ()
  "返回当前主题的 CSS 文件路径，nil 表示无主题。"
  (cdr (assoc henri/markdown-preview-theme henri/markdown-themes-list)))

(defun henri/markdown-set-theme (theme-name)
  "设置 Markdown 预览/导出使用的 HTML 主题为 THEME-NAME。"
  (interactive
   (list (completing-read "Markdown HTML 主题: "
                          (mapcar #'car henri/markdown-themes-list)
                          nil t nil nil henri/markdown-preview-theme)))
  (setq henri/markdown-preview-theme theme-name)
  (message "[md-theme] 已设置: %s" theme-name))

;; -----------------------------------------------------------------------------
;; pandoc → 临时 HTML 预览（自实现，不依赖 markdown-preview-eww 死代码）

(defvar henri/markdown--preview-tmpdir nil
  "Markdown 预览 HTML 临时目录，懒初始化。")

(defun henri/markdown--preview-tmpdir ()
  "返回（必要时创建）Markdown 预览 HTML 临时目录。"
  (unless (and henri/markdown--preview-tmpdir
               (file-directory-p henri/markdown--preview-tmpdir))
    (setq henri/markdown--preview-tmpdir
          (make-temp-file "henri-md-preview-" t)))
  henri/markdown--preview-tmpdir)

(defun henri/markdown--pandoc-css-args ()
  "返回 pandoc --css 参数列表（根据当前主题）。"
  (let ((css (henri/markdown--theme-css-path)))
    (if (and css (file-exists-p css))
        (list "--css" css)
      nil)))

(defun henri/markdown--file-url (path)
  "Return a file:// URL for PATH."
  (browse-url-file-url (expand-file-name path)))

(defun henri/markdown--preview-base-header (source-file)
  "Write and return a temporary HTML header with a base URL for SOURCE-FILE.

Pandoc preview HTML lives under a temp directory.  Without a <base> tag,
EWW resolves relative Markdown links against that temp directory instead
of the source document directory."
  (let* ((base-dir (file-name-as-directory
                    (file-name-directory (expand-file-name source-file))))
         (header-file (expand-file-name "base.html"
                                        (henri/markdown--preview-tmpdir))))
    (with-temp-file header-file
      (insert (format "<base href=\"%s\">\n"
                      (henri/markdown--file-url base-dir))))
    header-file))

(defun henri/markdown--relative-url-p (url)
  "Return non-nil when URL is relative to the current Markdown file."
  (and (stringp url)
       (not (string-empty-p url))
       (not (string-prefix-p "#" url))
       (not (string-prefix-p "/" url))
       (not (string-prefix-p "//" url))
       (not (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" url))))

(defun henri/markdown--absolute-file-url (url base-dir)
  "Resolve relative URL against BASE-DIR and return a file:// URL."
  (let* ((hash-pos (string-match-p "#" url))
         (path-part (if hash-pos (substring url 0 hash-pos) url))
         (fragment (if hash-pos (substring url hash-pos) "")))
    (if (string-empty-p path-part)
        url
      (concat (henri/markdown--file-url
               (expand-file-name (url-unhex-string path-part) base-dir))
              fragment))))

(defun henri/markdown--rewrite-html-relative-links (html-file source-file)
  "Rewrite relative href/src links in HTML-FILE against SOURCE-FILE's directory."
  (let ((base-dir (file-name-directory (expand-file-name source-file))))
    (with-temp-buffer
      (insert-file-contents html-file)
      (goto-char (point-min))
      (while (re-search-forward "\\(href\\|src\\)=\"\\([^\"]+\\)\"" nil t)
        (let ((attr (match-string 1))
              (url (match-string 2)))
          (when (henri/markdown--relative-url-p url)
            (let ((replacement
                   (save-match-data
                     (format "%s=\"%s\""
                             attr
                             (henri/markdown--absolute-file-url url base-dir)))))
              (replace-match replacement t t)))))
      (write-region (point-min) (point-max) html-file nil 'silent))))

(defun henri/markdown--render-to-html ()
  "用 pandoc 把当前缓冲区渲染为独立 HTML，返回 HTML 文件路径。"
  (unless (henri/executable-p "pandoc")
    (user-error "未找到 pandoc：请先安装（brew install pandoc）并重启 Emacs"))
  (let* ((src (or (buffer-file-name)
                  ;; 未保存的缓冲区：先写到临时 .md
                  (let ((tmp (expand-file-name
                              "buffer.md" (henri/markdown--preview-tmpdir))))
                    (write-region (point-min) (point-max) tmp nil 'silent)
                    tmp)))
         (out (expand-file-name
               (concat (file-name-base src) ".html")
               (henri/markdown--preview-tmpdir)))
         (resource-path (file-name-directory src))
         (base-header (henri/markdown--preview-base-header src))
         (args (append
                (list src
                      "-o" out
                      "-s"
                      "--include-in-header" base-header
                      "--metadata" (format "title=%s" (file-name-base src))
                      "--resource-path" resource-path
                      "-f" "gfm"
                      "-t" "html5")
                (henri/markdown--pandoc-css-args))))
    (with-temp-buffer
      (let ((exit (apply #'call-process "pandoc" nil t nil args)))
        (unless (zerop exit)
          (pop-to-buffer (current-buffer))
          (error "pandoc 渲染失败（exit=%d）" exit))))
    (henri/markdown--rewrite-html-relative-links out src)
    out))

;;;###autoload
(defun henri/markdown-preview-eww ()
  "用 pandoc 渲染当前 Markdown 并在 EWW 中打开。"
  (interactive)
  (unless (derived-mode-p 'markdown-mode)
    (user-error "当前不是 markdown-mode"))
  (let ((html (henri/markdown--render-to-html)))
    (require 'eww)
    (eww-open-file html)
    (message "[md-preview] EWW: %s" html)))

;;;###autoload
(defun henri/markdown-preview-browser ()
  "用 pandoc 渲染当前 Markdown 并在系统浏览器中打开。"
  (interactive)
  (unless (derived-mode-p 'markdown-mode)
    (user-error "当前不是 markdown-mode"))
  (let ((html (henri/markdown--render-to-html)))
    (browse-url (concat "file://" html))
    (message "[md-preview] browser: %s" html)))

;; 兼容旧别名
(defalias 'henri/markdown-preview-offline #'henri/markdown-preview-eww)

;; -----------------------------------------------------------------------------
;; grip 预览（GitHub 风格）

;;;###autoload
(defun henri/markdown-preview-github-style ()
  "使用 grip 的 GitHub 风格预览（需 `henri-enable-grip' 非 nil 且已安装 grip）。"
  (interactive)
  (unless (derived-mode-p 'markdown-mode)
    (user-error "当前不是 markdown-mode"))
  (unless (bound-and-true-p henri-enable-grip)
    (user-error "grip 已在配置中关闭：将 `henri-enable-grip' 设为 t 并重启 Emacs"))
  (unless (henri/executable-p "grip")
    (user-error "未找到 grip：请先安装（pip install grip）并重启 Emacs"))
  (unless (require 'grip-mode nil t)
    (user-error "grip-mode 未加载：请检查 `use-package grip-mode' 是否启用"))
  (call-interactively #'grip-mode))

;; -----------------------------------------------------------------------------
;; Markdown 基础配置

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-display-remote-images t)
  :bind (:map markdown-mode-map
         ("C-c C-c p" . henri/markdown-preview-eww)
         ("C-c C-c v" . henri/markdown-preview-browser)
         ("C-c C-c l" . markdown-live-preview-mode)
         ("C-c m p"   . henri/markdown-preview-eww)
         ("C-c m v"   . henri/markdown-preview-browser)
         ("C-c m l"   . markdown-live-preview-mode)
         ("C-c m g"   . henri/markdown-preview-github-style)
         ("C-c m c"   . henri/markdown-check-preview-deps)
         ("C-c m s"   . henri/markdown-set-theme)))

;; GitHub 风格预览支持（按需启用）
(use-package grip-mode
  :ensure t
  :after markdown-mode
  :if henri-enable-grip
  :bind (:map markdown-mode-map
         ("C-c C-g" . grip-mode)))

;; -----------------------------------------------------------------------------
;; CJK 字体：复用 Org 的 CJK serif 字体（苍耳今楷等），与 Org 视觉一致。
;; 使用 face-remap 机制，仅影响 markdown 缓冲区，不改变全局字体。

(defvar-local henri--markdown-face-remap-cookies nil
  "本缓冲区 `face-remap-add-relative' 的 cookie 列表。")

(defun henri/markdown-clear-cjk-face-remaps ()
  "移除由 `henri/markdown-setup-body-font' 安装的 face remap。"
  (dolist (cookie henri--markdown-face-remap-cookies)
    (when cookie
      (face-remap-remove-relative cookie)))
  (setq henri--markdown-face-remap-cookies nil))

(defun henri/markdown-setup-body-font (&optional buffer)
  "为 Markdown 缓冲区应用 CJK serif 字体，与 Org 模式视觉一致。
字体取自 `henri-org-cjk-serif-family'（默认苍耳今楷）。"
  (let ((target (or buffer (current-buffer))))
    (when (buffer-live-p target)
      (with-current-buffer target
        (when (derived-mode-p 'markdown-mode)
          (henri/markdown-clear-cjk-face-remaps)
          (when (display-graphic-p)
            (require 'visual-fonts)
            (let* ((families (font-family-list))
                   (serif
                    (or (and (boundp 'henri-org-cjk-serif-family)
                             (stringp henri-org-cjk-serif-family)
                             (member henri-org-cjk-serif-family families)
                             henri-org-cjk-serif-family)
                        (and (fboundp 'henri--org-first-available-font)
                             (henri--org-first-available-font
                              (bound-and-true-p henri--org-cjk-serif-candidates)
                              families))))
                   (sans
                    (or (and (boundp 'henri-org-cjk-sans-family)
                             (stringp henri-org-cjk-sans-family)
                             (member henri-org-cjk-sans-family families)
                             henri-org-cjk-sans-family)
                        (and (fboundp 'henri--org-first-available-font)
                             (henri--org-first-available-font
                              (bound-and-true-p henri--org-cjk-sans-candidates)
                              families)))))
              (when serif
                (and (fboundp 'henri/org-apply-cjk-fontset)
                     (henri/org-apply-cjk-fontset serif))
                (push (face-remap-add-relative 'default :family serif :height 1.08)
                      henri--markdown-face-remap-cookies)
                (push (face-remap-add-relative 'variable-pitch :family serif :height 1.08)
                      henri--markdown-face-remap-cookies))
              ;; Markdown heading 使用 sans 字体（与 Org 标题一致）
              (when sans
                (dolist (sym '(markdown-header-face-1
                                markdown-header-face-2
                                markdown-header-face-3
                                markdown-header-face-4
                                markdown-header-face-5
                                markdown-header-face-6))
                  (when (facep sym)
                    (push (face-remap-add-relative sym :family sans)
                          henri--markdown-face-remap-cookies)))))))))))

(add-hook 'markdown-mode-hook #'henri/markdown-setup-body-font)
(add-hook 'markdown-mode-hook
          (lambda ()
            (let ((buffer (current-buffer)))
              (run-with-timer 0.05 nil #'henri/markdown-setup-body-font buffer))))

(provide 'markdown-base)

;;; markdown-base.el ends here
