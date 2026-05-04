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
         (args (append
                (list src
                      "-o" out
                      "-s"
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
