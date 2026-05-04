;;; markdown-notes.el --- Markdown 笔记增强（截图 / 拖拽 / 字数统计） -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: writing, markdown, notes, images

;;; Commentary:

;; 笔记型 Markdown 工作流增强：
;;   - `henri/md-insert-screenshot' -- 截图贴入
;;   - 拖拽图片自动归档          -- dnd 协议覆写
;;   - `henri/md-word-count'       -- 中文字符 + 英文词数统计

;;; Code:

(require 'lib-system)
(require 'dnd)

;; ---------------------------------------------------------------------------
;; 图片资产目录管理

(defun henri/md--assets-dir (&optional filepath)
  "基于 FILEPATH（默认当前缓冲区文件）返回对应的 .assets/ 目录。"
  (let ((file (or filepath (buffer-file-name))))
    (unless file
      (user-error "当前缓冲区未关联文件"))
    (expand-file-name
     (concat (file-name-sans-extension (file-name-nondirectory file)) ".assets/")
     (file-name-directory file))))

(defun henri/md--ensure-assets-dir ()
  "确保当前文件对应的 .assets/ 目录存在，返回其路径。"
  (let ((dir (henri/md--assets-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun henri/md--timestamp-filename (ext)
  "生成带时间戳的文件名，扩展名为 EXT（如 \"png\"）。"
  (format-time-string (concat "%Y-%m-%d_%H-%M-%S." ext)))

;; ---------------------------------------------------------------------------
;; 截图贴入

(defun henri/md--screenshot-tool ()
  "返回当前系统可用的截图工具命令名，nil 表示未找到。"
  (cond
   ((eq (henri/get-os-type) 'macos)
    (when (henri/executable-p "pngpaste") "pngpaste"))
   ((eq (henri/get-os-type) 'linux)
    (cond ((henri/executable-p "wl-paste") "wl-paste")
          ((henri/executable-p "xclip") "xclip")))
   (t nil)))

(defun henri/md--screenshot-to-file (tool outfile)
  "使用 TOOL 将剪贴板图片保存到 OUTFILE。"
  (cond
   ((string= tool "pngpaste")
    (call-process "pngpaste" nil nil nil outfile))
   ((string= tool "wl-paste")
    (with-temp-buffer
      (call-process "wl-paste" nil t nil "-t" "image/png")
      (write-region (point-min) (point-max) outfile nil 'silent)))
   ((string= tool "xclip")
    (with-temp-buffer
      (call-process "xclip" nil t nil "-selection" "clipboard" "-t" "image/png" "-o")
      (write-region (point-min) (point-max) outfile nil 'silent)))
   (t (error "不支持的截图工具: %s" tool))))

;;;###autoload
(defun henri/md-insert-screenshot ()
  "将剪贴板中的截图保存到当前文件对应的 .assets/ 目录，并插入 Markdown 图片语法。"
  (interactive)
  (unless (eq major-mode 'markdown-mode)
    (user-error "当前不是 markdown-mode"))
  (unless (buffer-file-name)
    (user-error "请先保存文件"))
  (let ((tool (henri/md--screenshot-tool)))
    (unless tool
      (user-error "未找到截图工具：macOS 请安装 pngpaste（brew install pngpaste），Linux 请安装 wl-paste / xclip"))
    (let* ((assets (henri/md--ensure-assets-dir))
           (fname (henri/md--timestamp-filename "png"))
           (fpath (expand-file-name fname assets))
           (relpath (file-relative-name fpath (file-name-directory (buffer-file-name)))))
      (henri/md--screenshot-to-file tool fpath)
      (unless (file-exists-p fpath)
        (error "截图保存失败：%s" fpath))
      (insert (format "![%s](%s)" fname relpath))
      (message "[henri/md-notes] 已插入截图: %s" relpath))))

;; ---------------------------------------------------------------------------
;; 拖拽图片

(defun henri/md--dnd-image-handler (uri action)
  "处理拖拽到 markdown 缓冲区的图片 URI，复制到 .assets/ 并插入相对路径。"
  (let* ((file (dnd-get-local-file-name uri t)))
    (cond
     ((and file (file-exists-p file)
           (string-match-p "\\.\\(png\\|jpg\\|jpeg\\|gif\\|webp\\|svg\\)$" file))
      (let* ((assets (henri/md--ensure-assets-dir))
             (ext (or (file-name-extension file) "png"))
             (fname (henri/md--timestamp-filename ext))
             (fpath (expand-file-name fname assets))
             (relpath (file-relative-name fpath (file-name-directory (buffer-file-name)))))
        (copy-file file fpath t)
        (insert (format "![%s](%s)" fname relpath))
        (message "[henri/md-notes] 已插入图片: %s" relpath)
        'copy))
     (t
      ;; 非图片文件，回退到默认 dnd 处理
      (let ((dnd-protocol-alist nil))
        (dnd-handle-multiple-urls nil action (list uri)))))))

;;;###autoload
(defun henri/md-setup-dnd ()
  "为当前 markdown 缓冲区注册图片拖拽处理。"
  (setq-local dnd-protocol-alist
              (cons '("^file://" . henri/md--dnd-image-handler)
                    dnd-protocol-alist)))

(add-hook 'markdown-mode-hook #'henri/md-setup-dnd)

;; ---------------------------------------------------------------------------
;; 字数统计

;;;###autoload
(defun henri/md-word-count ()
  "统计当前 Markdown 缓冲区的中文字符数与英文词数，显示在 minibuffer。"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((chinese 0)
          (english-words 0))
      (while (not (eobp))
        (cond
         ;; 中文字符范围（粗略）
         ((and (>= (following-char) #x4e00) (<= (following-char) #x9fff))
          (setq chinese (1+ chinese))
          (forward-char 1))
         ;; CJK 扩展 A
         ((and (>= (following-char) #x3400) (<= (following-char) #x4dbf))
          (setq chinese (1+ chinese))
          (forward-char 1))
         ;; 英文单词字符
         ((looking-at "[a-zA-Z0-9_$]")
          (setq english-words (1+ english-words))
          (skip-chars-forward "a-zA-Z0-9_$"))
         (t
          (forward-char 1))))
      (message "中文字符: %d | 英文 token 数: %d | 含空白/标点的总字符数: %d"
               chinese english-words (- (point-max) (point-min))))))

;; ---------------------------------------------------------------------------
;; 键位绑定

(defvar markdown-mode-map)
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c m i s") #'henri/md-insert-screenshot)
  (define-key markdown-mode-map (kbd "C-c m w")   #'henri/md-word-count))

(provide 'markdown-notes)
;;; markdown-notes.el ends here
