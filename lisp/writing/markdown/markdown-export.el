;;; markdown-export.el --- Markdown 导出（PDF/HTML/docx） -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: writing, markdown, export

;;; Commentary:

;; Markdown → PDF / HTML / docx 导出，通过 pandoc 实现。
;; PDF 路径复用 LaTeX 模块的 CJK 字体与主题能力。
;;
;; 提供的命令：
;;   `henri/md-export-pdf'      -- pandoc → XeLaTeX PDF
;;   `henri/md-export-html'     -- pandoc → 独立 HTML
;;   `henri/md-export-docx'     -- pandoc → docx
;;   `henri/md-export-dispatch' -- transient 选择面板

;;; Code:

(require 'lib-system)
(require 'transient)

;; ---------------------------------------------------------------------------
;; 内部工具

(defun henri/md--default-output-file (ext)
  "基于当前缓冲区文件名生成输出路径，扩展名为 EXT。"
  (let* ((base (file-name-sans-extension (or (buffer-file-name)
                                             (buffer-name))))
         (fname (concat base ext))
         (dir (and (boundp 'henri-md-export-output-dir)
                   henri-md-export-output-dir)))
    (if dir
        (expand-file-name (file-name-nondirectory fname) dir)
      fname)))

(defun henri/md--cjk-mainfont ()
  "返回用于 LaTeX 导出的 CJK 主字体族名，nil 表示让 ctex 自动处理。"
  (let ((families (and (display-graphic-p) (font-family-list))))
    (or (and (boundp 'henri-org-cjk-serif-family)
             (stringp henri-org-cjk-serif-family)
             families
             (member henri-org-cjk-serif-family families)
             henri-org-cjk-serif-family)
        (and (require 'visual-fonts nil t)
             (fboundp 'henri--org-first-available-font)
             families
             (henri--org-first-available-font
              '("TsangerJinKai02" "仓耳今楷02" "Source Han Serif SC"
                "Noto Serif CJK SC" "Songti SC" "STSong" "Kaiti SC"
                "PingFang SC" "Microsoft YaHei" "Noto Sans CJK SC")
              families)))))

(defun henri/md--pdf-variables ()
  "返回 pandoc PDF 导出所需的变量参数列表（CJK 字体、页面设置）。"
  (let ((vars (list "--variable=geometry=margin=2.5cm"))
        (font (henri/md--cjk-mainfont)))
    (when font
      (push (format "--variable=CJKmainfont=%s" font) vars))
    vars))

(defun henri/md--run-pandoc (args &optional output-file)
  "运行 pandoc，传入 ARGS 列表，若 OUTPUT-FILE 非 nil 则提示结果。"
  (unless (henri/executable-p "pandoc")
    (user-error "未找到 pandoc：请先安装（如 brew install pandoc）"))
  (let ((cmd (mapconcat #'shell-quote-argument (cons "pandoc" args) " ")))
    (message "[henri/md-export] 运行: %s" cmd)
    (with-temp-buffer
      (let ((exit (call-process-shell-command cmd nil t)))
        (unless (zerop exit)
          (error "pandoc 导出失败（exit=%d）：\n%s" exit (buffer-string))))))
  (when output-file
    (message "[henri/md-export] 已生成: %s" output-file)))

;; ---------------------------------------------------------------------------
;; 导出命令

;;;###autoload
(defun henri/md-export-pdf (&optional async)
  "使用 pandoc + XeLaTeX 将当前 Markdown 导出为 PDF。
如果 ASYNC 非 nil，在后台异步执行。"
  (interactive)
  (unless (eq major-mode 'markdown-mode)
    (user-error "当前不是 markdown-mode"))
  (unless (buffer-file-name)
    (user-error "请先保存文件"))
  (let* ((engine (or (and (boundp 'henri-md-pdf-engine) henri-md-pdf-engine) "xelatex"))
         (output (henri/md--default-output-file ".pdf"))
         (args (append (list (buffer-file-name)
                             "-o" output
                             "--pdf-engine" engine
                             "-f" "markdown")
                       (henri/md--pdf-variables))))
    (unless (or (henri/executable-p "xelatex")
                (henri/executable-p "tectonic"))
      (user-error "未找到 xelatex / tectonic：请先安装 TeX 发行版（如 MacTeX）"))
    (if async
        (make-process
         :name "henri-md-export-pdf"
         :command (append (list "pandoc") args)
         :sentinel (lambda (proc _event)
                     (when (eq (process-status proc) 'exit)
                       (if (zerop (process-exit-status proc))
                           (message "[henri/md-export] PDF 已生成: %s" output)
                         (message "[henri/md-export] PDF 导出失败")))))
      (henri/md--run-pandoc args output))))

;;;###autoload
(defun henri/md-export-html (&optional async)
  "使用 pandoc 将当前 Markdown 导出为独立 HTML。"
  (interactive)
  (unless (eq major-mode 'markdown-mode)
    (user-error "当前不是 markdown-mode"))
  (unless (buffer-file-name)
    (user-error "请先保存文件"))
  (let* ((output (henri/md--default-output-file ".html"))
         (css-args (when (fboundp 'henri/markdown--pandoc-css-args)
                     (henri/markdown--pandoc-css-args)))
         (args (append (list (buffer-file-name)
                            "-o" output
                            "-s"                      ; standalone
                            "--toc"                   ; 自动目录
                            "-f" "markdown"
                            "-t" "html5")
                      css-args)))
    (if async
        (make-process
         :name "henri-md-export-html"
         :command (append (list "pandoc") args)
         :sentinel (lambda (proc _event)
                     (when (eq (process-status proc) 'exit)
                       (if (zerop (process-exit-status proc))
                           (message "[henri/md-export] HTML 已生成: %s" output)
                         (message "[henri/md-export] HTML 导出失败")))))
      (henri/md--run-pandoc args output))))

;;;###autoload
(defun henri/md-export-docx (&optional async)
  "使用 pandoc 将当前 Markdown 导出为 docx。"
  (interactive)
  (unless (eq major-mode 'markdown-mode)
    (user-error "当前不是 markdown-mode"))
  (unless (buffer-file-name)
    (user-error "请先保存文件"))
  (let* ((output (henri/md--default-output-file ".docx"))
         (args (list (buffer-file-name)
                     "-o" output
                     "-f" "markdown"
                     "-t" "docx")))
    (if async
        (make-process
         :name "henri-md-export-docx"
         :command (append (list "pandoc") args)
         :sentinel (lambda (proc _event)
                     (when (eq (process-status proc) 'exit)
                       (if (zerop (process-exit-status proc))
                           (message "[henri/md-export] docx 已生成: %s" output)
                         (message "[henri/md-export] docx 导出失败")))))
      (henri/md--run-pandoc args output))))

;; ---------------------------------------------------------------------------
;; Transient 分发面板

;;;###autoload
(transient-define-prefix henri/md-export-dispatch ()
  "Markdown 导出格式选择面板。"
  ["导出目标"
   [("p" "PDF  (pandoc → XeLaTeX)"  henri/md-export-pdf)
    ("h" "HTML (pandoc → standalone)" henri/md-export-html)
    ("d" "docx (pandoc → Word)"       henri/md-export-docx)]])

;; ---------------------------------------------------------------------------
;; 键位绑定

(defvar markdown-mode-map)
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c m e p") #'henri/md-export-pdf)
  (define-key markdown-mode-map (kbd "C-c m e h") #'henri/md-export-html)
  (define-key markdown-mode-map (kbd "C-c m e d") #'henri/md-export-docx)
  (define-key markdown-mode-map (kbd "C-c m e e") #'henri/md-export-dispatch))

(provide 'markdown-export)
;;; markdown-export.el ends here
