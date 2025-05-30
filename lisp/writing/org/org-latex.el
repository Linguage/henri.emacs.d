;;; org-latex.el --- Org Mode LaTeX 导出统一管理器 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 2.0
;; Keywords: org, latex, export, themes

;;; Commentary:

;; Org Mode LaTeX 导出统一管理系统，包含：
;; - 多主题支持
;; - 统一的 PDF 处理流程
;; - 智能编译器选择
;; - 错误处理和诊断

;;; Code:

;; =============================================================================
;; 核心配置

;; 基础 LaTeX 设置
(setq org-latex-compiler "xelatex")
(setq org-latex-prefer-user-labels t)

;; =============================================================================
;; 主题配置系统

(defvar my/org-latex-themes
  '((standard
     :name "标准主题"
     :class "ctexart"
     :packages ("amsmath" "amssymb" "graphicx" "hyperref")
     :geometry "top=2.5cm, bottom=2.5cm, left=2cm, right=2cm"
     :linespread 1.15
     :description "简洁的标准中文文档主题")
    
    (academic
     :name "学术主题"
     :class "ctexart"
     :packages ("amsmath" "amssymb" "amsfonts" "graphicx" "booktabs" "longtable" 
               "natbib" "cleveref" "hyperref")
     :geometry "top=2.5cm, bottom=2.5cm, left=2.5cm, right=2.5cm"
     :linespread 1.2
     :has-bibliography t
     :description "学术论文主题，支持参考文献")
    
    (modern
     :name "现代主题"
     :class "ctexart"
     :packages ("amsmath" "amssymb" "graphicx" "xcolor" "tcolorbox" "hyperref")
     :geometry "top=2cm, bottom=2cm, left=2cm, right=2cm"
     :linespread 1.1
     :colors t
     :description "现代化设计，支持彩色框和装饰")
    
    (minimal
     :name "极简主题"
     :class "article"
     :packages ("amsmath" "graphicx" "hyperref")
     :geometry "top=3cm, bottom=3cm, left=3cm, right=3cm"
     :linespread 1.0
     :description "极简设计，最少的包依赖"))
  "可用的 LaTeX 主题配置列表。")

(defvar my/current-latex-theme 'standard
  "当前使用的 LaTeX 主题。")

;; =============================================================================
;; PDF 处理流程管理

(defvar my/latex-pdf-processes
  '((standard . ("rm -f %b.aux %b.log %b.out %b.toc %b.bbl %b.blg"
                 "xelatex -interaction nonstopmode -output-directory %o %f"
                 "xelatex -interaction nonstopmode -output-directory %o %f"
                 "rm -f %b.aux %b.log %b.out %b.toc %b.bbl %b.blg"))
    
    (academic . ("rm -f %b.aux %b.log %b.out %b.toc %b.bbl %b.blg"
                 "xelatex -interaction nonstopmode -output-directory %o %f"
                 "bibtex %b"
                 "xelatex -interaction nonstopmode -output-directory %o %f"
                 "xelatex -interaction nonstopmode -output-directory %o %f"
                 "rm -f %b.aux %b.log %b.out %b.toc"))
    
    (fast . ("xelatex -interaction nonstopmode -output-directory %o %f")))
  "不同类型文档的 PDF 处理流程。")

;; =============================================================================
;; 主题管理函数

(defun my/get-theme-config (theme)
  "获取指定主题的配置。"
  (alist-get theme my/org-latex-themes))

(defun my/switch-latex-theme (theme)
  "切换到指定的 LaTeX 主题。"
  (interactive 
   (list (intern (completing-read 
                  "选择主题: " 
                  (mapcar (lambda (theme) 
                            (format "%s (%s)" 
                                    (symbol-name (car theme))
                                    (plist-get (cdr theme) :description)))
                          my/org-latex-themes)
                  nil t))))
  
  (let ((config (my/get-theme-config theme)))
    (if config
        (progn
          (setq my/current-latex-theme theme)
          (my/update-latex-classes)
          (my/update-pdf-process theme)
          (message "已切换到 %s 主题" (plist-get config :name)))
      (error "未找到主题: %s" theme))))

(defun my/update-pdf-process (theme)
  "根据主题更新 PDF 处理流程。"
  (let* ((config (my/get-theme-config theme))
         (has-bib (plist-get config :has-bibliography))
         (process-type (if has-bib 'academic 'standard)))
    (setq org-latex-pdf-process 
          (alist-get process-type my/latex-pdf-processes))))

;; =============================================================================
;; 动态文档类生成

(defun my/generate-latex-class (theme-config)
  "根据主题配置生成 LaTeX 文档类。"
  (let* ((class-name (plist-get theme-config :class))
         (packages (plist-get theme-config :packages))
         (geometry (plist-get theme-config :geometry))
         (linespread (plist-get theme-config :linespread))
         (colors (plist-get theme-config :colors)))
    
    (concat
     (format "\\documentclass[11pt]{%s}\n" class-name)
     ;; 改进的中文字体配置
     (when (string-match "ctex" class-name)
       (concat "\\usepackage[UTF8,fontset=fandol]{ctex}\n"
               "% 字体配置\n"
               "\\setCJKmainfont[BoldFont={FandolSong-Bold},ItalicFont={FandolKai-Regular}]{FandolSong-Regular}\n"
               "\\setCJKsansfont{FandolHei-Regular}\n"
               "\\setCJKmonofont{FandolFang-Regular}\n"
               "% 数学字体\n"
               "\\usepackage{unicode-math}\n"
               "\\setmathfont{Latin Modern Math}\n"))
     ;; 对于非 ctex 类，添加基本的 fontspec 支持
     (unless (string-match "ctex" class-name)
       "\\usepackage{fontspec}\n\\setmainfont{Latin Modern Roman}\n")
     (mapconcat (lambda (pkg) (format "\\usepackage{%s}\n" pkg)) packages "")
     (when geometry (format "\\usepackage[%s]{geometry}\n" geometry))
     (when linespread (format "\\linespread{%s}\n" linespread))
     "\\setlength{\\parskip}{0.5em}\n"
     "\\usepackage{titlesec}\n"
     "\\titlespacing*{\\section}{0pt}{1.5em}{1em}\n"
     "\\titlespacing*{\\subsection}{0pt}{1.25em}{0.75em}\n"
     "\\titlespacing*{\\subsubsection}{0pt}{1em}{0.5em}\n")))

(defun my/update-latex-classes ()
  "更新所有主题的 LaTeX 文档类。"
  (dolist (theme-entry my/org-latex-themes)
    (let* ((theme-name (car theme-entry))
           (theme-config (cdr theme-entry))
           (class-name (format "%s-theme" (symbol-name theme-name)))
           (class-definition (my/generate-latex-class theme-config)))
      
      ;; 移除现有定义
      (setq org-latex-classes 
            (assq-delete-all class-name org-latex-classes))
      
      ;; 添加新定义
      (add-to-list 'org-latex-classes
                   `(,class-name
                     ,class-definition
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))))

;; =============================================================================
;; 智能导出函数

(defun my/smart-latex-export ()
  "智能 LaTeX 导出，根据文档内容自动选择主题。"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((has-bibliography (re-search-forward "bibliography:" nil t))
          (has-citations (re-search-forward "cite:" nil t)))
      
      (cond
       ((or has-bibliography has-citations)
        (my/switch-latex-theme 'academic)
        (message "检测到参考文献，使用学术主题"))
       (t
        (my/switch-latex-theme my/current-latex-theme)
        (message "使用当前主题: %s" my/current-latex-theme))))
    
    ;; 执行导出
    (my/safe-latex-export-to-pdf)))

(defun my/safe-latex-export-to-pdf ()
  "安全的 LaTeX 到 PDF 导出函数。"
  (interactive)
  (let ((original-file (buffer-file-name)))
    (unless original-file
      (error "请先保存文件再导出 PDF"))
    
    (condition-case err
        (progn
          ;; 确保使用当前主题的类
          (my/ensure-latex-class)
          ;; 执行导出
          (org-latex-export-to-pdf))
      (error
       (message "PDF 导出失败: %s" (error-message-string err))
       (my/show-export-help)
       nil))))

(defun my/ensure-latex-class ()
  "确保文档使用正确的 LaTeX 类。"
  (save-excursion
    (goto-char (point-min))
    (let ((theme-class (format "%s-theme" (symbol-name my/current-latex-theme))))
      (unless (re-search-forward "^#\\+LATEX_CLASS:" nil t)
        (goto-char (point-min))
        (insert (format "#+LATEX_CLASS: %s\n\n" theme-class))))))

;; =============================================================================
;; 帮助和诊断

(defun my/show-export-help ()
  "显示导出帮助信息。"
  (message "导出失败，可以尝试：")
  (message "1. C-c t d - 运行诊断")
  (message "2. C-c t f - 修复常见问题") 
  (message "3. C-c t s - 切换主题"))

(defun my/diagnose-latex-environment ()
  "诊断 LaTeX 环境。"
  (interactive)
  (let ((diagnosis-buffer (get-buffer-create "*LaTeX 诊断*")))
    (with-current-buffer diagnosis-buffer
      (erase-buffer)
      (insert "=== LaTeX 环境诊断 ===\n\n")
      
      ;; 当前主题信息
      (let ((config (my/get-theme-config my/current-latex-theme)))
        (insert (format "当前主题: %s (%s)\n" 
                        (plist-get config :name)
                        (plist-get config :description))))
      
      ;; LaTeX 程序检查
      (insert "\nLaTeX 程序检查:\n")
      (dolist (prog '("xelatex" "pdflatex" "bibtex"))
        (let ((path (executable-find prog)))
          (insert (format "  %s %s: %s\n" 
                          (if path "✅" "❌")
                          prog 
                          (or path "未找到")))))
      
      ;; 文档检查
      (insert "\n文档检查:\n")
      (insert (format "  文件: %s\n" (or (buffer-file-name) "未保存")))
      (insert (format "  模式: %s\n" major-mode))
      
      (goto-char (point-min)))
    (display-buffer diagnosis-buffer)))

;; =============================================================================
;; 字体诊断和修复

(defun my/diagnose-fonts ()
  "诊断系统字体配置。"
  (interactive)
  (let ((diagnosis-buffer (get-buffer-create "*字体诊断*")))
    (with-current-buffer diagnosis-buffer
      (erase-buffer)
      (insert "=== 字体环境诊断 ===\n\n")
      
      ;; 检查系统字体
      (insert "系统字体检查:\n")
      (let ((fonts '("FandolSong-Regular" "SimSun" "Arial Unicode MS" "DejaVu Sans")))
        (dolist (font fonts)
          (let ((available (shell-command-to-string 
                           (format "fc-list | grep -i '%s'" font))))
            (if (string-empty-p available)
                (insert (format "  ❌ %s: 未找到\n" font))
              (insert (format "  ✅ %s: 可用\n" font))))))
      
      ;; 检查 LaTeX 包
      (insert "\nLaTeX 包检查:\n")
      (let ((packages '("ctex" "fontspec" "unicode-math")))
        (dolist (pkg packages)
          (let ((result (shell-command-to-string 
                        (format "kpsewhich %s.sty" pkg))))
            (if (string-empty-p result)
                (insert (format "  ❌ %s: 未安装\n" pkg))
              (insert (format "  ✅ %s: 已安装\n" pkg))))))
      
      ;; 建议
      (insert "\n修复建议:\n")
      (insert "1. 安装中文字体: brew install font-fandol\n")
      (insert "2. 更新 LaTeX 包: tlmgr update --all\n")
      (insert "3. 使用 C-c t f 修复字体配置\n")
      
      (goto-char (point-min)))
    (display-buffer diagnosis-buffer)))

(defun my/fix-font-issues ()
  "尝试修复常见的字体问题。"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((fixes-applied 0))
      
      ;; 检查是否有 LATEX_CLASS 设置
      (unless (re-search-forward "^#\\+LATEX_CLASS:" nil t)
        (goto-char (point-min))
        (insert "#+LATEX_CLASS: standard-theme\n")
        (setq fixes-applied (1+ fixes-applied)))
      
      ;; 添加字体配置选项
      (goto-char (point-min))
      (unless (re-search-forward "^#\\+LATEX_HEADER:.*fontset" nil t)
        (goto-char (point-min))
        (when (re-search-forward "^#\\+LATEX_CLASS:" nil t)
          (end-of-line)
          (newline)
          (insert "#+LATEX_HEADER: \\usepackage[UTF8,fontset=fandol]{ctex}\n")
          (setq fixes-applied (1+ fixes-applied))))
      
      (message "应用了 %d 个字体修复" fixes-applied)
      (when (> fixes-applied 0)
        (message "建议使用 C-c t p 重新导出 PDF")))))

;; =============================================================================
;; 快捷键绑定

(global-set-key (kbd "C-c t s") 'my/switch-latex-theme)
(global-set-key (kbd "C-c t e") 'my/smart-latex-export)
(global-set-key (kbd "C-c t p") 'my/safe-latex-export-to-pdf)
(global-set-key (kbd "C-c t d") 'my/diagnose-latex-environment)
(global-set-key (kbd "C-c t f") 'my/fix-font-issues)
(global-set-key (kbd "C-c t F") 'my/diagnose-fonts)

;; =============================================================================
;; 初始化

(with-eval-after-load 'ox-latex
  ;; 初始化主题系统
  (my/update-latex-classes)
  (my/update-pdf-process my/current-latex-theme)
  
  ;; 显示欢迎信息
  (message "LaTeX 主题系统已加载，使用 C-c t s 切换主题"))

(provide 'org-latex)

;;; org-latex.el ends here
