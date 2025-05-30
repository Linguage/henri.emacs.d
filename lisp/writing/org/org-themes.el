;;; org-themes.el --- Org Mode LaTeX 主题库 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Keywords: org, latex, themes

;;; Commentary:

;; Org Mode LaTeX 主题库，提供各种预定义主题

;;; Code:

;; =============================================================================
;; 扩展主题定义

(defvar my/org-latex-extended-themes
  '((elegant
     :name "优雅主题"
     :class "ctexart"
     :packages ("amsmath" "amssymb" "graphicx" "xcolor" "fancyhdr" "hyperref")
     :geometry "top=2.5cm, bottom=2.5cm, left=2.5cm, right=2.5cm"
     :linespread 1.2
     :header-footer t
     :description "优雅的文档布局，带页眉页脚")
    
    (presentation
     :name "演示主题"
     :class "beamer"
     :packages ("amsmath" "amssymb" "graphicx" "xcolor")
     :theme "Madrid"
     :description "Beamer 演示文稿主题")
    
    (book
     :name "书籍主题"
     :class "ctexbook"
     :packages ("amsmath" "amssymb" "graphicx" "fancyhdr" "titlesec" "hyperref")
     :geometry "top=2.5cm, bottom=2.5cm, left=2.5cm, right=2.5cm"
     :linespread 1.15
     :chapters t
     :description "适合长文档和书籍的主题")
    
    (report
     :name "报告主题"
     :class "ctexrep"
     :packages ("amsmath" "amssymb" "graphicx" "booktabs" "longtable" "hyperref")
     :geometry "top=2.5cm, bottom=2.5cm, left=2.5cm, right=2.5cm"
     :linespread 1.1
     :description "技术报告和项目文档主题"))
  "扩展的 LaTeX 主题配置。")

;; =============================================================================
;; 字体安全的主题定义

(defvar my/org-latex-font-safe-themes
  '((safe-chinese
     :name "安全中文主题"
     :class "ctexart"
     :packages ("amsmath" "amssymb" "graphicx" "hyperref")
     :geometry "top=2.5cm, bottom=2.5cm, left=2cm, right=2cm"
     :linespread 1.15
     :fontset "fandol"
     :description "使用 Fandol 字体的安全中文主题")
    
    (universal
     :name "通用主题"
     :class "article"
     :packages ("amsmath" "amssymb" "graphicx" "fontspec" "hyperref")
     :geometry "top=2.5cm, bottom=2.5cm, left=2cm, right=2cm"
     :linespread 1.15
     :fonts "Latin Modern"
     :description "使用 Latin Modern 字体的通用主题")
    
    (simple
     :name "简单主题"
     :class "article"
     :packages ("amsmath" "graphicx")
     :geometry "top=3cm, bottom=3cm, left=3cm, right=3cm"
     :linespread 1.0
     :description "最简单的主题，最少依赖"))
  "字体安全的 LaTeX 主题配置。")

;; =============================================================================
;; 主题预览和选择

(defun my/preview-theme (theme)
  "预览指定主题的配置。"
  (interactive 
   (list (intern (completing-read "预览主题: " 
                                  (append my/org-latex-themes 
                                          my/org-latex-extended-themes)))))
  (let ((config (or (alist-get theme my/org-latex-themes)
                    (alist-get theme my/org-latex-extended-themes))))
    (if config
        (let ((preview-buffer (get-buffer-create "*主题预览*")))
          (with-current-buffer preview-buffer
            (erase-buffer)
            (insert (format "=== %s 主题预览 ===\n\n" (plist-get config :name)))
            (insert (format "描述: %s\n" (plist-get config :description)))
            (insert (format "文档类: %s\n" (plist-get config :class)))
            (insert (format "包列表: %s\n" (mapconcat 'identity (plist-get config :packages) ", ")))
            (insert (format "页面布局: %s\n" (or (plist-get config :geometry) "默认")))
            (insert (format "行距: %s\n" (or (plist-get config :linespread) "1.0")))
            (when (plist-get config :has-bibliography)
              (insert "支持参考文献: 是\n"))
            (when (plist-get config :colors)
              (insert "彩色支持: 是\n"))
            (goto-char (point-min)))
          (display-buffer preview-buffer))
      (message "未找到主题: %s" theme))))

;; =============================================================================
;; 主题管理界面

(defun my/theme-manager ()
  "打开主题管理界面。"
  (interactive)
  (let ((manager-buffer (get-buffer-create "*主题管理器*")))
    (with-current-buffer manager-buffer
      (erase-buffer)
      (insert "=== Org LaTeX 主题管理器 ===\n\n")
      (insert (format "当前主题: %s\n\n" my/current-latex-theme))
      
      (insert "可用主题:\n")
      (dolist (theme-entry (append my/org-latex-themes my/org-latex-extended-themes))
        (let* ((theme-name (car theme-entry))
               (config (cdr theme-entry))
               (current-mark (if (eq theme-name my/current-latex-theme) " [当前]" "")))
          (insert (format "  %s - %s%s\n" 
                          theme-name 
                          (plist-get config :description)
                          current-mark))))
      
      (insert "\n快捷键:\n")
      (insert "  s - 切换主题\n")
      (insert "  p - 预览主题\n")
      (insert "  q - 退出\n")
      
      (goto-char (point-min))
      (local-set-key (kbd "s") 'my/switch-latex-theme)
      (local-set-key (kbd "p") 'my/preview-theme)
      (local-set-key (kbd "q") 'quit-window))
    (display-buffer manager-buffer)))

;; =============================================================================
;; 主题生成函数

(defun my/generate-font-safe-class (theme-config)
  "生成字体安全的 LaTeX 文档类。"
  (let* ((class-name (plist-get theme-config :class))
         (packages (plist-get theme-config :packages))
         (geometry (plist-get theme-config :geometry))
         (linespread (plist-get theme-config :linespread))
         (fontset (plist-get theme-config :fontset))
         (fonts (plist-get theme-config :fonts)))
    
    (concat
     (format "\\documentclass[11pt]{%s}\n" class-name)
     ;; 字体配置
     (cond
      ((string= class-name "ctexart")
       (if fontset
           (format "\\usepackage[UTF8,fontset=%s]{ctex}\n" fontset)
         "\\usepackage[UTF8,fontset=fandol]{ctex}\n"))
      (fonts
       (format "\\usepackage{fontspec}\n\\setmainfont{%s}\n" fonts))
      (t ""))
     ;; 包配置
     (mapconcat (lambda (pkg) (format "\\usepackage{%s}\n" pkg)) packages "")
     ;; 页面配置
     (when geometry (format "\\usepackage[%s]{geometry}\n" geometry))
     (when linespread (format "\\linespread{%s}\n" linespread))
     "\\setlength{\\parskip}{0.5em}\n")))

;; =============================================================================
;; 主题切换函数

(defun my/switch-to-safe-theme ()
  "切换到字体安全的主题。"
  (interactive)
  (let ((theme (intern (completing-read 
                        "选择安全主题: " 
                        (mapcar (lambda (theme) 
                                  (format "%s (%s)" 
                                          (symbol-name (car theme))
                                          (plist-get (cdr theme) :description)))
                                my/org-latex-font-safe-themes)
                        nil t))))
    (my/apply-safe-theme theme)))

(defun my/apply-safe-theme (theme)
  "应用字体安全的主题。"
  (let ((config (alist-get theme my/org-latex-font-safe-themes)))
    (when config
      ;; 生成文档类
      (let ((class-name (format "%s-safe" (symbol-name theme)))
            (class-definition (my/generate-font-safe-class config)))
        
        ;; 更新 org-latex-classes
        (setq org-latex-classes 
              (assq-delete-all class-name org-latex-classes))
        (add-to-list 'org-latex-classes
                     `(,class-name
                       ,class-definition
                       ("\\section{%s}" . "\\section*{%s}")
                       ("\\subsection{%s}" . "\\subsection*{%s}")
                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
        
        ;; 设置 PDF 处理流程
        (setq org-latex-pdf-process
              '("xelatex -interaction nonstopmode -output-directory %o %f"
                "xelatex -interaction nonstopmode -output-directory %o %f"))
        
        (message "已切换到 %s 主题" (plist-get config :name))))))

;; =============================================================================
;; 快速修复函数

(defun my/create-minimal-test-document ()
  "创建一个最小的测试文档。"
  (interactive)
  (let ((test-file (expand-file-name "~/Documents/font-test.org")))
    (with-temp-file test-file
      (insert "#+TITLE: 字体测试文档
#+AUTHOR: Henri
#+LATEX_CLASS: simple-safe
#+OPTIONS: toc:nil

* 测试标题

这是一个字体测试文档。

** 英文测试
Hello World! This is English text.

** 数学测试
$E = mc^2$

** 列表测试
- 项目 1
- 项目 2
- 项目 3
"))
    (find-file test-file)
    (message "已创建字体测试文档: %s" test-file)))

;; =============================================================================
;; 快捷键

(global-set-key (kbd "C-c t m") 'my/theme-manager)
(global-set-key (kbd "C-c t v") 'my/preview-theme)
(global-set-key (kbd "C-c t S") 'my/switch-to-safe-theme)
(global-set-key (kbd "C-c t T") 'my/create-minimal-test-document)

;; =============================================================================
;; 初始化

(defun my/init-safe-themes ()
  "初始化字体安全主题。"
  (dolist (theme-entry my/org-latex-font-safe-themes)
    (let ((theme-name (car theme-entry)))
      (my/apply-safe-theme theme-name))))

;; 自动初始化
(with-eval-after-load 'ox-latex
  (my/init-safe-themes))

(provide 'org-themes)

;;; org-themes.el ends here
