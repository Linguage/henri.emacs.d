;;; org-latex.el --- Org Mode LaTeX/PDF 导出统一配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 3.0 (合并版本)
;; Keywords: org, latex, pdf, export

;;; Commentary:

;; Org Mode LaTeX/PDF 导出统一配置，包含：
;; - LaTeX 编译器和文档类配置
;; - PDF 工具和自动打开
;; - 主题系统支持
;; - 字体诊断和修复工具
;; - 导出快捷功能

;;; Code:

;; =============================================================================
;; 核心 LaTeX 配置

(require 'ox-latex)

;; 立即设置编译器（不等待 eval-after-load）
(setq org-latex-compiler "xelatex")
(setq org-latex-prefer-user-labels t)

;; PDF 处理流程
(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))

;; 默认包配置（支持中文）
(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t ("pdflatex"))
        ("T1" "fontenc" t ("pdflatex"))
        ("" "graphicx" t)
        ("" "grffile" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("" "hyperref" nil)))

;; =============================================================================
;; LaTeX 主题系统

(defvar org-latex-themes-directory
  (expand-file-name "lisp/writing/LaTeX/themes/" user-emacs-directory)
  "LaTeX 主题文件目录")

(defvar org-latex-themes-alist
  '(("default" . nil)
    ("academic" . "academic")
    ("minimal" . "minimal") 
    ("elegant" . "elegant")
    ("modern" . "modern"))
  "可用的 LaTeX 主题列表")

(defun org-latex-load-theme-content (theme-name)
  "加载指定主题的 LaTeX 内容"
  (when theme-name
    (let ((theme-file (expand-file-name (concat theme-name ".sty") org-latex-themes-directory)))
      (when (file-exists-p theme-file)
        (with-temp-buffer
          (insert-file-contents theme-file)
          (buffer-string))))))

(defun org-latex-apply-theme (theme-name)
  "为当前 org 缓冲区应用 LaTeX 主题"
  (interactive 
   (list (completing-read "选择 LaTeX 主题: " 
                         (mapcar 'car org-latex-themes-alist))))
  (let ((theme-content (org-latex-load-theme-content theme-name)))
    (if theme-content
        (progn
          ;; 设置局部变量，在导出时使用
          (setq-local org-latex-theme-content theme-content)
          (message "已应用主题: %s" theme-name))
      (progn
        (setq-local org-latex-theme-content nil)
        (message "使用默认主题")))))

(defun org-latex-get-current-theme-header ()
  "获取当前主题的 LaTeX 头部内容"
  (when (bound-and-true-p org-latex-theme-content)
    org-latex-theme-content))

;; =============================================================================
;; PDF 工具配置

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  ;; 设置 PDF-tools 兼容性
  ;; 禁用 PDF 查看模式下的行号显示
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (display-line-numbers-mode -1))))

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;; 修改 org-open-pdf-after-export 函数确保使用 pdf-tools
(defun org-open-pdf-after-export (backend)
  "Open the generated PDF file in pdf-tools after org-mode export."
  (when (eq backend 'latex)
    (let ((pdf-file (concat (file-name-sans-extension buffer-file-name) ".pdf")))
      (when (file-exists-p pdf-file)
        (find-file pdf-file)
        ;; 确保在此处关闭行号显示
        (when (bound-and-true-p display-line-numbers-mode)
          (display-line-numbers-mode -1))))))

(add-hook 'org-export-after-processing-hook 'org-open-pdf-after-export)

;; =============================================================================
;; 文档类和配置（延迟加载）

(with-eval-after-load 'ox-latex
  
  ;; 中文支持包
  (add-to-list 'org-latex-packages-alist '("UTF8" "ctex" t))
  
  ;; 代码块支持
  (add-to-list 'org-latex-packages-alist '("" "listings" nil))
  (add-to-list 'org-latex-packages-alist '("" "xcolor" nil))
  
  ;; 添加基础的中文文档类
  (add-to-list 'org-latex-classes
               '("ctexart"
                 "\\documentclass[11pt]{ctexart}
\\usepackage[UTF8,fontset=fandol]{ctex}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\usepackage[top=2cm, bottom=2cm, left=1.5cm, right=1.5cm]{geometry}
\\linespread{1.15}
\\setlength{\\parskip}{0.5em}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  
  ;; 添加更安全的英文文档类（避免字体问题）
  (add-to-list 'org-latex-classes
               '("article-safe"
                 "\\documentclass[11pt]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\usepackage[top=2cm, bottom=2cm, left=1.5cm, right=1.5cm]{geometry}
\\linespread{1.15}
\\setlength{\\parskip}{0.5em}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; 添加专门的期刊文档类（1cm页边距）
  (add-to-list 'org-latex-classes
               '("journal"
                 "\\documentclass[11pt]{article}
\\usepackage[UTF8,fontset=fandol]{ctex}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\usepackage[top=2cm, bottom=2cm, left=1.5cm, right=1.5cm]{geometry}
\\linespread{1.15}
\\setlength{\\parskip}{0.5em}
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\fancyhf{}
\\rhead{\\thepage}
\\lhead{日志}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; =============================================================================
  ;; 自定义 LaTeX 类（主题支持）

  (defun org-latex-create-class-with-theme (class-name theme-content)
    "创建带主题的 LaTeX 类"
    (let ((header (concat "\\documentclass{article}\n"
                         (or theme-content "")
                         "\n[NO-DEFAULT-PACKAGES]\n[PACKAGES]\n[EXTRA]\n")))
      (add-to-list 'org-latex-classes
                   `(,class-name ,header
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

  ;; 创建基础主题类
  (org-latex-create-class-with-theme "article-default" nil)

  ;; 动态创建主题类
  (dolist (theme org-latex-themes-alist)
    (let ((theme-name (car theme))
          (theme-file (cdr theme)))
      (when theme-file
        (let ((theme-content (org-latex-load-theme-content theme-file)))
          (when theme-content
            (org-latex-create-class-with-theme 
             (concat "article-" theme-name) 
             theme-content))))))

  ;; =============================================================================
  ;; 导出辅助函数

  (defun org-export-pdf-with-theme (theme-name)
    "使用指定主题导出 PDF"
    (interactive 
     (list (completing-read "选择主题导出 PDF: " 
                           (mapcar 'car org-latex-themes-alist))))
    (let ((org-latex-classes-backup org-latex-classes))
      (unwind-protect
          (progn
            ;; 临时设置导出类
            (if (string= theme-name "default")
                (setq-local org-latex-default-class "article-default")
              (setq-local org-latex-default-class (concat "article-" theme-name)))
            ;; 执行导出
            (org-latex-export-to-pdf))
        ;; 恢复设置
        (setq org-latex-classes org-latex-classes-backup))))

  (defun org-export-pdf-quick ()
    "快速导出 PDF（使用当前主题或默认主题）"
    (interactive)
    (org-latex-export-to-pdf))

  ;; =============================================================================
  ;; 字体诊断和修复功能
  
  (defun org-latex-diagnose-fonts ()
    "诊断 LaTeX 字体配置问题。"
    (interactive)
    (let ((diagnosis-buffer (get-buffer-create "*LaTeX 字体诊断*")))
      (with-current-buffer diagnosis-buffer
        (erase-buffer)
        (insert "=== LaTeX 字体环境诊断 ===\n\n")
        
        ;; 检查编译器
        (insert (format "当前编译器: %s\n" org-latex-compiler))
        (let ((xelatex-path (executable-find "xelatex")))
          (if xelatex-path
              (insert (format "✅ XeLaTeX 可用: %s\n" xelatex-path))
            (insert "❌ XeLaTeX 未找到\n")))
        
        ;; 检查字体
        (insert "\n字体检查:\n")
        (let ((fonts '("FandolSong" "SimSun" "Arial Unicode MS")))
          (dolist (font fonts)
            (let ((result (shell-command-to-string 
                          (format "fc-list | grep -i '%s' | head -1" font))))
              (if (string-empty-p result)
                  (insert (format "❌ %s: 未找到\n" font))
                (insert (format "✅ %s: 可用\n" font))))))
        
        ;; 文档类检查
        (insert "\n可用文档类:\n")
        (dolist (class '("ctexart" "article-safe" "article-default"))
          (if (assoc class org-latex-classes)
              (insert (format "✅ %s: 已配置\n" class))
            (insert (format "❌ %s: 未配置\n" class))))
        
        ;; 主题检查
        (insert "\n可用主题:\n")
        (dolist (theme org-latex-themes-alist)
          (let ((theme-name (car theme))
                (theme-file (cdr theme)))
            (if theme-file
                (let ((theme-content (org-latex-load-theme-content theme-file)))
                  (if theme-content
                      (insert (format "✅ %s: 主题文件可用\n" theme-name))
                    (insert (format "❌ %s: 主题文件未找到\n" theme-name))))
              (insert (format "✅ %s: 默认主题\n" theme-name)))))
        
        ;; 建议
        (insert "\n修复建议:\n")
        (insert "1. 如果字体报错，尝试: brew install font-fandol\n")
        (insert "2. 使用英文文档类: #+LATEX_CLASS: article-safe\n")
        (insert "3. 使用中文文档类: #+LATEX_CLASS: ctexart\n")
        (insert "4. 重新加载配置: M-x org-latex-reload-config\n")
        (insert "5. 快速导出: C-c l q\n")
        (insert "6. 主题导出: C-c l p\n")
        
        (goto-char (point-min)))
      (display-buffer diagnosis-buffer)))
  
  (defun org-latex-reload-config ()
    "重新加载 LaTeX 配置。"
    (interactive)
    (setq org-latex-compiler "xelatex")
    (setq org-latex-pdf-process
          '("xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f"))
    (message "LaTeX 配置已重新加载，使用 XeLaTeX 编译器"))

  (message "Org LaTeX/PDF 统一配置已加载"))

;; =============================================================================
;; 快捷键设置

(defun org-latex-setup-keybindings ()
  "设置 org-latex 相关快捷键"
  (local-set-key (kbd "C-c l t") 'org-latex-apply-theme)
  (local-set-key (kbd "C-c l p") 'org-export-pdf-with-theme)
  (local-set-key (kbd "C-c l q") 'org-export-pdf-quick)
  (local-set-key (kbd "C-c l d") 'org-latex-diagnose-fonts)
  (local-set-key (kbd "C-c l r") 'org-latex-reload-config))

(add-hook 'org-mode-hook #'org-latex-setup-keybindings)

;; =============================================================================
;; 全局快捷键

(global-set-key (kbd "C-c l d") 'org-latex-diagnose-fonts)
(global-set-key (kbd "C-c l r") 'org-latex-reload-config)

(provide 'org-latex)

;;; org-latex.el ends here
