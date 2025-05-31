;;; latex-themes.el --- LaTeX 主题管理器 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (auctex "14.0"))

;;; Commentary:

;; LaTeX 主题管理系统，支持：
;; 1. 预定义主题模板
;; 2. 自定义主题创建
;; 3. 主题快速切换

;;; Code:

;; =============================================================================
;; 主题定义

(defvar latex-themes-directory
  (expand-file-name "lisp/writing/LaTeX/themes/" user-emacs-directory)
  "LaTeX 主题文件存储目录")

(defvar latex-themes-alist
  '(("academic" . 
     "% Academic Theme
\\usepackage[margin=1in]{geometry}
\\usepackage{setspace}
\\onehalfspacing
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\fancyhf{}
\\rhead{\\thepage}
\\lhead{\\leftmark}")
    
    ("minimal" . 
     "% Minimal Theme  
\\usepackage[margin=1.2in]{geometry}
\\usepackage{microtype}
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{6pt plus 2pt minus 1pt}")
    
    ("elegant" .
     "% Elegant Theme
\\usepackage[margin=1in]{geometry}
\\usepackage{palatino}
\\usepackage{microtype}
\\usepackage{titlesec}
\\titleformat{\\section}{\\Large\\bfseries}{\\thesection}{1em}{}
\\titleformat{\\subsection}{\\large\\bfseries}{\\thesubsection}{1em}{}")
    
    ("modern" .
     "% Modern Theme
\\usepackage[margin=1in]{geometry}
\\usepackage{fontspec}
\\setmainfont{Helvetica Neue}
\\usepackage{xcolor}
\\definecolor{modernblue}{RGB}{0,123,191}
\\usepackage{titlesec}
\\titleformat{\\section}{\\Large\\bfseries\\color{modernblue}}{\\thesection}{1em}{}"))
  "预定义的 LaTeX 主题")

;; =============================================================================
;; 主题管理函数

(defun latex-themes-ensure-directory ()
  "确保主题目录存在"
  (unless (file-exists-p latex-themes-directory)
    (make-directory latex-themes-directory t)))

(defun latex-themes-list-available ()
  "列出所有可用主题"
  (interactive)
  (latex-themes-ensure-directory)
  (let ((predefined (mapcar 'car latex-themes-alist))
        (custom (when (file-exists-p latex-themes-directory)
                  (mapcar (lambda (f) 
                            (file-name-sans-extension f))
                          (directory-files latex-themes-directory nil "\\.sty$")))))
    (append predefined custom)))

(defun latex-themes-insert-theme ()
  "插入选择的主题到当前文档"
  (interactive)
  (let* ((available-themes (latex-themes-list-available))
         (theme-name (completing-read "选择主题: " available-themes))
         (theme-content (or (cdr (assoc theme-name latex-themes-alist))
                           (latex-themes-load-custom-theme theme-name))))
    (when theme-content
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "\\\\documentclass.*" nil t)
          (end-of-line)
          (insert "\n\n" theme-content "\n"))))))

(defun latex-themes-load-custom-theme (theme-name)
  "加载自定义主题文件"
  (let ((theme-file (expand-file-name (concat theme-name ".sty") latex-themes-directory)))
    (when (file-exists-p theme-file)
      (with-temp-buffer
        (insert-file-contents theme-file)
        (buffer-string)))))

(defun latex-themes-create-custom-theme ()
  "创建自定义主题"
  (interactive)
  (latex-themes-ensure-directory)
  (let* ((theme-name (read-string "主题名称: "))
         (theme-file (expand-file-name (concat theme-name ".sty") latex-themes-directory))
         (theme-content (read-string "主题内容 (LaTeX 包和设置): ")))
    (with-temp-file theme-file
      (insert (format "%% Custom LaTeX Theme: %s\n%% Created: %s\n\n%s\n"
                     theme-name
                     (format-time-string "%Y-%m-%d %H:%M:%S")
                     theme-content)))
    (message "主题 '%s' 已创建: %s" theme-name theme-file)))

(defun latex-themes-edit-custom-theme ()
  "编辑自定义主题"
  (interactive)
  (latex-themes-ensure-directory)
  (let* ((custom-themes (mapcar (lambda (f) 
                                 (file-name-sans-extension f))
                               (directory-files latex-themes-directory nil "\\.sty$")))
         (theme-name (completing-read "选择要编辑的主题: " custom-themes))
         (theme-file (expand-file-name (concat theme-name ".sty") latex-themes-directory)))
    (if (file-exists-p theme-file)
        (find-file theme-file)
      (message "主题文件不存在: %s" theme-file))))

;; =============================================================================
;; 主题预览功能

(defun latex-themes-preview-theme ()
  "预览主题效果"
  (interactive)
  (let* ((available-themes (latex-themes-list-available))
         (theme-name (completing-read "选择要预览的主题: " available-themes))
         (theme-content (or (cdr (assoc theme-name latex-themes-alist))
                           (latex-themes-load-custom-theme theme-name)))
         (preview-buffer "*LaTeX Theme Preview*"))
    (when theme-content
      (with-output-to-temp-buffer preview-buffer
        (princ (format "主题: %s\n\n" theme-name))
        (princ theme-content)))))

;; =============================================================================
;; 快捷键绑定

(defun latex-themes-setup-keybindings ()
  "设置主题相关快捷键"
  (local-set-key (kbd "C-c t i") 'latex-themes-insert-theme)
  (local-set-key (kbd "C-c t c") 'latex-themes-create-custom-theme)
  (local-set-key (kbd "C-c t e") 'latex-themes-edit-custom-theme)
  (local-set-key (kbd "C-c t p") 'latex-themes-preview-theme)
  (local-set-key (kbd "C-c t l") 'latex-themes-list-available))

;; 在 LaTeX 模式中自动设置主题快捷键
(add-hook 'LaTeX-mode-hook #'latex-themes-setup-keybindings)

;; =============================================================================
;; 初始化默认主题

(defun latex-themes-create-default-themes ()
  "创建默认主题文件"
  (latex-themes-ensure-directory)
  (dolist (theme latex-themes-alist)
    (let ((theme-file (expand-file-name (concat (car theme) ".sty") latex-themes-directory)))
      (unless (file-exists-p theme-file)
        (with-temp-file theme-file
          (insert (cdr theme)))))))

;; 首次加载时创建默认主题
(latex-themes-create-default-themes)

(provide 'latex-themes)

;;; latex-themes.el ends here
