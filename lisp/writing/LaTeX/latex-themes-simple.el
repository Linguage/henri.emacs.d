;;; latex-themes-simple.el --- 简化的 LaTeX 主题管理 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; 简化的 LaTeX 主题管理，专门服务于 Org-mode PDF 导出
;; 只保留核心主题定义和基本管理功能

;;; Code:

;; =============================================================================
;; 核心主题定义

(defvar latex-themes-simple-alist
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
\\usepackage{microtype}
\\usepackage{titlesec}
\\titleformat{\\section}{\\Large\\bfseries}{\\thesection}{1em}{}
\\titleformat{\\subsection}{\\large\\bfseries}{\\thesubsection}{1em}{}")
    
    ("modern" .
     "% Modern Theme
\\usepackage[margin=1in]{geometry}
\\usepackage{xcolor}
\\definecolor{modernblue}{RGB}{0,123,191}
\\usepackage{titlesec}
\\titleformat{\\section}{\\Large\\bfseries\\color{modernblue}}{\\thesection}{1em}{}"))
  "简化的 LaTeX 主题定义")

;; =============================================================================
;; 主题文件生成

(defun latex-themes-simple-ensure-directory ()
  "确保主题目录存在"
  (let ((themes-dir (expand-file-name "lisp/writing/LaTeX/themes/" user-emacs-directory)))
    (unless (file-exists-p themes-dir)
      (make-directory themes-dir t))
    themes-dir))

(defun latex-themes-simple-generate-files ()
  "生成所有主题的 .sty 文件"
  (let ((themes-dir (latex-themes-simple-ensure-directory)))
    (dolist (theme latex-themes-simple-alist)
      (let ((theme-name (car theme))
            (theme-content (cdr theme)))
        (with-temp-file (expand-file-name (concat theme-name ".sty") themes-dir)
          (insert theme-content))))
    (message "LaTeX 主题文件已生成到: %s" themes-dir)))

;; =============================================================================
;; 初始化

;; 自动生成主题文件
(latex-themes-simple-generate-files)

(provide 'latex-themes-simple)

;;; latex-themes-simple.el ends here
