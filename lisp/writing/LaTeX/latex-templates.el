;;; latex-templates.el --- LaTeX 模板和快速插入 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (auctex "14.0"))

;;; Commentary:

;; LaTeX 模板管理和快速插入功能

;;; Code:

;; =============================================================================
;; LaTeX 文档模板

(defvar latex-templates-alist
  '(("article" . "\\documentclass{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{amsmath,amsfonts,amssymb}
\\usepackage{geometry}
\\usepackage{graphicx}
\\usepackage{hyperref}

\\title{%s}
\\author{Henri}
\\date{\\today}

\\begin{document}
\\maketitle

%s

\\end{document}")
    
    ("beamer" . "\\documentclass{beamer}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{amsmath,amsfonts,amssymb}
\\usepackage{graphicx}

\\title{%s}
\\author{Henri}
\\date{\\today}

\\begin{document}

\\frame{\\titlepage}

\\begin{frame}
\\frametitle{Outline}
\\tableofcontents
\\end{frame}

%s

\\end{document}")
    
    ("report" . "\\documentclass[12pt,a4paper]{report}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{amsmath,amsfonts,amssymb}
\\usepackage{geometry}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\usepackage{fancyhdr}

\\title{%s}
\\author{Henri}
\\date{\\today}

\\begin{document}
\\maketitle
\\tableofcontents

\\chapter{Introduction}
%s

\\end{document}"))
  "LaTeX 文档模板列表")

(defun latex-insert-template ()
  "插入 LaTeX 文档模板"
  (interactive)
  (let* ((template-type (completing-read "选择模板类型: " 
                                        (mapcar 'car latex-templates-alist)))
         (title (read-string "文档标题: "))
         (template (cdr (assoc template-type latex-templates-alist))))
    (insert (format template title ""))
    (search-backward "\n\n\\end{document}")
    (forward-line -1)
    (end-of-line)))

;; =============================================================================
;; LaTeX 快捷输入函数

(defun latex-insert-equation ()
  "插入数学公式环境"
  (interactive)
  (insert "\\begin{equation}\n\\label{eq:}\n\n\\end{equation}")
  (search-backward "eq:")
  (forward-char 3))

(defun latex-insert-figure ()
  "插入图片环境"
  (interactive)
  (let ((caption (read-string "图片标题: "))
        (label (read-string "图片标签: "))
        (filename (read-string "图片文件名: ")))
    (insert (format "\\begin{figure}[htbp]
\\centering
\\includegraphics[width=0.8\\textwidth]{%s}
\\caption{%s}
\\label{fig:%s}
\\end{figure}" filename caption label))))

(defun latex-insert-table ()
  "插入表格环境"
  (interactive)
  (let ((caption (read-string "表格标题: "))
        (label (read-string "表格标签: "))
        (columns (read-number "列数: " 3)))
    (insert (format "\\begin{table}[htbp]
\\centering
\\caption{%s}
\\label{tab:%s}
\\begin{tabular}{%s}
\\hline
%s
\\hline
\\end{tabular}
\\end{table}" 
                    caption 
                    label 
                    (make-string columns ?c)
                    (mapconcat (lambda (i) " & ") (number-sequence 1 columns) "")))))

;; =============================================================================
;; 预览优化函数

(defun latex-preview-math-at-point ()
  "预览光标处的数学公式"
  (interactive)
  (if (texmathp)
      (preview-at-point)
    (message "光标不在数学环境中")))

(defun latex-toggle-preview-all ()
  "切换整个文档的预览"
  (interactive)
  (if (get-buffer-window "*preview*")
      (preview-clearout-buffer)
    (preview-buffer)))

;; =============================================================================
;; 编译和查看函数

(defun latex-compile-and-view ()
  "编译并查看 PDF"
  (interactive)
  (save-buffer)
  (TeX-command-run-all nil))

(defun latex-word-count ()
  "统计 LaTeX 文档字数"
  (interactive)
  (shell-command (format "detex %s | wc -w" (buffer-file-name))))

(provide 'latex-templates)

;;; latex-templates.el ends here
