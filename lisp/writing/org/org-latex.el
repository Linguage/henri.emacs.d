;;; org-latex.el --- Org Mode LaTeX 导出配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Keywords: org, latex, export

;;; Commentary:

;; Org Mode LaTeX 导出配置，包含：
;; - LaTeX 编译器设置
;; - 中文支持配置
;; - 文档类定义
;; - 导出处理钩子

;;; Code:

;; =============================================================================
;; LaTeX 编译配置

;; 设置 LaTeX 编译器
(setq org-latex-compiler "xelatex")

;; 设置 PDF 处理流程
(setq org-latex-pdf-process
      '("rm -f %b.aux %b.log %b.out %b.toc %b.bbl %b.blg"  ; 先清理临时文件
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "rm -f %b.aux %b.log %b.out %b.toc %b.bbl %b.blg")) ; 编译后再清理

;; =============================================================================
;; 导出处理钩子

(defun my/org-export-add-latex-class (_backend)
  "在导出为 LaTeX/PDF 之前自动添加 #+LATEX_CLASS: ctexart.
_BACKEND 是导出后端，由钩子提供但在此函数中未使用。"
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^#\\+LATEX_CLASS: ctexart" nil t)
      (insert "#+LATEX_CLASS: ctexart\n\n"))))

(add-hook 'org-export-before-processing-hook 'my/org-export-add-latex-class)

;; =============================================================================
;; 中文文档类配置

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("ctexart"
                 "\\documentclass[11pt]{ctexart}
\\usepackage[UTF8]{ctex}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{graphicx}
\\usepackage{hyperref}
% 设置页边距
\\usepackage[top=2.5cm, bottom=2.5cm, left=2cm, right=2cm]{geometry}
% 设置行间距
\\linespread{1.15}
% 设置段落间距
\\setlength{\\parskip}{0.5em}
% 设置章节标题与正文间距
\\usepackage{titlesec}
\\titlespacing*{\\section}{0pt}{1.5em}{1em}
\\titlespacing*{\\subsection}{0pt}{1.25em}{0.75em}
\\titlespacing*{\\subsubsection}{0pt}{1em}{0.5em}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(provide 'org-latex)

;;; org-latex.el ends here
