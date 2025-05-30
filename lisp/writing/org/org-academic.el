;;; org-academic.el --- Org Mode 学术写作配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Keywords: org, academic, citation, bibliography

;;; Commentary:

;; Org Mode 学术写作配置，包含：
;; - 引用管理 (org-ref)
;; - 参考文献配置
;; - 脚注功能
;; - 内部链接增强
;; - 学术文档模板

;; 注意：此模块中的功能目前被注释掉，可以根据需要启用

;;; Code:

;; =============================================================================
;; 学术写作配置（目前被注释掉）

;; 以下配置在原文件中被注释，如需启用请取消注释：

;; ;; 引用管理配置
;; (use-package org-ref
;;   :ensure t
;;   :config
;;   ;; 设置参考文献样式
;;   (setq org-ref-default-bibliography '("~/Documents/bibliography/references.bib")
;;         org-ref-pdf-directory "~/Documents/bibliography/bibtex-pdfs/"
;;         org-ref-bibliography-notes "~/Documents/bibliography/notes.org"
;;         org-ref-cite-format 'natbib)
;;   
;;   ;; 改进引用链接外观
;;   (setq org-ref-label-use-font-lock t
;;         org-ref-cite-use-font-lock t)
;;   
;;   ;; 增强引用插入体验
;;   (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link))

;; ;; 参考文献导出增强
;; (with-eval-after-load 'ox-latex
;;   ;; 添加参考文献支持包
;;   (add-to-list 'org-latex-packages-alist '("" "natbib" t))
;;   (add-to-list 'org-latex-packages-alist '("" "cleveref" t)) ;; 增强内部引用
;;   
;;   ;; 自动添加参考文献列表
;;   (setq org-latex-prefer-user-labels t)
;;   
;;   ;; 允许在生成的PDF中进行交叉引用
;;   (add-to-list 'org-latex-packages-alist '("" "hyperref" t)))

;; ;; 脚注增强配置
;; (with-eval-after-load 'org
;;   ;; 启用脚注功能
;;   (setq org-export-with-footnotes t)
;;   
;;   ;; 使用行内脚注
;;   (setq org-footnote-define-inline t)
;;   
;;   ;; 脚注自动排序
;;   (setq org-footnote-auto-adjust t)
;;   
;;   ;; 设置快捷键
;;   (define-key org-mode-map (kbd "C-c f") 'org-footnote-new))

;; ;; 内部链接增强
;; (with-eval-after-load 'org
;;   ;; 自动为标题创建ID
;;   (use-package org-id
;;     :ensure nil
;;     :config
;;     (setq org-id-link-to-org-use-id 'create-if-interactive)
;;     (setq org-id-track-globally t))
;;   
;;   ;; 启用交叉引用功能
;;   (setq org-latex-hyperref-template 
;;         "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c},\n pdflang={%L},\n colorlinks=true,\n linkcolor=blue,\n citecolor=blue,\n urlcolor=blue}\n")

;;   ;; 便捷地插入内部链接
;;   (defun my/org-insert-internal-link ()
;;     "在当前位置插入到文档中其他标题的链接。"
;;     (interactive)
;;     (let* ((targets (org-map-entries 
;;                     (lambda () 
;;                       (cons 
;;                        (nth 4 (org-heading-components)) 
;;                        (org-id-get-create)))
;;                     t 'file))
;;            (target (completing-read "链接到: " targets))
;;            (id (cdr (assoc target targets))))
;;       (insert (format "[[id:%s][%s]]" id target))))
;;   (define-key org-mode-map (kbd "C-c l") 'my/org-insert-internal-link))

;; ;; 导出增强 - 代码和方程式编号支持
;; (with-eval-after-load 'ox-latex
;;   ;; 代码块编号
;;   (setq org-latex-listings t)
;;   (add-to-list 'org-latex-packages-alist '("" "listings" t))
;;   
;;   ;; 方程式编号
;;   (add-to-list 'org-latex-packages-alist '("" "amsmath" t))
;;   (setq org-latex-prefer-user-labels t))

;; ;; 创建复杂学术文档模板
;; (defun my/create-academic-template ()
;;   "创建一个带有标准学术文章结构的Org模板"
;;   (interactive)
;;   (find-file (read-file-name "创建学术文档: " "~/Documents/"))
;;   (erase-buffer)
;;   (insert "#+TITLE: 在此输入标题
;; #+AUTHOR: Henri
;; #+DATE: \\today
;; #+LATEX_CLASS: ctexart
;; #+OPTIONS: toc:t num:t
;; #+BIBLIOGRAPHY: references.bib
;; #+LATEX_HEADER: \\usepackage{amsmath,amssymb,graphicx}
;; #+LATEX_HEADER: \\usepackage[colorlinks=true,linkcolor=blue,citecolor=blue]{hyperref}

;; * 摘要
;; 在此输入摘要...

;; * 引言
;; 在此输入引言...

;; * 方法
;; 在此输入方法...

;; * 结果
;; 在此输入结果...

;; * 讨论
;; 在此输入讨论...

;; * 结论
;; 在此输入结论...

;; * 参考文献
;; <<bibliography>>
;; bibliographystyle:unsrt
;; bibliography:references.bib
;; "))

;; ;; 绑定快捷键
;; (global-set-key (kbd "C-c t a") 'my/create-academic-template)

;; ;; 优化导出 - 确保正确处理参考文献
;; (setq org-latex-pdf-process
;;       '("rm -f %b.aux %b.log %b.out %b.toc %b.bbl %b.blg"
;;         "xelatex -interaction nonstopmode -output-directory %o %f"
;;         "bibtex %b"
;;         "xelatex -interaction nonstopmode -output-directory %o %f"
;;         "xelatex -interaction nonstopmode -output-directory %o %f"
;;         "rm -f %b.aux %b.log %b.out %b.toc")) ;保留 .bbl 和 .blg 文件以便调试

(provide 'org-academic)

;;; org-academic.el ends here
