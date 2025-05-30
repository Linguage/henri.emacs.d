;;; org-academic.el --- Org Mode 学术写作扩展 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 2.0
;; Keywords: org, academic, citation, bibliography

;;; Commentary:

;; Org Mode 学术写作扩展配置，包含：
;; - 引用管理 (org-ref)
;; - 学术模板
;; - 与 org-latex.el 的集成

;;; Code:

;; =============================================================================
;; 引用管理配置

(when (package-installed-p 'org-ref)
  (use-package org-ref
    :ensure t
    :config
    ;; 设置参考文献路径
    (let ((bib-dir (expand-file-name "~/Documents/bibliography"))
          (pdf-dir (expand-file-name "~/Documents/bibliography/bibtex-pdfs"))
          (notes-file (expand-file-name "~/Documents/bibliography/notes.org"))
          (bib-file (expand-file-name "~/Documents/bibliography/references.bib")))
      
      ;; 确保目录存在
      (unless (file-exists-p bib-dir)
        (make-directory bib-dir t))
      (unless (file-exists-p pdf-dir)
        (make-directory pdf-dir t))
      
      ;; 确保文件存在
      (unless (file-exists-p bib-file)
        (with-temp-file bib-file
          (insert "% 参考文献数据库\n% 创建时间: " (current-time-string) "\n\n")))
      (unless (file-exists-p notes-file)
        (with-temp-file notes-file
          (insert "#+TITLE: 参考文献笔记\n#+AUTHOR: Henri\n\n")))
      
      ;; 设置 org-ref 变量
      (setq org-ref-default-bibliography (list bib-file)
            org-ref-pdf-directory pdf-dir
            org-ref-bibliography-notes notes-file
            org-ref-cite-format 'natbib))
    
    ;; 改进引用链接外观
    (setq org-ref-label-use-font-lock t
          org-ref-cite-use-font-lock t)
    
    ;; 快捷键
    (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)))

;; =============================================================================
;; 学术文档模板

(defun my/create-academic-paper ()
  "创建学术论文模板。"
  (interactive)
  (find-file (read-file-name "创建学术论文: " "~/Documents/"))
  (erase-buffer)
  (insert "#+TITLE: 论文标题
#+AUTHOR: Henri
#+DATE: \\today
#+LATEX_CLASS: academic-theme
#+OPTIONS: toc:t num:t

* 摘要
在此输入摘要...

* 引言
在此输入引言...

* 方法
在此输入方法...

* 结果
在此输入结果...

* 讨论
在此输入讨论...

* 结论
在此输入结论...

* 参考文献
bibliographystyle:unsrt
bibliography:references.bib
"))

(defun my/create-conference-paper ()
  "创建会议论文模板。"
  (interactive)
  (find-file (read-file-name "创建会议论文: " "~/Documents/"))
  (erase-buffer)
  (insert "#+TITLE: 会议论文标题
#+AUTHOR: Henri
#+DATE: \\today
#+LATEX_CLASS: academic-theme
#+OPTIONS: toc:nil num:t

#+BEGIN_ABSTRACT
在此输入摘要...
#+END_ABSTRACT

* 引言

* 主要内容

* 结论

* 参考文献
bibliographystyle:acm
bibliography:references.bib
"))

;; =============================================================================
;; 与 org-latex 集成

(defun my/enable-academic-mode ()
  "启用学术写作模式（使用学术主题）。"
  (interactive)
  (require 'org-latex)
  (my/switch-latex-theme 'academic)
  (message "已启用学术写作模式"))

;; =============================================================================
;; 快捷键

(global-set-key (kbd "C-c w p") 'my/create-academic-paper)
(global-set-key (kbd "C-c w c") 'my/create-conference-paper)
(global-set-key (kbd "C-c w m") 'my/enable-academic-mode)

;; =============================================================================
;; 状态检查

(defun my/check-academic-environment ()
  "检查学术写作环境状态。"
  (interactive)
  (if (and (featurep 'org-ref) 
           (boundp 'org-ref-default-bibliography))
      (progn
        (message "✅ 学术写作环境正常")
        (message "参考文献: %s" (car org-ref-default-bibliography)))
    (message "❌ org-ref 未正确配置")))

(global-set-key (kbd "C-c w s") 'my/check-academic-environment)

(provide 'org-academic)

;;; org-academic.el ends here
