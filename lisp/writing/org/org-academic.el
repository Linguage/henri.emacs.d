;;; org-academic.el --- Org Mode 学术写作模板系统 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Keywords: org, academic, writing, research

;;; Commentary:

;; Org Mode 学术写作模板系统，包含：
;; - 学术论文模板（多种格式）
;; - 研究笔记模板
;; - 文献管理和引用
;; - 学术写作工作流

;;; Code:

;; =============================================================================
;; 学术写作目录设置

(defcustom org-academic-directory
  (if (boundp 'henri-notes-directory)
      (expand-file-name "Academic" (expand-file-name henri-notes-directory))
    (expand-file-name "~/Academic"))
  "学术写作文档的根目录"
  :type 'directory
  :group 'org-academic)

(defcustom org-academic-papers-dir (concat org-academic-directory "/Papers")
  "学术论文存放目录"
  :type 'directory
  :group 'org-academic)

(defcustom org-academic-notes-dir (concat org-academic-directory "/Research-Notes")
  "研究笔记存放目录"
  :type 'directory
  :group 'org-academic)

(defcustom org-academic-bibliography-file (concat org-academic-directory "/bibliography.bib")
  "参考文献数据库文件"
  :type 'file
  :group 'org-academic)

;; =============================================================================
;; 学术写作模板定义

(defvar org-academic-paper-template
  "#+TITLE: %^{论文标题}
#+AUTHOR: %^{作者姓名}
#+DATE: %T
#+EMAIL: %^{邮箱地址}
#+LANGUAGE: zh-cn
#+OPTIONS: toc:2 num:t ^:nil
#+STARTUP: content

#+BEGIN_EXPORT html
<style>
.abstract { 
  background-color: #f8f9fa; 
  padding: 15px; 
  border-left: 4px solid #007acc; 
  margin: 20px 0; 
}
</style>
#+END_EXPORT

* 摘要
:PROPERTIES:
:CUSTOM_ID: abstract
:END:

#+BEGIN_abstract
%^{摘要内容}

*关键词：* %^{关键词（用逗号分隔）}
#+END_abstract

* 1. 引言
:PROPERTIES:
:CUSTOM_ID: introduction
:END:

** 1.1 研究背景

** 1.2 研究问题

** 1.3 研究目标

** 1.4 论文结构

* 2. 文献综述
:PROPERTIES:
:CUSTOM_ID: literature-review
:END:

** 2.1 理论基础

** 2.2 相关研究

** 2.3 研究空白

* 3. 研究方法
:PROPERTIES:
:CUSTOM_ID: methodology
:END:

** 3.1 研究设计

** 3.2 数据收集

** 3.3 分析方法

* 4. 研究结果
:PROPERTIES:
:CUSTOM_ID: results
:END:

** 4.1 主要发现

** 4.2 数据分析

* 5. 讨论
:PROPERTIES:
:CUSTOM_ID: discussion
:END:

** 5.1 结果解释

** 5.2 理论贡献

** 5.3 实践意义

** 5.4 研究局限

* 6. 结论
:PROPERTIES:
:CUSTOM_ID: conclusion
:END:

** 6.1 主要结论

** 6.2 未来研究方向

* 参考文献
:PROPERTIES:
:CUSTOM_ID: references
:END:

#+BIBLIOGRAPHY: %s plain

* 附录
:PROPERTIES:
:CUSTOM_ID: appendix
:END:

#+BEGIN_COMMENT
论文写作说明：
1. 使用 C-c C-l 插入链接和引用
2. 使用 C-c C-x C-l 预览 LaTeX 片段
3. 使用 C-c C-e 导出为所需格式
4. 参考文献使用 BibTeX 格式管理
#+END_COMMENT"
  "学术论文基础模板")

(defvar org-academic-research-note-template
  "#+TITLE: %^{研究笔记标题}
#+AUTHOR: %^{研究者}
#+DATE: %T
#+TAGS: research note %^{标签}
#+CATEGORY: %^{研究领域}
#+STARTUP: content

* 研究信息
:PROPERTIES:
:研究主题: %^{研究主题}
:研究阶段: %^{阶段|文献调研|实验设计|数据收集|数据分析|论文写作}
:优先级: %^{优先级|A|B|C}
:截止日期: %^{截止日期}t
:相关项目: %^{相关项目}
:END:

** 研究背景
%^{研究背景和动机}

** 研究问题
%^{具体研究问题}

* 文献笔记
:PROPERTIES:
:CUSTOM_ID: literature-notes
:END:

** 核心文献

| 作者 | 标题 | 年份 | 核心观点 | 引用价值 |
|------+------+------+----------+----------|
|      |      |      |          |          |

** 理论框架

** 方法论参考

* 研究思路
:PROPERTIES:
:CUSTOM_ID: research-ideas
:END:

** 假设
1. 

** 研究设计
- 研究类型：
- 样本选择：
- 数据收集方法：
- 分析方法：

** 预期结果

* 进展记录
:PROPERTIES:
:CUSTOM_ID: progress-log
:END:

** TODO 待办事项
- [ ] 
- [ ] 
- [ ] 

** 已完成
- [X] 

* 资源链接
:PROPERTIES:
:CUSTOM_ID: resources
:END:

** 相关网站

** 数据来源

** 软件工具

* 反思总结
:PROPERTIES:
:CUSTOM_ID: reflection
:END:

** 遇到的问题

** 解决方案

** 经验教训

** 下一步计划"
  "研究笔记模板")

(defvar org-academic-conference-abstract-template
  "#+TITLE: %^{会议摘要标题}
#+AUTHOR: %^{作者}
#+DATE: %T
#+CONFERENCE: %^{会议名称}
#+DEADLINE: %^{提交截止日期}t
#+OPTIONS: toc:nil num:nil

* 会议信息
:PROPERTIES:
:会议名称: %^{会议全称}
:会议时间: %^{会议时间}
:会议地点: %^{会议地点}
:摘要字数限制: %^{字数限制}
:提交方式: %^{提交方式}
:END:

* 摘要

** 标题
%^{英文标题（如需要）}

** 正文
%^{摘要正文内容}

** 关键词
%^{关键词列表}

* 扩展信息

** 研究意义

** 创新点

** 预期影响

* 投稿状态
:PROPERTIES:
:投稿状态: %^{状态|准备中|已投稿|已接收|已拒绝}
:反馈意见: 
:修改计划:
:END:"
  "会议摘要模板")

;; =============================================================================
;; 简化模板格式化函数

(defun org-academic-format-simple-paper-template (title bib-file)
  "创建简洁的学术论文模板"
  (format "#+TITLE: %s
#+AUTHOR: %s
#+DATE: %s
#+EMAIL: %s
#+LANGUAGE: zh-cn
#+OPTIONS: toc:3 num:t ^:nil title:t
#+STARTUP: showeverything
#+LATEX_CLASS: article
#+LATEX_CLASS_OPTIONS: [a4paper,12pt]
#+LATEX_HEADER: \\usepackage[UTF8]{ctex}
#+LATEX_HEADER: \\usepackage[margin=2.5cm]{geometry}
#+LATEX_HEADER: \\usepackage{setspace}
#+LATEX_HEADER: \\onehalfspacing
#+LATEX_HEADER: \\usepackage{fancyhdr}
#+LATEX_HEADER: \\pagestyle{fancy}
#+LATEX_HEADER: \\fancyhf{}
#+LATEX_HEADER: \\fancyhead[C]{%s}
#+LATEX_HEADER: \\fancyfoot[C]{\\thepage}
#+LATEX_HEADER: \\renewcommand{\\headrulewidth}{0.4pt}
#+LATEX_HEADER: \\renewcommand{\\footrulewidth}{0.4pt}
#+LATEX_HEADER: \\usepackage{titletoc}
#+LATEX_HEADER: \\usepackage{titlesec}
#+LATEX_HEADER: \\titleformat{\\section}{\\Large\\bfseries}{\\thesection}{1em}{}
#+LATEX_HEADER: \\titleformat{\\subsection}{\\large\\bfseries}{\\thesubsection}{1em}{}
#+LATEX_HEADER: \\titleformat{\\subsubsection}{\\normalsize\\bfseries}{\\thesubsubsection}{1em}{}
#+LATEX_HEADER: \\renewcommand{\\contentsname}{目录}

* 摘要

[在此填写摘要内容...]

*关键词：* [关键词1, 关键词2, 关键词3]

* 引言

** 研究背景

[填写研究背景]

** 研究问题

[填写研究问题]

** 研究目标

[填写研究目标]

** 论文结构

[填写论文结构]

* 文献综述

** 理论基础

[填写理论基础]

** 相关研究

[填写相关研究]

** 研究空白

[填写研究空白]

* 研究方法

** 研究设计

[填写研究设计]

** 数据收集

[填写数据收集]

** 分析方法

[填写分析方法]

* 研究结果

** 主要发现

[填写主要发现]

** 数据分析

[填写数据分析]

* 讨论

** 结果解释

[填写结果解释]

** 理论贡献

[填写理论贡献]

** 实践意义

[填写实践意义]

** 研究局限

[填写研究局限]

* 结论

** 主要结论

[填写主要结论]

** 未来研究方向

[填写未来研究方向]

* 参考文献

#+BIBLIOGRAPHY: %s plain

* 附录

#+BEGIN_COMMENT
论文写作说明：
1. 使用 C-c C-l 插入链接和引用
2. 使用 C-c C-x C-l 预览 LaTeX 片段
3. 使用 C-c C-e 导出为所需格式
4. 参考文献使用 BibTeX 格式管理
#+END_COMMENT"
          title
          (or user-full-name "作者姓名")
          (format-time-string "%Y-%m-%d")
          (or user-mail-address "email@example.com")
          title ; 用于页眉
          bib-file))

(defun org-academic-format-simple-note-template (title)
  "创建简洁的研究笔记模板"
  (format "#+TITLE: %s
#+AUTHOR: %s
#+DATE: %s
#+EMAIL: %s
#+LANGUAGE: zh-cn
#+OPTIONS: toc:3 num:t ^:nil title:t
#+STARTUP: showeverything
#+LATEX_CLASS: article
#+LATEX_CLASS_OPTIONS: [a4paper,12pt]
#+LATEX_HEADER: \\usepackage[UTF8]{ctex}
#+LATEX_HEADER: \\usepackage[margin=2.5cm]{geometry}
#+LATEX_HEADER: \\usepackage{setspace}
#+LATEX_HEADER: \\onehalfspacing
#+LATEX_HEADER: \\usepackage{fancyhdr}
#+LATEX_HEADER: \\pagestyle{fancy}
#+LATEX_HEADER: \\fancyhf{}
#+LATEX_HEADER: \\fancyhead[C]{%s}
#+LATEX_HEADER: \\fancyfoot[C]{\\thepage}
#+LATEX_HEADER: \\renewcommand{\\headrulewidth}{0.4pt}
#+LATEX_HEADER: \\renewcommand{\\footrulewidth}{0.4pt}
#+LATEX_HEADER: \\usepackage{titletoc}
#+LATEX_HEADER: \\usepackage{titlesec}
#+LATEX_HEADER: \\titleformat{\\section}{\\Large\\bfseries}{\\thesection}{1em}{}
#+LATEX_HEADER: \\titleformat{\\subsection}{\\large\\bfseries}{\\thesubsection}{1em}{}
#+LATEX_HEADER: \\titleformat{\\subsubsection}{\\normalsize\\bfseries}{\\thesubsubsection}{1em}{}
#+LATEX_HEADER: \\renewcommand{\\contentsname}{目录}

* 研究信息

** 研究主题
[填写研究主题]

** 研究阶段
[文献调研/实验设计/数据收集/数据分析/论文写作]

** 优先级
[A/B/C]

** 截止日期
[YYYY-MM-DD]

** 相关项目
[项目名称]

** 研究背景
[描述研究背景和动机...]

** 研究问题
[明确具体研究问题...]

* 文献笔记

** 核心文献

| 作者 | 标题 | 年份 | 核心观点 | 引用价值 |
|------+------+------+----------+----------|
|      |      |      |          |          |

** 理论框架

** 方法论参考

* 研究思路

** 假设
1. 

** 研究设计
- 研究类型：
- 样本选择：
- 数据收集方法：
- 分析方法：

** 预期结果

* 进展记录

** TODO 待办事项
- [ ] 
- [ ] 
- [ ] 

** 已完成
- [X] 

* 资源链接

** 相关网站

** 数据来源

** 软件工具

* 反思总结

** 遇到的问题

** 解决方案

** 经验教训

** 下一步计划"
          title
          (or user-full-name "研究者")
          (format-time-string "%Y-%m-%d")
          (or user-mail-address "email@example.com")
          title))

(defun org-academic-format-simple-abstract-template (title conference)
  "创建简洁的会议摘要模板"
  (format "#+TITLE: %s
#+AUTHOR: %s
#+DATE: %s
#+EMAIL: %s
#+LANGUAGE: zh-cn
#+OPTIONS: toc:3 num:t ^:nil title:t
#+STARTUP: showeverything
#+LATEX_CLASS: article
#+LATEX_CLASS_OPTIONS: [a4paper,12pt]
#+LATEX_HEADER: \\usepackage[UTF8]{ctex}
#+LATEX_HEADER: \\usepackage[margin=2.5cm]{geometry}
#+LATEX_HEADER: \\usepackage{setspace}
#+LATEX_HEADER: \\onehalfspacing
#+LATEX_HEADER: \\usepackage{fancyhdr}
#+LATEX_HEADER: \\pagestyle{fancy}
#+LATEX_HEADER: \\fancyhf{}
#+LATEX_HEADER: \\fancyhead[C]{%s}
#+LATEX_HEADER: \\fancyfoot[C]{\\thepage}
#+LATEX_HEADER: \\renewcommand{\\headrulewidth}{0.4pt}
#+LATEX_HEADER: \\renewcommand{\\footrulewidth}{0.4pt}
#+LATEX_HEADER: \\usepackage{titletoc}
#+LATEX_HEADER: \\usepackage{titlesec}
#+LATEX_HEADER: \\titleformat{\\section}{\\Large\\bfseries}{\\thesection}{1em}{}
#+LATEX_HEADER: \\titleformat{\\subsection}{\\large\\bfseries}{\\thesubsection}{1em}{}
#+LATEX_HEADER: \\titleformat{\\subsubsection}{\\normalsize\\bfseries}{\\thesubsubsection}{1em}{}
#+LATEX_HEADER: \\renewcommand{\\contentsname}{目录}

* 会议信息

** 会议名称
%s

** 会议时间
[YYYY-MM-DD]

** 会议地点
[会议地点]

** 摘要字数限制
[字数限制]

** 提交方式
[在线提交/邮件等]

* 摘要

** 标题
[英文标题（如需要）]

** 正文
[摘要正文内容...]

** 关键词
[关键词1, 关键词2, 关键词3]

* 扩展信息

** 研究意义

** 创新点

** 预期影响

* 投稿状态

** 投稿状态
准备中

** 反馈意见
[填写反馈意见]

** 修改计划
[填写修改计划]"
          title
          (or user-full-name "作者")
          (format-time-string "%Y-%m-%d")
          (or user-mail-address "email@example.com")
          title ; 用于页眉
          conference
          conference))

;; =============================================================================
;; 简化的模板创建函数

(defun org-academic-create-paper (&optional title)
  "创建新的学术论文文档 - 只需要输入标题"
  (interactive "s论文标题: ")
  (let* ((paper-title (or title "新学术论文"))
         (safe-title (replace-regexp-in-string "[^a-zA-Z0-9\u4e00-\u9fff]" "-" paper-title))
         (filename (format "%s/%s_%s.org" 
                          org-academic-papers-dir
                          (format-time-string "%Y%m%d")
                          safe-title))
         (bib-file (file-relative-name org-academic-bibliography-file 
                                      (file-name-directory filename))))
    
    (unless (file-exists-p org-academic-papers-dir)
      (make-directory org-academic-papers-dir t))
    
    (find-file filename)
    (insert (org-academic-format-simple-paper-template paper-title bib-file))
    (goto-char (point-min))
    (org-mode)
    (message "✓ 已创建学术论文: %s" filename)))

(defun org-academic-create-research-note (&optional title)
  "创建新的研究笔记 - 只需要输入标题"
  (interactive "s研究笔记标题: ")
  (let* ((note-title (or title "新研究笔记"))
         (safe-title (replace-regexp-in-string "[^a-zA-Z0-9\u4e00-\u9fff]" "-" note-title))
         (filename (format "%s/%s_%s.org" 
                          org-academic-notes-dir
                          (format-time-string "%Y%m%d")
                          safe-title)))
    
    (unless (file-exists-p org-academic-notes-dir)
      (make-directory org-academic-notes-dir t))
    
    (find-file filename)
    (insert (org-academic-format-simple-note-template note-title))
    (goto-char (point-min))
    (org-mode)
    (message "✓ 已创建研究笔记: %s" filename)))

(defun org-academic-create-conference-abstract (&optional title)
  "创建会议摘要 - 只需要输入标题"
  (interactive "s摘要标题: ")
  (let* ((abstract-title (or title "新会议摘要"))
         (conference (read-string "会议名称: " "学术会议"))
         (safe-title (replace-regexp-in-string "[^a-zA-Z0-9\u4e00-\u9fff]" "-" abstract-title))
         (filename (format "%s/%s_%s_abstract.org" 
                          org-academic-papers-dir
                          (format-time-string "%Y%m%d")
                          safe-title)))
    
    (unless (file-exists-p org-academic-papers-dir)
      (make-directory org-academic-papers-dir t))
    
    (find-file filename)
    (insert (org-academic-format-simple-abstract-template abstract-title conference))
    (goto-char (point-min))
    (org-mode)
    (message "✓ 已创建会议摘要: %s" filename)))

;; =============================================================================
;; 快速创建命令 (无交互)

(defun org-academic-quick-paper ()
  "快速创建学术论文（无交互）"
  (interactive)
  (let* ((paper-title (format "学术论文_%s" (format-time-string "%m%d_%H%M")))
         (safe-title (replace-regexp-in-string "[^a-zA-Z0-9\u4e00-\u9fff]" "-" paper-title))
         (filename (format "%s/%s_%s.org" 
                          org-academic-papers-dir
                          (format-time-string "%Y%m%d")
                          safe-title))
         (bib-file (file-relative-name org-academic-bibliography-file 
                                      (file-name-directory filename))))
    
    (unless (file-exists-p org-academic-papers-dir)
      (make-directory org-academic-papers-dir t))
    
    (find-file filename)
    (insert (org-academic-format-simple-paper-template paper-title bib-file))
    (goto-char (point-min))
    (org-mode)
    (message "✓ 快速创建学术论文: %s" filename)))

(defun org-academic-quick-note ()
  "快速创建研究笔记（无交互）"
  (interactive)
  (let* ((note-title (format "研究笔记_%s" (format-time-string "%m%d_%H%M")))
         (safe-title (replace-regexp-in-string "[^a-zA-Z0-9\u4e00-\u9fff]" "-" note-title))
         (filename (format "%s/%s_%s.org" 
                          org-academic-notes-dir
                          (format-time-string "%Y%m%d")
                          safe-title)))
    
    (unless (file-exists-p org-academic-notes-dir)
      (make-directory org-academic-notes-dir t))
    
    (find-file filename)
    (insert (org-academic-format-simple-note-template note-title))
    (goto-char (point-min))
    (org-mode)
    (message "✓ 快速创建研究笔记: %s" filename)))

;; =============================================================================
;; 文献管理功能

(defun org-academic-setup-bibliography ()
  "设置参考文献环境"
  (interactive)
  (unless (file-exists-p org-academic-bibliography-file)
    (with-temp-file org-academic-bibliography-file
      (insert "% 参考文献数据库\n% 使用 BibTeX 格式\n\n")))
  
  (find-file org-academic-bibliography-file)
  (message "参考文献数据库已就绪: %s" org-academic-bibliography-file))

(defun org-academic-insert-citation ()
  "插入文献引用"
  (interactive)
  (let ((cite-key (read-string "引用关键字: ")))
    (insert (format "cite:%s" cite-key))))

;; =============================================================================
;; 学术写作工作流

(defun org-academic-dashboard ()
  "打开学术写作仪表板"
  (interactive)
  (let ((dashboard-buffer "*Academic Dashboard*"))
    (get-buffer-create dashboard-buffer)
    (with-current-buffer dashboard-buffer
      (erase-buffer)
      (insert "# 学术写作仪表板 📚\n\n")
      (insert "## 快速创建命令\n")
      (insert "### 标准创建（需要输入标题）\n")
      (insert "- `C-c a p` 创建学术论文\n")
      (insert "- `C-c a n` 创建研究笔记\n") 
      (insert "- `C-c a c` 创建会议摘要\n\n")
      (insert "### 快速创建（无交互）\n")
      (insert "- `C-c a P` 快速创建论文（自动命名）\n")
      (insert "- `C-c a N` 快速创建笔记（自动命名）\n\n")
      (insert "### 管理功能\n")
      (insert "- `C-c a b` 管理参考文献\n")
      (insert "- `C-c a d` 打开仪表板\n")
      (insert "- `C-c a i` 插入文献引用\n\n")
      
      (insert "## 最近论文\n")
      (when (file-exists-p org-academic-papers-dir)
        (let ((files (directory-files org-academic-papers-dir nil "\\.org$")))
          (if files
              (dolist (file (seq-take files 5))
                (insert (format "- [[file:%s/%s][%s]]\n" 
                               org-academic-papers-dir file file)))
            (insert "- 暂无文档\n"))))
      
      (insert "\n## 最近笔记\n")
      (when (file-exists-p org-academic-notes-dir)
        (let ((files (directory-files org-academic-notes-dir nil "\\.org$")))
          (if files
              (dolist (file (seq-take files 5))
                (insert (format "- [[file:%s/%s][%s]]\n" 
                               org-academic-notes-dir file file)))
            (insert "- 暂无笔记\n"))))
      
      (insert "\n---\n")
      (insert "*提示：按 `q` 退出此缓冲区*")
      
      (org-mode)
      (goto-char (point-min))
      (local-set-key (kbd "q") 'kill-this-buffer))
    (switch-to-buffer dashboard-buffer)))

;; =============================================================================
;; 快捷键绑定 - 简化版

(defvar org-academic-mode-map
  (let ((map (make-sparse-keymap)))
    ;; 基础创建命令（需要输入标题）
    (define-key map (kbd "C-c a p") 'org-academic-create-paper)
    (define-key map (kbd "C-c a n") 'org-academic-create-research-note)
    (define-key map (kbd "C-c a c") 'org-academic-create-conference-abstract)
    
    ;; 快速创建命令（无交互）
    (define-key map (kbd "C-c a P") 'org-academic-quick-paper)
    (define-key map (kbd "C-c a N") 'org-academic-quick-note)
    
    ;; 管理功能
    (define-key map (kbd "C-c a b") 'org-academic-setup-bibliography)
    (define-key map (kbd "C-c a d") 'org-academic-dashboard)
    (define-key map (kbd "C-c a i") 'org-academic-insert-citation)
    map)
  "学术写作模式快捷键映射")

(define-minor-mode org-academic-mode
  "学术写作辅助模式 - 简化版"
  :lighter " Academic"
  :keymap org-academic-mode-map
  :global t)

;; =============================================================================
;; 初始化 - 简化版

(defun org-academic-init ()
  "初始化学术写作环境"
  (interactive)
  
  ;; 创建必要的目录
  (unless (file-exists-p org-academic-directory)
    (make-directory org-academic-directory t))
  (unless (file-exists-p org-academic-papers-dir)
    (make-directory org-academic-papers-dir t))
  (unless (file-exists-p org-academic-notes-dir)
    (make-directory org-academic-notes-dir t))
  
  ;; 启用学术写作模式
  (org-academic-mode 1)
  
  (message "✓ 学术写作环境初始化完成"))

;; 自动初始化
(add-hook 'org-mode-hook 
          (lambda ()
            (when (string-match-p "/Academic/" (or buffer-file-name ""))
              (org-academic-mode 1))))

(provide 'org-academic)

;;; org-academic.el ends here
