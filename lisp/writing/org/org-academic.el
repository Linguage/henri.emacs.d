;;; org-academic.el --- Org Mode 学术写作模板系统 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 2.0
;; Keywords: org, academic, writing, research

;;; Commentary:

;; Lightweight academic writing workflow:
;; - idea    -- short thinking cards
;; - reading -- literature reading cards
;; - project -- research project pages
;; - paper   -- full paper drafts
;;
;; The templates intentionally avoid heavy inline LaTeX headers.  Stable PDF
;; export is delegated to `org-latex.el' classes such as `ctexart'.

;;; Code:

(require 'seq)
(require 'subr-x)

;; =============================================================================
;; Customization

(defgroup org-academic nil
  "Academic writing workflow."
  :group 'henri-writing)

(defcustom org-academic-directory
  (if (boundp 'henri-notes-directory)
      (expand-file-name "Academic" (expand-file-name henri-notes-directory))
    (expand-file-name "~/Academic"))
  "Academic writing root directory."
  :type 'directory
  :group 'org-academic)

(defcustom org-academic-ideas-dir
  (expand-file-name "Ideas" org-academic-directory)
  "Lightweight idea notes directory."
  :type 'directory
  :group 'org-academic)

(defcustom org-academic-reading-dir
  (expand-file-name "Reading" org-academic-directory)
  "Literature reading notes directory."
  :type 'directory
  :group 'org-academic)

(defcustom org-academic-projects-dir
  (expand-file-name "Projects" org-academic-directory)
  "Research project pages directory."
  :type 'directory
  :group 'org-academic)

(defcustom org-academic-papers-dir
  (expand-file-name "Papers" org-academic-directory)
  "Paper drafts and conference abstracts directory."
  :type 'directory
  :group 'org-academic)

(defcustom org-academic-notes-dir
  (expand-file-name "Research-Notes" org-academic-directory)
  "Legacy research notes directory kept for existing files."
  :type 'directory
  :group 'org-academic)

(defcustom org-academic-bibliography-file
  (expand-file-name "bibliography.bib" org-academic-directory)
  "BibTeX bibliography file."
  :type 'file
  :group 'org-academic)

(defcustom org-academic-library-dir
  (expand-file-name "PDFs" org-academic-directory)
  "Literature PDF directory."
  :type 'directory
  :group 'org-academic)

(defcustom org-academic-literature-notes-dir
  org-academic-reading-dir
  "Academic literature notes directory used by Citar.

By default this points to `org-academic-reading-dir' at load time.  If
you customize `org-academic-reading-dir', customize this option as well
so `citar-open-notes' continues to use the same reading-card directory."
  :type 'directory
  :group 'org-academic)

(defcustom org-academic-org-roam-dir
  org-academic-directory
  "Legacy Org-roam academic notes directory.

Org-roam is now configured by `org-roam-henri' as a general writing
knowledge base.  This option is kept for compatibility with older
custom files and should not set `org-roam-directory' directly."
  :type 'directory
  :group 'org-academic)

(defcustom org-academic-enable-org-roam nil
  "Compatibility option for the older academic Org-roam integration.

The active Org-roam setup now lives in `org-roam-henri'.  Keep this nil
unless older local customizations still read it."
  :type 'boolean
  :group 'org-academic)

(make-obsolete-variable
 'org-academic-org-roam-dir
 'henri-org-roam-directory
 "2026-05-04")

(make-obsolete-variable
 'org-academic-enable-org-roam
 'henri-org-roam-enable-citar-integration
 "2026-05-04")

;; =============================================================================
;; Citations and optional knowledge base

(defun org-academic-configure-citations ()
  "Configure Org Cite and Citar from academic path customizations."
  (setq org-cite-global-bibliography (list org-academic-bibliography-file)
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        citar-bibliography (list org-academic-bibliography-file)
        citar-library-paths (list org-academic-library-dir)
        citar-notes-paths (list org-academic-literature-notes-dir)))

(use-package citar
  :ensure t
  :commands (citar-insert-citation citar-open)
  :config
  (org-academic-configure-citations))

(with-eval-after-load 'oc
  (org-academic-configure-citations))

;; =============================================================================
;; Internal helpers

(defun org-academic--ensure-dirs ()
  "Create academic directories required by the lightweight workflow."
  (dolist (dir (list org-academic-directory
                     org-academic-ideas-dir
                     org-academic-reading-dir
                     org-academic-projects-dir
                     org-academic-papers-dir
                     org-academic-notes-dir
                     org-academic-library-dir
                     org-academic-literature-notes-dir))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t))))

(defun org-academic--safe-title (title)
  "Return a filesystem-safe slug from TITLE."
  (let ((slug (replace-regexp-in-string "[^a-zA-Z0-9\u4e00-\u9fff]+" "-" title)))
    (string-trim slug "-+" "-+")))

(defun org-academic--blank-string-p (value)
  "Return non-nil when VALUE is nil or an empty string."
  (or (null value)
      (and (stringp value) (string-empty-p value))))

(defun org-academic--dated-file (dir title &optional suffix)
  "Return a dated Org file path under DIR for TITLE and optional SUFFIX."
  (expand-file-name
   (format "%s_%s%s.org"
           (format-time-string "%Y%m%d")
           (org-academic--safe-title title)
           (or suffix ""))
   dir))

(defun org-academic--relative-bib (target-file)
  "Return bibliography path relative to TARGET-FILE."
  (file-relative-name org-academic-bibliography-file
                      (file-name-directory target-file)))

(defun org-academic--front-matter (title type &optional options)
  "Return stable academic Org front matter for TITLE and TYPE."
  (format "#+TITLE: %s
#+AUTHOR: %s
#+DATE: %s
#+EMAIL: %s
#+LANGUAGE: zh-cn
#+CATEGORY: academic-%s
#+OPTIONS: %s
#+STARTUP: content
#+LATEX_CLASS: ctexart

"
          title
          (or user-full-name "Henri")
          (format-time-string "%Y-%m-%d")
          (or user-mail-address "email@example.com")
          type
          (or options "toc:nil num:nil ^:nil title:t")))

(defun org-academic--open-new-file (file content message-prefix)
  "Create FILE with CONTENT and show MESSAGE-PREFIX."
  (when (file-exists-p file)
    (user-error "文件已存在: %s" file))
  (make-directory (file-name-directory file) t)
  (find-file file)
  (insert content)
  (goto-char (point-min))
  (org-mode)
  (save-buffer)
  (message "✓ %s: %s" message-prefix file))

(defun org-academic--files (dir)
  "Return recent Org files in DIR."
  (when (file-directory-p dir)
    (seq-take
     (sort (directory-files dir nil "\\.org\\'")
           #'string>)
     8)))

(defun org-academic--insert-file-list (label dir)
  "Insert a dashboard section LABEL for recent Org files in DIR."
  (insert (format "## %s\n" label))
  (let ((files (org-academic--files dir)))
    (if files
        (dolist (file files)
          (insert (format "- [[file:%s][%s]]\n"
                          (expand-file-name file dir)
                          file)))
      (insert "- 暂无\n")))
  (insert "\n"))

;; =============================================================================
;; Templates

(defun org-academic-format-idea-template (title)
  "Create a lightweight idea note template."
  (format "%s* 一句话

[这条想法最核心的判断。]

* 为什么现在重要

-

* 证据 / 触发

- 来源：
- 观察：

* 可以继续追问

-

* 下一步

- [ ]
"
          (org-academic--front-matter title "idea")))

(defun org-academic-format-reading-template (title cite-key bib-file)
  "Create a literature reading note template."
  (format "%s#+bibliography: %s

* 文献信息

- Citation: %s
- PDF:
- 主题:

* 核心问题

[作者试图回答什么问题？]

* 方法 / 材料

[数据、实验、推导或论证路径。]

* 关键结论

-

* 可引用片段

#+BEGIN_QUOTE

#+END_QUOTE

* 与我的研究的关系

- 支持：
- 反驳：
- 可迁移：

* 下一步

- [ ]
"
          (org-academic--front-matter title "reading")
          bib-file
          (if (org-academic--blank-string-p cite-key)
              "[cite key]"
            (format "cite:%s" cite-key))))

(defun org-academic-format-project-template (title bib-file)
  "Create a research project note template."
  (format "%s#+bibliography: %s

* 定位

- 研究主题：
- 当前阶段：
- 成果形态：

* 研究问题

1.

* 工作假设

-

* 方法路径

- 材料 / 数据：
- 工具 / 模型：
- 验证方式：

* 进展

** TODO 下一步

- [ ]

** 记录

-

* 产出

- 论文：
- 数据：
- 代码：
- 报告：
"
          (org-academic--front-matter title "project" "toc:2 num:nil ^:nil title:t")
          bib-file))

(defun org-academic-format-simple-paper-template (title bib-file)
  "Create a stable full paper draft template."
  (format "%s#+bibliography: %s

* 摘要

[在此填写摘要。]

* 关键词

[关键词1；关键词2；关键词3]

* 引言

** 研究背景

** 研究问题

** 贡献概述

* 文献综述

** 相关研究

** 研究空白

* 方法

** 研究设计

** 数据 / 材料

** 分析方法

* 结果

* 讨论

** 理论意义

** 实践意义

** 局限

* 结论

* 参考文献

#+print_bibliography:
"
          (org-academic--front-matter title "paper" "toc:2 num:t ^:nil title:t")
          bib-file))

(defun org-academic-format-simple-note-template (title)
  "Compatibility wrapper: create a lightweight idea note template."
  (org-academic-format-idea-template title))

(defun org-academic-format-simple-abstract-template (title conference)
  "Create a stable conference abstract template."
  (format "%s* 会议信息

- 会议名称：%s
- 会议时间：
- 地点：
- 字数限制：
- 截止日期：

* 摘要

** 标题

[英文标题（如需要）]

** 正文

[摘要正文内容。]

** 关键词

[关键词1；关键词2；关键词3]

* 投稿状态

- 状态：准备中
- 反馈：
- 修改计划：
"
          (org-academic--front-matter title "abstract")
          conference))

;; =============================================================================
;; Creation commands

(defun org-academic-create-idea-note (&optional title)
  "Create a lightweight academic idea note."
  (interactive "s想法标题: ")
  (org-academic--ensure-dirs)
  (let* ((note-title (if (org-academic--blank-string-p title) "新想法" title))
         (file (org-academic--dated-file org-academic-ideas-dir note-title)))
    (org-academic--open-new-file
     file
     (org-academic-format-idea-template note-title)
     "已创建想法卡")))

(defun org-academic-create-reading-note (&optional title cite-key)
  "Create a literature reading note."
  (interactive
   (list (read-string "阅读笔记标题: ")
         (read-string "Cite key（可空）: ")))
  (org-academic--ensure-dirs)
  (let* ((note-title (if (org-academic--blank-string-p title) "新阅读笔记" title))
         (file (org-academic--dated-file org-academic-reading-dir note-title))
         (bib-file (org-academic--relative-bib file)))
    (org-academic--open-new-file
     file
     (org-academic-format-reading-template note-title (or cite-key "") bib-file)
     "已创建阅读卡")))

(defun org-academic-create-project-note (&optional title)
  "Create a research project note."
  (interactive "s项目标题: ")
  (org-academic--ensure-dirs)
  (let* ((project-title (if (org-academic--blank-string-p title) "新研究项目" title))
         (file (org-academic--dated-file org-academic-projects-dir project-title))
         (bib-file (org-academic--relative-bib file)))
    (org-academic--open-new-file
     file
     (org-academic-format-project-template project-title bib-file)
     "已创建研究项目")))

(defun org-academic-create-paper (&optional title)
  "Create a full academic paper draft."
  (interactive "s论文标题: ")
  (org-academic--ensure-dirs)
  (let* ((paper-title (if (org-academic--blank-string-p title) "新学术论文" title))
         (file (org-academic--dated-file org-academic-papers-dir paper-title))
         (bib-file (org-academic--relative-bib file)))
    (org-academic--open-new-file
     file
     (org-academic-format-simple-paper-template paper-title bib-file)
     "已创建学术论文")))

(defun org-academic-create-research-note (&optional title)
  "Compatibility command: create a lightweight idea note."
  (interactive "s研究笔记标题: ")
  (org-academic-create-idea-note title))

(defun org-academic-create-conference-abstract (&optional title)
  "Create a conference abstract note."
  (interactive "s摘要标题: ")
  (org-academic--ensure-dirs)
  (let* ((abstract-title (if (org-academic--blank-string-p title) "新会议摘要" title))
         (conference (read-string "会议名称: " "学术会议"))
         (file (org-academic--dated-file org-academic-papers-dir abstract-title "_abstract")))
    (org-academic--open-new-file
     file
     (org-academic-format-simple-abstract-template abstract-title conference)
     "已创建会议摘要")))

(defun org-academic-quick-paper ()
  "Quickly create a timestamped academic paper draft."
  (interactive)
  (org-academic-create-paper
   (format "学术论文_%s" (format-time-string "%m%d_%H%M"))))

(defun org-academic-quick-note ()
  "Quickly create a timestamped lightweight idea note."
  (interactive)
  (org-academic-create-idea-note
   (format "想法_%s" (format-time-string "%m%d_%H%M"))))

;; =============================================================================
;; Bibliography and citations

(defun org-academic-setup-bibliography ()
  "Create bibliography, PDF, and reading-note directories when missing."
  (interactive)
  (org-academic--ensure-dirs)
  (unless (file-exists-p org-academic-bibliography-file)
    (make-directory (file-name-directory org-academic-bibliography-file) t)
    (with-temp-file org-academic-bibliography-file
      (insert "% 参考文献数据库\n% 使用 BibTeX 格式\n\n")))
  (org-academic-configure-citations)
  (find-file org-academic-bibliography-file)
  (message "参考文献数据库已就绪: %s" org-academic-bibliography-file))

(defun org-academic-insert-citation ()
  "Insert a citation through Citar, with a manual fallback."
  (interactive)
  (if (fboundp 'citar-insert-citation)
      (citar-insert-citation)
    (let ((cite-key (read-string "引用关键字: ")))
      (insert (format "cite:%s" cite-key)))))

;; =============================================================================
;; Dashboard

(defun org-academic-dashboard ()
  "Open the academic writing dashboard."
  (interactive)
  (org-academic--ensure-dirs)
  (let ((dashboard-buffer "*Academic Dashboard*"))
    (get-buffer-create dashboard-buffer)
    (with-current-buffer dashboard-buffer
      (erase-buffer)
      (insert "# 学术写作仪表板\n\n")
      (insert "## 快速入口\n")
      (insert "- `C-c a n` 想法卡（轻量 note）\n")
      (insert "- `C-c a r` 阅读卡\n")
      (insert "- `C-c a j` 研究项目页\n")
      (insert "- `C-c a p` 论文草稿\n")
      (insert "- `C-c a c` 会议摘要\n")
      (insert "- `C-c a b` 初始化 bibliography / PDFs\n")
      (insert "- `C-c a i` 插入引用\n\n")
      (insert "## 路径\n")
      (insert (format "- BibTeX: `%s`\n" org-academic-bibliography-file))
      (insert (format "- PDFs: `%s`\n" org-academic-library-dir))
      (insert (format "- Ideas: `%s`\n" org-academic-ideas-dir))
      (insert (format "- Reading: `%s`\n" org-academic-reading-dir))
      (insert (format "- Projects: `%s`\n" org-academic-projects-dir))
      (insert (format "- Papers: `%s`\n\n" org-academic-papers-dir))
      (org-academic--insert-file-list "Ideas" org-academic-ideas-dir)
      (org-academic--insert-file-list "Reading Notes" org-academic-reading-dir)
      (org-academic--insert-file-list "Projects" org-academic-projects-dir)
      (org-academic--insert-file-list "Papers" org-academic-papers-dir)
      (insert "---\n")
      (insert "*提示：按 `q` 退出此缓冲区*")
      (org-mode)
      (goto-char (point-min))
      (local-set-key (kbd "q") #'kill-this-buffer))
    (switch-to-buffer dashboard-buffer)))

;; =============================================================================
;; Minor mode and initialization

(defvar org-academic-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a n") #'org-academic-create-idea-note)
    (define-key map (kbd "C-c a r") #'org-academic-create-reading-note)
    (define-key map (kbd "C-c a j") #'org-academic-create-project-note)
    (define-key map (kbd "C-c a p") #'org-academic-create-paper)
    (define-key map (kbd "C-c a c") #'org-academic-create-conference-abstract)
    (define-key map (kbd "C-c a P") #'org-academic-quick-paper)
    (define-key map (kbd "C-c a N") #'org-academic-quick-note)
    (define-key map (kbd "C-c a b") #'org-academic-setup-bibliography)
    (define-key map (kbd "C-c a d") #'org-academic-dashboard)
    (define-key map (kbd "C-c a i") #'org-academic-insert-citation)
    map)
  "Keymap for `org-academic-mode'.")

(define-minor-mode org-academic-mode
  "Academic writing helper mode."
  :lighter " Academic"
  :keymap org-academic-mode-map
  :global t)

(defun org-academic-init ()
  "Initialize the academic writing environment."
  (interactive)
  (org-academic--ensure-dirs)
  (org-academic-configure-citations)
  (org-academic-mode 1)
  (message "✓ 学术写作环境初始化完成"))

(add-hook 'org-mode-hook
          (lambda ()
            (when (string-match-p "/Academic/" (or buffer-file-name ""))
              (org-academic-mode 1))))

(provide 'org-academic)

;;; org-academic.el ends here
