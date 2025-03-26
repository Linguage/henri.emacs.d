;;; init-writing.el --- Emacs 写作环境配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: writing, markdown, org, latex

;;; Commentary:

;; 本配置文件提供写作环境支持，包含以下主要模块：

;; 1. Markdown 支持
;;    - markdown-mode      -- Markdown 语法支持
;;    - markdown-preview   -- 实时预览支持
;;    - grip-mode         -- GitHub 风格预览

;; 2. Org Mode 增强
;;    - org-bullets       -- 美化标题样式
;;    - org-superstar     -- 美化列表符号
;;    - org-fancy-priorities -- 优先级美化

;; 3. LaTeX 支持
;;    - AucTeX            -- TeX/LaTeX 支持
;;    - CDLaTeX           -- 快速输入支持

;;; Code:

;; =============================================================================
;; Markdown 配置

;; Markdown 基础配置
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")     ; 使用 pandoc 作为转换工具
  :config
  ;; 内置预览设置
  (setq markdown-preview-stylesheets
        '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"))
  ;; 预览配置
  (setq markdown-fontify-code-blocks-natively t)  ; 原生代码块高亮
  (setq markdown-display-remote-images t)         ; 显示远程图片
  :bind (:map markdown-mode-map
         ("C-c C-v" . markdown-preview)          ; 使用内置预览
         ("C-c C-c p" . markdown-preview-mode))) ; 备选预览模式

;; GitHub 风格预览支持
(use-package grip-mode
  :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-map
         ("C-c C-g" . grip-mode)))

;; 增强预览支持
(use-package markdown-preview-eww
  :ensure t
  :after markdown-mode
  :config
  (setq markdown-preview-eww-open-on-start t)   ; 打开文件时自动预览
  (setq markdown-preview-eww-relative-images t)) ; 支持相对路径图片


;; =============================================================================
;; Org Mode 配置

(use-package org
  :ensure t
  :mode (("\\.org$" . org-mode))
  :init
  (setq org-startup-indented t)           ; 启用缩进
  (setq org-startup-with-inline-images t) ; 显示内联图片
  :config
  (setq org-log-done 'time))             ; 记录完成时间

;; 美化支持
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t))


;; 基础 Org Journal 设置
(require 'org)

;; 设置日志存放目录
(setq org-directory "~/Documents/EmacsNotes/Journal")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; 统一日志模板 - 三种类型：日记(diary)、工作(work)和学习(study)
(setq org-capture-templates
      '(("d" "个人日记" entry (file+olp+datetree "~/Documents/EmacsNotes/Journal/diary.org")
         "* %U %? :journal:diary:\n%i\n** 今日要点\n\n** 花销记录\n| 项目 | 金额 | 类别 |\n|------+------+------|\n|      |      |      |\n"
         :empty-lines 1)
        
        ("w" "工作日志" entry (file+olp+datetree "~/Documents/EmacsNotes/Journal/worklog.org")
         "* %U %? :journal:work:\n%i\n** 完成任务\n\n** 问题和解决方案\n\n** 明日计划\n"
         :empty-lines 1)
        
        ("s" "学习日志" entry (file+olp+datetree "~/Documents/EmacsNotes/Journal/studylog.org")
         "* %U %? :journal:study:\n%i\n** 学习内容\n\n** 重要笔记\n\n** 资源链接\n"
         :empty-lines 1)))

;; 设置快捷键
(global-set-key (kbd "C-c c") 'org-capture)  ;; 快速创建日志
(global-set-key (kbd "C-c a") 'org-agenda)   ;; 打开议程视图

;; 添加通用日志搜索功能
(defun my/search-journal ()
  "在所有日志中搜索关键词"
  (interactive)
  (let ((keyword (read-string "搜索关键词: ")))
    (org-search-view nil keyword)))

(global-set-key (kbd "C-c s") 'my/search-journal)

;; 设置 Org-mode 的 Agenda 文件 - 统一路径命名
(setq org-agenda-files '("/Users/henri/Documents/EmacsNotes/Journal/diary.org"
                         "/Users/henri/Documents/EmacsNotes/Journal/worklog.org"
                         "/Users/henri/Documents/EmacsNotes/Journal/studylog.org"))

;; 通用日志查看函数 - 支持多种日志类型
(defun my/view-journal-by-date (&optional journal-type date)
  "通过选择日期查看特定日期的日志。
JOURNAL-TYPE 可以是 'diary'(个人日记), 'work'(工作日志) 或 'study'(学习日志)。"
  (interactive
   (list (completing-read "选择日志类型: " '("diary" "work" "study") nil t)
         (org-read-date nil nil nil "选择日期: ")))
  
  (let* ((journal-file (cond ((string= journal-type "work") "/Users/henri/Documents/EmacsNotes/Journal/worklog.org")
                              ((string= journal-type "study") "/Users/henri/Documents/EmacsNotes/Journal/studylog.org")
                              (t "/Users/henri/Documents/EmacsNotes/Journal/diary.org")))
         (time (org-time-string-to-time date))
         (day (string-to-number (format-time-string "%d" time)))
         (month (string-to-number (format-time-string "%m" time)))
         (year (string-to-number (format-time-string "%Y" time))))
    
    (find-file journal-file)
    (widen)
    (goto-char (point-min))
    ;; 先查找年份标题
    (if (re-search-forward (format "^\\*+[ \t]+%d$" year) nil t)
        (progn
          (org-narrow-to-subtree)
          ;; 然后查找月份
          (if (re-search-forward (format "^\\*+[ \t]+%s$" 
                                        (format-time-string "%B" time)) nil t)
              (progn
                (org-narrow-to-subtree)
                ;; 最后查找日期
                (if (re-search-forward (format "^\\*+[ \t]+%d" day) nil t)
                    (progn
                      (org-reveal)
                      (org-show-subtree)
                      (recenter)
                      (widen))
                  (widen)
                  (message "未找到 %d 日的%s条目" 
                           day 
                           (cond ((string= journal-type "work") "工作日志")
                                 ((string= journal-type "study") "学习日志")
                                 (t "日记"))))
              (widen)
              (message "未找到 %s 月的%s条目" 
                       (format-time-string "%B" time)
                       (cond ((string= journal-type "work") "工作日志")
                             ((string= journal-type "study") "学习日志")
                             (t "日记"))))
          (widen))
      (message "未找到 %d 年的%s条目" 
               year
               (cond ((string= journal-type "work") "工作日志")
                     ((string= journal-type "study") "学习日志")
                     (t "日记")))))))

;; 便捷函数 - 直接查看个人日记
(defun my/view-diary-by-date (&optional date)
  "通过选择日期查看特定日期的个人日记"
  (interactive)
  (let ((date (or date (org-read-date nil nil nil "选择日期: "))))
    (my/view-journal-by-date "diary" date)))

;; 添加统一的快捷键
(global-set-key (kbd "C-c d") 'my/view-diary-by-date)       ;; 直接查看个人日记
(global-set-key (kbd "C-c j") 'my/view-journal-by-date)     ;; 选择日志类型查看

;; 统一 Agenda 视图名称和结构
(setq org-agenda-custom-commands
      '(("j" "日志概览"
         ((agenda "" ((org-agenda-span 'week)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-show-all-dates t)))
          (tags "diary"
                ((org-agenda-sorting-strategy '(time-up priority-down))
                 (org-agenda-prefix-format "  %i %?-12t% s")
                 (org-agenda-overriding-header "📔 个人日记:")))
          (tags "work"
                ((org-agenda-sorting-strategy '(time-up priority-down))
                 (org-agenda-prefix-format "  %i %?-12t% s")
                 (org-agenda-overriding-header "💼 工作日志:")))
          (tags "study"
                ((org-agenda-sorting-strategy '(time-up priority-down))
                 (org-agenda-prefix-format "  %i %?-12t% s")
                 (org-agenda-overriding-header "📚 学习日志:")))
          (todo ""
                ((org-agenda-files org-agenda-files)
                 (org-agenda-overriding-header "📝 所有待办事项:"))))
         ((org-agenda-compact-blocks t)))
        
        ("d" "个人日记"
         ((tags "diary"
                ((org-agenda-sorting-strategy '(time-up priority-down))
                 (org-agenda-overriding-header "📔 个人日记条目:"))))
         ((org-agenda-compact-blocks t)))
        
        ("w" "工作日志"
         ((tags "work"
                ((org-agenda-sorting-strategy '(time-up priority-down))
                 (org-agenda-overriding-header "💼 工作日志条目:"))))
         ((org-agenda-compact-blocks t)))
        
        ("s" "学习日志"
         ((tags "study"
                ((org-agenda-sorting-strategy '(time-up priority-down))
                 (org-agenda-overriding-header "📚 学习日志条目:"))))
         ((org-agenda-compact-blocks t)))))

;; 优化日历集成功能
(defun my/calendar-open-journal ()
  "在日历中选择日期后打开对应的日志条目"
  (interactive)
  (let* ((date (calendar-cursor-to-date))
         (day (nth 1 date))
         (month (nth 0 date))
         (year (nth 2 date))
         (date-str (format "%04d-%02d-%02d" year month day))
         (journal-type (completing-read "选择日志类型: " 
                                        '(("个人日记" . "diary")
                                          ("工作日志" . "work") 
                                          ("学习日志" . "study")) 
                                        nil t)))
    (my/view-journal-by-date 
     (cond ((string= journal-type "个人日记") "diary")
           ((string= journal-type "工作日志") "work")
           ((string= journal-type "学习日志") "study")
           (t journal-type))
     date-str)))

(add-hook 'calendar-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'my/calendar-open-journal)))

; (use-package org-download
;   :ensure t
;   :config
;   ;; 设置拖放图片的默认目录
;   (setq org-download-image-dir "./images")
;   ;; 自动插入图片链接
;   (add-hook 'dired-mode-hook 'org-download-enable))

(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process
      '("rm -f %b.aux %b.log %b.out %b.toc %b.bbl %b.blg"  ; 先清理临时文件
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "rm -f %b.aux %b.log %b.out %b.toc %b.bbl %b.blg")) ; 编译后再清理

(defun my/org-export-add-latex-class (_backend)
  "在导出为 LaTeX/PDF 之前自动添加 #+LATEX_CLASS: ctexart.
_BACKEND 是导出后端，由钩子提供但在此函数中未使用。"
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^#\\+LATEX_CLASS: ctexart" nil t)
      (insert "#+LATEX_CLASS: ctexart\n\n"))))

(add-hook 'org-export-before-processing-hook 'my/org-export-add-latex-class)
;; 增加对中文的支持
;; ...existing code...

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

;; =============================================================================
;; LaTeX 配置

(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

(use-package cdlatex
  :ensure t
  :hook ((latex-mode . turn-on-cdlatex)
         (org-mode . turn-on-org-cdlatex)))

(provide 'init-writing)

;;; init-writing.el ends here