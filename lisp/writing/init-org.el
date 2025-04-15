
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
; (global-set-key (kbd "C-c j") 'my/view-journal-by-date)     ;; 选择日志类型查看

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

(use-package org-download
  :ensure t
  :config
  ;; 设置拖放图片的默认目录
  (setq org-download-image-dir "./images")
  ;; 自动插入图片链接
  (add-hook 'dired-mode-hook 'org-download-enable))

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

; ;; 安装并配置 org-journal
; (use-package org-journal
;   :ensure t
;   :defer t
;   :custom
;   (org-journal-dir "~/Documents/EmacsNotes/TexNotes/NotesOrg/Journal")
;   (org-journal-file-format "%Y%m%d.org")
;   (org-journal-date-format "%A, %Y年%m月%d日")
;   (org-journal-enable-agenda-integration t)
;   :bind (("C-c j j" . org-journal-new-entry)
;          ("C-c j s" . org-journal-search)
;          ("C-c j v" . my/view-journal-by-date)
;          ("C-c j e" . my/org-journal-summarize-expenses))
;   :config
;   (setq org-journal-file-type 'monthly)
  
;   ;; 设置日记模板，包含花销记录区域
;   (setq org-journal-template
;         "* %<%H:%M> 日记内容
;   %?
  
;   ** 今日任务
;   - [ ] 
  
;   ** 今日感想
  
;   ** 花销记录
;   | 项目 | 金额 | 类别 | 备注 |
;   |------+------+------+------|
;   |      |      |      |      |
;   #+TBLFM: @>$2=vsum(@I..@II)
;   "))

; (defun my/org-journal-summarize-expenses (start-date end-date)
;   "汇总从START-DATE到END-DATE期间的所有花销。"
;   (interactive
;    (let* ((start (org-read-date nil nil nil "开始日期: "))
;           (end (org-read-date nil nil nil "结束日期: ")))
;      (list start end)))
;   (let ((files (directory-files org-journal-dir t "^[0-9]\\{8\\}\\.org$"))
;         (expenses '())
;         (total 0.0))
;     (dolist (file files)
;       (let ((file-date (replace-regexp-in-string "^.*\\([0-9]\\{8\\}\\)\\.org$" "\\1" file)))
;         (when (and (string-lessp start-date file-date)
;                    (string-lessp file-date end-date))
;           (with-temp-buffer
;             (insert-file-contents file)
;             (goto-char (point-min))
;             (while (re-search-forward "^\\s-*|\\s-*\\(.+?\\)\\s-*|\\s-*\\([0-9.]+\\)\\s-*|\\s-*\\(.+?\\)\\s-*|" nil t)
;               (let ((item (match-string 1))
;                     (amount (string-to-number (match-string 2)))
;                     (category (match-string 3)))
;                 (when (> amount 0)
;                   (push (list file-date item amount category) expenses)
;                   (setq total (+ total amount)))))))))
    
;     ;; 创建汇总报告
;     (with-current-buffer (get-buffer-create "*花销汇总*")
;       (erase-buffer)
;       (org-mode)
;       (insert (format "#+TITLE: %s至%s花销汇总\n\n" start-date end-date))
;       (insert "| 日期 | 项目 | 金额 | 类别 |\n")
;       (insert "|------+------+------+------|\n")
      
;       (dolist (expense (reverse expenses))
;         (insert (format "| %s | %s | %.2f | %s |\n" 
;                         (nth 0 expense) (nth 1 expense) 
;                         (nth 2 expense) (nth 3 expense))))
      
;       (insert "|------+------+------+------|\n")
;       (insert (format "| 总计 |      | %.2f |      |\n" total))
;       (org-table-align)
      
;       ;; 添加图表
;       (insert "\n** 按类别统计\n")
;       (let ((categories (make-hash-table :test 'equal)))
;         (dolist (expense expenses)
;           (let ((category (nth 3 expense))
;                 (amount (nth 2 expense)))
;             (puthash category (+ (gethash category categories 0.0) amount) categories)))
        
;         (insert "| 类别 | 金额 | 百分比 |\n")
;         (insert "|------+------+--------|\n")
        
;         (maphash (lambda (category amount)
;                    (insert (format "| %s | %.2f | %.1f%% |\n" 
;                                   category amount (* 100 (/ amount total)))))
;                  categories)
        
;         (insert "|------+------+--------|\n")
;         (insert (format "| 总计 | %.2f | 100%% |\n" total))
;         (org-table-align))
      
;       (switch-to-buffer (current-buffer)))))



; ;; 配置 PDF 导出选项
; (with-eval-after-load 'ox-latex
;   ;; 设置中文支持
;   (setq org-latex-compiler "xelatex")
;   (setq org-latex-pdf-process
;         '("xelatex -interaction nonstopmode -output-directory %o %f"
;           "xelatex -interaction nonstopmode -output-directory %o %f"))
  
;   ;; 自定义 LaTeX 类，使日记打印更美观
;   (add-to-list 'org-latex-classes
;                '("journal"
;                  "\\documentclass[12pt,a4paper]{article}
;                   \\usepackage{xeCJK}
;                   \\setCJKmainfont{SimSun}
;                   \\usepackage[top=2cm,bottom=2cm,left=2cm,right=2cm]{geometry}
;                   \\usepackage{fancyhdr}
;                   \\pagestyle{fancy}
;                   \\fancyhf{}
;                   \\fancyhead[L]{\\rightmark}
;                   \\fancyhead[R]{\\leftmark}
;                   \\fancyfoot[C]{\\thepage}
;                   \\renewcommand{\\headrulewidth}{0.4pt}
;                   \\renewcommand{\\footrulewidth}{0.4pt}"
;                  ("\\section{%s}" . "\\section*{%s}")
;                  ("\\subsection{%s}" . "\\subsection*{%s}")
;                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
;                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(provide 'init-org)