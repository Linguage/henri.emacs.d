
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




(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"))

;; =============================================================================
;; Org Mode 配置

;; =============================================================================
;; pdf-tools
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  ;; 设置 PDF-tools 兼容性
  ;; 禁用 PDF 查看模式下的行号显示
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (display-line-numbers-mode -1))))

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))



;; 修改 org-open-pdf-after-export 函数确保使用 pdf-tools
(defun org-open-pdf-after-export (backend)
  "Open the generated PDF file in pdf-tools after org-mode export."
  (when (eq backend 'latex)
    (let ((pdf-file (concat (file-name-sans-extension buffer-file-name) ".pdf")))
      (when (file-exists-p pdf-file)
        (find-file pdf-file)
        ;; 确保在此处关闭行号显示
        (when (bound-and-true-p display-line-numbers-mode)
          (display-line-numbers-mode -1))))))
(add-hook 'org-export-after-processing-hook 'org-open-pdf-after-export)

;; ...existing code...

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
         "* %U %? :journal:study:\n%i\n** 主题与工作\n\n** 要点笔记\n\n** 资源链接\n"
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


; ;; ...existing code...

; ;; =========================================================================
; ;; Org Mode 高级学术写作增强配置

; ;; 改进引用功能配置
; (use-package org-ref
;   :ensure t
;   :config
;   ;; 设置参考文献样式
;   (setq org-ref-default-bibliography '("~/Documents/bibliography/references.bib")
;         org-ref-pdf-directory "~/Documents/bibliography/bibtex-pdfs/"
;         org-ref-bibliography-notes "~/Documents/bibliography/notes.org"
;         org-ref-cite-format 'natbib)
  
;   ;; 改进引用链接外观
;   (setq org-ref-label-use-font-lock t
;         org-ref-cite-use-font-lock t)
  
;   ;; 增强引用插入体验
;   (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link))

; ;; 参考文献导出增强
; (with-eval-after-load 'ox-latex
;   ;; 添加参考文献支持包
;   (add-to-list 'org-latex-packages-alist '("" "natbib" t))
;   (add-to-list 'org-latex-packages-alist '("" "cleveref" t)) ;; 增强内部引用
  
;   ;; 自动添加参考文献列表
;   (setq org-latex-prefer-user-labels t)
  
;   ;; 允许在生成的PDF中进行交叉引用
;   (add-to-list 'org-latex-packages-alist '("" "hyperref" t)))

; ;; 脚注增强配置
; (with-eval-after-load 'org
;   ;; 启用脚注功能
;   (setq org-export-with-footnotes t)
  
;   ;; 使用行内脚注
;   (setq org-footnote-define-inline t)
  
;   ;; 脚注自动排序
;   (setq org-footnote-auto-adjust t)
  
;   ;; 设置快捷键
;   (define-key org-mode-map (kbd "C-c f") 'org-footnote-new))

; ;; 内部链接增强
; (with-eval-after-load 'org
;   ;; 自动为标题创建ID
;   (use-package org-id
;     :ensure nil
;     :config
;     (setq org-id-link-to-org-use-id 'create-if-interactive)
;     (setq org-id-track-globally t))
  
;   ;; 启用交叉引用功能
;   (setq org-latex-hyperref-template 
;         "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c},\n pdflang={%L},\n colorlinks=true,\n linkcolor=blue,\n citecolor=blue,\n urlcolor=blue}\n")

;   ;; 便捷地插入内部链接
;   (defun my/org-insert-internal-link ()
;     "在当前位置插入到文档中其他标题的链接。"
;     (interactive)
;     (let* ((targets (org-map-entries 
;                     (lambda () 
;                       (cons 
;                        (nth 4 (org-heading-components)) 
;                        (org-id-get-create)))
;                     t 'file))
;            (target (completing-read "链接到: " targets))
;            (id (cdr (assoc target targets))))
;       (insert (format "[[id:%s][%s]]" id target))))
;   (define-key org-mode-map (kbd "C-c l") 'my/org-insert-internal-link))

; ;; 导出增强 - 代码和方程式编号支持
; (with-eval-after-load 'ox-latex
;   ;; 代码块编号
;   (setq org-latex-listings t)
;   (add-to-list 'org-latex-packages-alist '("" "listings" t))
  
;   ;; 方程式编号
;   (add-to-list 'org-latex-packages-alist '("" "amsmath" t))
;   (setq org-latex-prefer-user-labels t))

; ;; 创建复杂学术文档模板
; (defun my/create-academic-template ()
;   "创建一个带有标准学术文章结构的Org模板"
;   (interactive)
;   (find-file (read-file-name "创建学术文档: " "~/Documents/"))
;   (erase-buffer)
;   (insert "#+TITLE: 在此输入标题
; #+AUTHOR: Henri
; #+DATE: \today
; #+LATEX_CLASS: ctexart
; #+OPTIONS: toc:t num:t
; #+BIBLIOGRAPHY: references.bib
; #+LATEX_HEADER: \\usepackage{amsmath,amssymb,graphicx}
; #+LATEX_HEADER: \\usepackage[colorlinks=true,linkcolor=blue,citecolor=blue]{hyperref}

; * 摘要
; 在此输入摘要...

; * 引言
; 在此输入引言...

; * 方法
; 在此输入方法...

; * 结果
; 在此输入结果...

; * 讨论
; 在此输入讨论...

; * 结论
; 在此输入结论...

; * 参考文献
; <<bibliography>>
; bibliographystyle:unsrt
; bibliography:references.bib
; "))

; ;; 绑定快捷键
; (global-set-key (kbd "C-c t a") 'my/create-academic-template)

; ;; 优化导出 - 确保正确处理参考文献
; (setq org-latex-pdf-process
;       '("rm -f %b.aux %b.log %b.out %b.toc %b.bbl %b.blg"
;         "xelatex -interaction nonstopmode -output-directory %o %f"
;         "bibtex %b"
;         "xelatex -interaction nonstopmode -output-directory %o %f"
;         "xelatex -interaction nonstopmode -output-directory %o %f"
;         "rm -f %b.aux %b.log %b.out %b.toc")) ;保留 .bbl 和 .blg 文件以便调试

; ;; ...existing code...


(provide 'init-org)