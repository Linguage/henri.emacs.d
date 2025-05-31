;;; org-journal.el --- Org Mode 日志系统配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Keywords: org, journal, diary

;;; Commentary:

;; Org Mode 日志系统配置，包含：
;; - 日志模板配置
;; - 快捷键设置
;; - 日志查看和搜索功能
;; - Agenda 自定义视图

;;; Code:

;; =============================================================================
;; Org 日志系统配置

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

;; 设置 Org-mode 的 Agenda 文件 - 统一路径命名
(setq org-agenda-files '("/Users/henri/Documents/EmacsNotes/Journal/diary.org"
                         "/Users/henri/Documents/EmacsNotes/Journal/worklog.org"
                         "/Users/henri/Documents/EmacsNotes/Journal/studylog.org"))

;; =============================================================================
;; 日志查看和搜索功能

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

;; 添加通用日志搜索功能
(defun my/search-journal ()
  "在所有日志中搜索关键词"
  (interactive)
  (let ((keyword (read-string "搜索关键词: ")))
    (org-search-view nil keyword)))

;; =============================================================================
;; Agenda 自定义视图

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

;; =============================================================================
;; 日历集成

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

;; =============================================================================
;; 快捷键设置

(global-set-key (kbd "C-c c") 'org-capture)            ;; 快速创建日志
(global-set-key (kbd "C-c a") 'org-agenda)             ;; 打开议程视图
(global-set-key (kbd "C-c j s") 'my/search-journal)      ;; 搜索日志
(global-set-key (kbd "C-c j d") 'my/view-diary-by-date)  ;; 直接查看个人日记

;; =============================================================================
;; 期刊PDF导出配置

(defun org-journal-setup-pdf-export ()
  "为期刊文件设置PDF导出的1cm页边距"
  (interactive)
  (when (buffer-file-name)
    (save-excursion
      (goto-char (point-min))
      ;; 检查是否已经有LATEX_CLASS设置
      (unless (re-search-forward "^#\\+LATEX_CLASS:" nil t)
        ;; 如果没有，在适当位置添加
        (goto-char (point-min))
        (if (re-search-forward "^#\\+STARTUP:" nil t)
            (progn
              (end-of-line)
              (insert "\n#+LATEX_CLASS: journal")
              (when (called-interactively-p 'any)
                (save-buffer)))
          ;; 如果没有STARTUP行，在其他位置添加
          (goto-char (point-min))
          (if (re-search-forward "^#\\+OPTIONS:" nil t)
              (progn
                (end-of-line)
                (insert "\n#+LATEX_CLASS: journal")
                (when (called-interactively-p 'any)
                  (save-buffer)))
            ;; 如果都没有，在最前面添加
            (goto-char (point-min))
            (insert "#+LATEX_CLASS: journal\n")
            (when (called-interactively-p 'any)
              (save-buffer))))))
    (message "已为当前期刊文件设置1cm页边距的PDF导出")))

(defun org-journal-auto-setup-pdf ()
  "如果当前文件包含journal标签，自动设置PDF导出"
  (when (and (derived-mode-p 'org-mode)
             (or (save-excursion
                   (goto-char (point-min))
                   (re-search-forward ":journal:" nil t))
                 (string-match-p "journal\\|diary\\|worklog\\|studylog" 
                               (buffer-file-name))))
    (org-journal-setup-pdf-export)))

;; 自动为期刊文件设置正确的LaTeX类
(add-hook 'org-mode-hook 'org-journal-auto-setup-pdf)

(provide 'org-journal)

;;; org-journal.el ends here
