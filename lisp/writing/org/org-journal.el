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
(defvar henri-journal-directory
  (expand-file-name "Journal/" (expand-file-name henri-notes-directory))
  "Directory for Henri's Org journal files.")

(defcustom henri-journal-auto-save-delay 10
  "Seconds to wait after Journal edits before saving the visited file."
  :type 'integer
  :group 'henri-writing)

(defun henri-journal-file (name)
  "Return absolute path for journal file NAME."
  (expand-file-name name henri-journal-directory))

(defun henri-journal-html-setupfile ()
  "Return default HTML setupfile for Journal exports."
  (let ((setup-file
         (expand-file-name
          "org/theme-henri-bearblog.setup"
          (file-name-as-directory
           (expand-file-name henri-org-html-themes-directory)))))
    (if (file-exists-p setup-file)
        setup-file
      (progn
        (message "[henri] Journal HTML theme missing: %s — run lisp/writing/org/install-themes.sh"
                 setup-file)
        nil))))

(defun henri-journal-month-string (&optional time)
  "Return YYYY-MM month string for TIME, or current time when nil."
  (format-time-string "%Y-%m" (or time (current-time))))

(defun henri-journal-month-heading (&optional time)
  "Return the monthly heading used inside a monthly journal file."
  (format-time-string "%Y-%m %B" (or time (current-time))))

(defun henri-journal-day-heading (&optional time)
  "Return the daily heading used inside a monthly journal file."
  (format-time-string "%Y-%m-%d %A" (or time (current-time))))

(defun henri-journal-capture-time ()
  "Return the time Org capture should use for journal placement."
  (or (and (boundp 'org-overriding-default-time)
           org-overriding-default-time)
      (current-time)))

(defun henri-journal--goto-or-create-heading (level title &optional limit)
  "Go to heading TITLE at LEVEL before LIMIT, creating it when absent."
  (let ((regexp (format "^\\*\\{%d\\}[ \t]+%s[ \t]*$"
                        level
                        (regexp-quote title))))
    (if (re-search-forward regexp limit t)
        (goto-char (match-beginning 0))
      (goto-char (or limit (point-max)))
      (unless (bolp) (insert "\n"))
      (let ((pos (point)))
        (insert (make-string level ?*) " " title "\n")
        (goto-char pos)))))

(defun henri-journal-goto-month-day ()
  "Move point to this capture's month/day subtree, creating it if needed.
The journal file itself is already monthly, so the first heading is the
month and the second heading is the day."
  (let* ((time (henri-journal-capture-time))
         (month (henri-journal-month-heading time))
         (day (henri-journal-day-heading time)))
    (goto-char (point-min))
    (henri-journal--goto-or-create-heading 1 month)
    (let ((month-start (point)))
      (org-end-of-subtree t t)
      (let ((month-end (point)))
        (goto-char month-start)
        (forward-line 1)
        (henri-journal--goto-or-create-heading 2 day month-end)
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))))))

(defun henri-journal-flatten-year-headings ()
  "Remove top-level year headings from the current monthly journal buffer.
Existing Org datetree entries like year/month/day become month/day/entry."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((changed nil))
        (while (re-search-forward "^\\* [0-9]\\{4\\}[ \t]*$" nil t)
          (let* ((year-start (line-beginning-position))
                 (content-start (line-beginning-position 2))
                 (year-end (save-excursion
                             (org-end-of-subtree t t)
                             (point))))
            (save-restriction
              (narrow-to-region content-start year-end)
              (goto-char (point-min))
              (while (re-search-forward "^\\(\\*\\{2,\\}\\)" nil t)
                (replace-match
                 (substring (match-string 1) 1)
                 t t nil 1)))
            (delete-region year-start content-start)
            (setq changed t)
            (goto-char year-start)))
        (when (called-interactively-p 'interactive)
          (message (if changed
                       "Journal year headings flattened."
                     "No top-level year headings found.")))))))

(defun henri-journal-kind-label (kind)
  "Return display label for journal KIND."
  (pcase kind
    ('diary "个人日记")
    ('work "工作日志")
    ('study "学习日志")
    (_ "日志")))

(defun henri-journal-kind-prefix (kind)
  "Return file prefix for journal KIND."
  (pcase kind
    ('diary "journal")
    ('work "worklog")
    ('study "studylog")
    (_ "journal")))

(defun henri-journal-monthly-file (kind &optional time)
  "Return monthly journal file for KIND and TIME.
KIND is one of `diary', `work', or `study'."
  (henri-journal-file
   (format "%s-%s.org"
           (henri-journal-kind-prefix kind)
           (henri-journal-month-string time))))

(defun henri-journal-ensure-monthly-file (kind &optional time)
  "Ensure and return monthly journal file for KIND and TIME."
  (let ((file (henri-journal-monthly-file kind time)))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (write-region
       (format "#+TITLE: %s %s\n#+LATEX_CLASS: journal\n#+STARTUP: content\n%s\n"
               (henri-journal-kind-label kind)
               (henri-journal-month-string time)
               (if-let ((setup-file (henri-journal-html-setupfile)))
                   (format "#+SETUPFILE: %s\n" setup-file)
                 ""))
       nil file nil 'silent))
    file))

(defun henri-journal-current-diary-file ()
  "Return current month's personal journal file."
  (henri-journal-ensure-monthly-file 'diary))

(defun henri-journal-current-worklog-file ()
  "Return current month's work journal file."
  (henri-journal-ensure-monthly-file 'work))

(defun henri-journal-current-studylog-file ()
  "Return current month's study journal file."
  (henri-journal-ensure-monthly-file 'study))

(defun henri-journal-agenda-files ()
  "Return monthly Journal files suitable for `org-agenda-files'."
  (append
   (mapcar (lambda (kind) (henri-journal-ensure-monthly-file kind))
           '(diary work study))
   (directory-files henri-journal-directory t
                    "\\`\\(journal\\|worklog\\|studylog\\)-[0-9]\\{4\\}-[0-9]\\{2\\}\\.org\\'")))

(defun henri-journal-refresh-agenda-files ()
  "Refresh `org-agenda-files' from monthly Journal files."
  (interactive)
  (setq org-agenda-files (delete-dups (henri-journal-agenda-files))))

(setq org-directory henri-journal-directory)
(setq org-default-notes-file (henri-journal-file "notes.org"))

;; 统一日志模板 - 三种类型：日记(diary)、工作(work)和学习(study)
(setq org-capture-templates
      '(("d" "个人日记" entry (file+function henri-journal-current-diary-file henri-journal-goto-month-day)
         "*** %U %? :journal:diary:\n%i\n**** 今日要点\n\n**** 花销记录\n| 项目 | 金额 | 类别 |\n|------+------+------|\n|      |      |      |\n"
         :empty-lines 1)
        
        ("w" "工作日志" entry (file+function henri-journal-current-worklog-file henri-journal-goto-month-day)
         "*** %U %? :journal:work:\n%i\n**** 完成任务\n\n**** 问题和解决方案\n\n**** 明日计划\n"
         :empty-lines 1)
        
        ("s" "学习日志" entry (file+function henri-journal-current-studylog-file henri-journal-goto-month-day)
         "*** %U %? :journal:study:\n%i\n**** 主题与工作\n\n**** 要点笔记\n\n**** 资源链接\n"
         :empty-lines 1)))

;; 设置 Org-mode 的 Agenda 文件 - 统一路径命名
(henri-journal-refresh-agenda-files)

;; Journal 自动保存 -----------------------------------------------------------
(defvar-local henri-journal--auto-save-timer nil
  "Idle timer used to save the current Journal buffer.")

(defun henri-journal-buffer-p (&optional buffer)
  "Return non-nil when BUFFER visits a file under `henri-journal-directory'."
  (with-current-buffer (or buffer (current-buffer))
    (and buffer-file-name
         (file-in-directory-p (file-truename buffer-file-name)
                              (file-truename henri-journal-directory)))))

(defun henri-journal-save-buffer (&optional buffer)
  "Save BUFFER when it is a modified Journal file."
  (with-current-buffer (or buffer (current-buffer))
    (when (and (henri-journal-buffer-p)
               (buffer-modified-p)
               buffer-file-name)
      (save-buffer))))

(defun henri-journal-save-all-buffers ()
  "Save all modified Journal buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (henri-journal-save-buffer buffer))))

(defun henri-journal-schedule-auto-save (&rest _)
  "Schedule a delayed save for the current Journal buffer."
  (when (henri-journal-buffer-p)
    (when (timerp henri-journal--auto-save-timer)
      (cancel-timer henri-journal--auto-save-timer))
    (setq henri-journal--auto-save-timer
          (run-with-timer
           henri-journal-auto-save-delay nil
           (lambda (buffer)
             (when (buffer-live-p buffer)
               (henri-journal-save-buffer buffer)))
           (current-buffer)))))

(defun henri-journal-enable-auto-save ()
  "Enable delayed visited-file auto-save for Journal org buffers."
  (when (henri-journal-buffer-p)
    (setq-local auto-save-default t)
    (auto-save-mode 1)
    (add-hook 'after-change-functions #'henri-journal-schedule-auto-save nil t)))

(add-hook 'org-mode-hook #'henri-journal-enable-auto-save)
(add-hook 'org-capture-after-finalize-hook
          (lambda ()
            (henri-journal-refresh-agenda-files)
            (henri-journal-save-all-buffers)))

;; =============================================================================
;; 日志查看和搜索功能

;; 通用日志查看函数 - 支持多种日志类型
(defun my/view-journal-by-date (&optional journal-type date)
  "通过选择日期查看特定日期的日志。
JOURNAL-TYPE 可以是 'diary'(个人日记), 'work'(工作日志) 或 'study'(学习日志)。"
  (interactive
   (list (completing-read "选择日志类型: " '("diary" "work" "study") nil t)
         (org-read-date nil nil nil "选择日期: ")))
  (let* ((time (org-time-string-to-time date))
         (journal-file (cond ((string= journal-type "work")
                              (henri-journal-monthly-file 'work time))
                             ((string= journal-type "study")
                              (henri-journal-monthly-file 'study time))
                             (t
                              (henri-journal-monthly-file 'diary time))))
         (date-prefix (format-time-string "%Y-%m-%d" time))
         (journal-label (cond ((string= journal-type "work") "工作日志")
                              ((string= journal-type "study") "学习日志")
                              (t "日记"))))
    (if (not (file-exists-p journal-file))
        (message "未找到月份日志文件: %s" journal-file)
      (find-file journal-file)
      (widen)
      (goto-char (point-min))
      (if (re-search-forward (format "^\\*+[ \t]+%s\\b" (regexp-quote date-prefix)) nil t)
          (progn
            (org-reveal)
            (org-show-subtree)
            (recenter))
        (message "未找到 %s 的%s条目" date-prefix journal-label)))))

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
                 (and buffer-file-name
                      (string-match-p "journal\\|diary\\|worklog\\|studylog"
                                      buffer-file-name))))
    (org-journal-setup-pdf-export)))

;; 自动为期刊文件设置正确的LaTeX类
(add-hook 'org-mode-hook 'org-journal-auto-setup-pdf)

(provide 'org-journal)

;;; org-journal.el ends here
