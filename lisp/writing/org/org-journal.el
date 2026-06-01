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
(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'ox)
(require 'subr-x)

;; =============================================================================
;; 轻量 GTD Agenda 配置

(defvar henri-agenda-directory
  (expand-file-name "agenda/" (expand-file-name henri-notes-directory))
  "Directory for Henri's GTD agenda files.")

(defun henri-agenda-file (name)
  "Return absolute path for agenda file NAME."
  (expand-file-name name henri-agenda-directory))

(defun henri-agenda-files ()
  "Return the task-focused files used by the main Org agenda."
  (append
   (mapcar #'henri-agenda-file
           '("inbox.org" "tasks.org" "projects.org" "someday.org"))
   (henri-agenda-roam-project-files)))

(defun henri-agenda-roam-project-files ()
  "Return Roam project files when `henri-roam-as-agenda-files' is enabled."
  (let ((project-dir (and (bound-and-true-p henri-roam-as-agenda-files)
                          (boundp 'henri-org-roam-directory)
                          (expand-file-name "projects" henri-org-roam-directory))))
    (if (and project-dir (file-directory-p project-dir))
        (directory-files project-dir t "\\.org\\'")
      nil)))

(defun henri-agenda--file-title (file)
  "Return a human-friendly title for agenda FILE."
  (pcase (file-name-base file)
    ("inbox" "Inbox")
    ("tasks" "Tasks")
    ("projects" "Projects")
    ("someday" "Someday")
    (_ (capitalize (file-name-base file)))))

(defun henri-agenda-ensure-files ()
  "Create Henri's agenda directory and core Org files when missing."
  (make-directory henri-agenda-directory t)
  (dolist (file (henri-agenda-files))
    (unless (file-exists-p file)
      (write-region
       (format "#+TITLE: %s\n#+STARTUP: content\n\n"
               (henri-agenda--file-title file))
       nil file nil 'silent))))

(defun henri-agenda-refresh-files ()
  "Refresh `org-agenda-files' from Henri's task-focused agenda files."
  (interactive)
  (henri-agenda-ensure-files)
  (setq org-agenda-files (henri-agenda-files)))

(defun henri-agenda-refresh-files-maybe ()
  "Refresh agenda files during interactive startup only."
  (unless noninteractive
    (henri-agenda-refresh-files)))

(defun henri/open-agenda-inbox ()
  "Open the GTD inbox file."
  (interactive)
  (henri-agenda-ensure-files)
  (find-file (henri-agenda-file "inbox.org")))

(defun henri/open-agenda-tasks ()
  "Open the GTD tasks file."
  (interactive)
  (henri-agenda-ensure-files)
  (find-file (henri-agenda-file "tasks.org")))

(defun henri/open-agenda-projects ()
  "Open the GTD projects file."
  (interactive)
  (henri-agenda-ensure-files)
  (find-file (henri-agenda-file "projects.org")))

;; 设置日志存放目录
(defvar henri-journal-directory
  (expand-file-name "Journal/" (expand-file-name henri-notes-directory))
  "Directory for Henri's Org journal files.")

(defcustom henri-journal-auto-save-delay 10
  "Seconds to wait after Journal edits before saving the visited file."
  :type 'integer
  :group 'henri-writing)

(defcustom henri-journal-expense-bills-directory
  (expand-file-name "bills/" henri-journal-directory)
  "Directory for generated monthly Journal expense bills."
  :type 'directory
  :group 'henri-writing)

(defcustom henri-journal-expense-heading-regexp "花销记录"
  "Regexp matching Journal headings that contain expense tables."
  :type 'regexp
  :group 'henri-writing)

(defcustom henri-journal-large-transaction-threshold 300
  "Minimum single expense amount collected as a large Journal transaction."
  :type 'number
  :group 'henri-writing)

(defcustom henri-journal-large-transactions-file
  (expand-file-name "large-transactions.org" henri-journal-expense-bills-directory)
  "Single Org file collecting large income and expense records."
  :type 'file
  :group 'henri-writing)

(defcustom henri-journal-day-status-template
  "- 天气：温度  ℃；状况\n  - [ ] 晴\n  - [ ] 阴\n  - [ ] 雨\n- 睡眠：\n  - 质量：\n  - 时间长度：\n- 体重： kg\n- 活动场所：\n- 大额收支\n  - 收入：项目 金额 类别/备注\n  - 支出：项目 金额 类别/备注\n\n"
  "Template inserted once under a newly created Journal day heading."
  :type 'string
  :group 'henri-writing)

(defun henri-journal-file (name)
  "Return absolute path for journal file NAME."
  (expand-file-name name henri-journal-directory))

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

(defun henri-journal--roam-daily-buffer-p (buffer)
  "Return non-nil when BUFFER visits a Roam daily file."
  (and (buffer-live-p buffer)
       (boundp 'henri-org-roam-directory)
       (with-current-buffer buffer
         (and buffer-file-name
              (file-in-directory-p
               (file-truename buffer-file-name)
               (file-truename
                (expand-file-name "daily" henri-org-roam-directory)))))))

(defun henri-journal-warn-when-capturing-from-roam-daily ()
  "Warn when a Journal capture is started from a Roam daily buffer."
  (when (and (boundp 'org-capture-plist)
             (member (plist-get org-capture-plist :key) '("d" "w" "l")))
    (let ((origin (plist-get org-capture-plist :original-buffer)))
      (when (henri-journal--roam-daily-buffer-p origin)
        (message "[henri] 当前在 Roam daily 中；知识线索优先考虑继续写 Roam daily。")))))

(defun henri-journal--goto-or-create-heading (level title &optional limit)
  "Go to heading TITLE at LEVEL before LIMIT, creating it when absent.
Return non-nil when the heading was newly created."
  (let ((regexp (format "^\\*\\{%d\\}[ \t]+%s[ \t]*$"
                        level
                        (regexp-quote title))))
    (if (re-search-forward regexp limit t)
        (progn
          (goto-char (match-beginning 0))
          nil)
      (goto-char (or limit (point-max)))
      (unless (bolp) (insert "\n"))
      (let ((pos (point)))
        (insert (make-string level ?*) " " title "\n")
        (goto-char pos)
        t))))

(defun henri-journal-insert-day-status-template ()
  "Insert the daily status template below the current day heading."
  (save-excursion
    (end-of-line)
    (insert "\n" henri-journal-day-status-template)))

(defun henri-journal-goto-month-day ()
  "Move point to this capture's month/day subtree, creating it if needed.
The journal file itself is already monthly, so the first heading is the
month and the second heading is the day.

Leaves point ON the day heading line, so that `org-capture' detects
`target-entry-p' and inserts the captured entry as a child of the day
(i.e. at level 3) rather than falling back to a top-level entry."
  (let* ((time (henri-journal-capture-time))
         (month (henri-journal-month-heading time))
         (day (henri-journal-day-heading time)))
    (henri-journal-warn-when-capturing-from-roam-daily)
    (goto-char (point-min))
    (henri-journal--goto-or-create-heading 1 month)
    (let ((month-start (point)))
      (org-end-of-subtree t t)
      (let ((month-end (point)))
        (goto-char month-start)
        (forward-line 1)
        (when (henri-journal--goto-or-create-heading 2 day month-end)
          (henri-journal-insert-day-status-template))))))

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
  "Return display label for diary monthly KIND (`diary)."
  (pcase kind
    ('diary "个人日记")
    (_ "个人日记")))

(defun henri-journal-monthly-file (kind &optional time)
  "Return monthly `journal-YYYY-MM.org' path for TIME.
Symbol KIND must be `diary; diary/work/study captures all write here."
  (unless (eq kind 'diary)
    (error "henri-journal-monthly-file: only diary is supported (%S)" kind))
  (henri-journal-file
   (format "journal-%s.org" (henri-journal-month-string time))))

(defun henri-journal-ensure-monthly-file (kind &optional time)
  "Create if missing and return the monthly `journal-*.org' path.
KIND must be `diary' (shared file for all journal capture types)."
  (unless (eq kind 'diary)
    (error "henri-journal-ensure-monthly-file: only diary is supported (%S)" kind))
  (let ((file (henri-journal-monthly-file kind time)))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (write-region
       (format "#+TITLE: %s %s\n#+LATEX_CLASS: journal\n#+OPTIONS: toc:t num:nil H:5\n#+STARTUP: content\n"
               (henri-journal-kind-label kind)
               (henri-journal-month-string time))
       nil file nil 'silent))
    file))

(defun henri-journal-current-diary-file ()
  "Return current month's unified journal file (diary / work / study)."
  (henri-journal-ensure-monthly-file 'diary))

(defun henri-journal-agenda-files ()
  "Monthly `journal-YYYY-MM.org' files under `henri-journal-directory'."
  (delete-dups
   (cons
    (henri-journal-ensure-monthly-file 'diary)
    (directory-files
     henri-journal-directory t
     "\\`journal-[0-9]\\{4\\}-[0-9]\\{2\\}\\.org\\'"))))

(defun henri-journal-refresh-agenda-files ()
  "Refresh task agenda files and ensure the current monthly journal exists.
Journal files stay out of the main `org-agenda-files' and are used only by
Journal-specific custom agenda commands."
  (interactive)
  (henri-journal-ensure-monthly-file 'diary)
  (henri-agenda-refresh-files))

(setq org-agenda-files (henri-agenda-files))
(add-hook 'after-init-hook #'henri-agenda-refresh-files-maybe)

(setq org-default-notes-file (henri-agenda-file "inbox.org"))

(defun henri-org-capture-register-templates (templates)
  "Register org-capture TEMPLATES by replacing existing entries with same key."
  (let ((keys (mapcar #'car templates)))
    (setq org-capture-templates
          (append
           (cl-remove-if (lambda (template)
                           (member (car-safe template) keys))
                         (bound-and-true-p org-capture-templates))
           templates))))

;; 轻量 GTD + 统一日志模板。
;; diary / work / study 写入同一月度 journal；模板与 tag 区分。
(henri-org-capture-register-templates
 `(("t" "快速 TODO" entry (file ,(henri-agenda-file "inbox.org"))
    "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n"
    :empty-lines 1)

   ("p" "项目 TODO" entry (file ,(henri-agenda-file "projects.org"))
    "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n"
    :empty-lines 1)

   ("s" "Someday" entry (file ,(henri-agenda-file "someday.org"))
    "* TODO %? :SOMEDAY:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n"
    :empty-lines 1)

   ("d" "个人日记" entry (file+function henri-journal-current-diary-file henri-journal-goto-month-day)
    "*** %U %? :journal:diary:\n%i\n**** 今日要点\n\n**** 花销记录\n| 项目 | 金额 | 类别 | 详情 |\n|------+------+------+------|\n|      |      |      |      |\n"
    :empty-lines 1)

   ("w" "工作记录" entry (file+function henri-journal-current-diary-file henri-journal-goto-month-day)
    "*** %U %? :journal:work:\n%i\n**** 工作任务描述\n\n**** 要点\n\n**** 待办记录\n"
    :empty-lines 1)

   ("l" "学习卡片" entry (file+function henri-journal-current-diary-file henri-journal-goto-month-day)
    "*** %U %? :journal:study:\n%i\n**** 主题\n\n**** 概念\n\n**** 解读\n\n**** 类别\n\n"
    :empty-lines 1)))

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
(defun henri/view-journal-by-date (&optional journal-type date)
  "在同一月度 journal 文件中打开 DATE 当天的 subtree。
JOURNAL-TYPE diary / work / study 现为同一跳转（日历入口保留类型选择）。"
  (interactive
   (list (completing-read "视角 (均打开同一天 journal): "
                          '("diary" "work" "study") nil t)
         (org-read-date nil nil nil "选择日期: ")))
  (ignore journal-type)
  (let* ((time (org-time-string-to-time date))
         (journal-file (henri-journal-monthly-file 'diary time))
         (date-prefix (format-time-string "%Y-%m-%d" time)))
    (if (not (file-exists-p journal-file))
        (message "未找到月份 journal 文件: %s" journal-file)
      (find-file journal-file)
      (widen)
      (goto-char (point-min))
      (if (re-search-forward (format "^\\*+[ \t]+%s\\b"
                                     (regexp-quote date-prefix))
                             nil t)
          (progn
            (org-reveal)
            (cond ((fboundp 'org-fold-show-subtree) (org-fold-show-subtree))
                  ((fboundp 'org-show-subtree) (org-show-subtree)))
            (recenter))
        (message "未找到 %s 的 journal day" date-prefix)))))

;; 便捷函数 - 直接查看个人日记
(defun henri/view-diary-by-date (&optional date)
  "通过选择日期查看特定日期的个人日记"
  (interactive)
  (let ((date (or date (org-read-date nil nil nil "选择日期: "))))
    (henri/view-journal-by-date "diary" date)))

;; 添加通用日志搜索功能
(defun henri/search-journal ()
  "在所有日志中搜索关键词"
  (interactive)
  (let ((keyword (read-string "搜索关键词: ")))
    (org-search-view nil keyword)))

;; =============================================================================
;; Journal 花销账单

(defun henri-journal-expense-bill-file (month)
  "Return generated expense bill path for MONTH (YYYY-MM)."
  (expand-file-name (format "bill-%s.org" month)
                    henri-journal-expense-bills-directory))

(defun henri-journal-expense--month-from-file-name (&optional file)
  "Return YYYY-MM parsed from journal or bill FILE name."
  (when-let ((name (and (or file buffer-file-name)
                        (file-name-nondirectory (or file buffer-file-name)))))
    (when (string-match
           "\\`\\(?:journal\\|bill\\)-\\([0-9]\\{4\\}-[0-9]\\{2\\}\\)\\.org\\'"
           name)
      (match-string 1 name))))

(defun henri-journal-expense--available-months ()
  "Return months that have monthly Journal files."
  (when (file-directory-p henri-journal-directory)
    (sort
     (mapcar (lambda (file)
               (string-remove-prefix
                "journal-"
                (file-name-sans-extension (file-name-nondirectory file))))
             (directory-files
              henri-journal-directory t
              "\\`journal-[0-9]\\{4\\}-[0-9]\\{2\\}\\.org\\'"))
     #'string<)))

(defun henri-journal-expense--default-month ()
  "Return the default month for expense bill generation."
  (or (henri-journal-expense--month-from-file-name)
      (henri-journal-month-string)))

(defun henri-journal-expense--parse-month (month)
  "Return a time value for MONTH in YYYY-MM form."
  (unless (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)\\'" month)
    (user-error "月份格式应为 YYYY-MM: %s" month))
  (encode-time 0 0 0 1
               (string-to-number (match-string 2 month))
               (string-to-number (match-string 1 month))))

(defun henri-journal-expense--previous-date (date)
  "Return the date before DATE (YYYY-MM-DD)."
  (unless (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'" date)
    (user-error "日期格式应为 YYYY-MM-DD: %s" date))
  (format-time-string
   "%Y-%m-%d"
   (time-subtract
    (encode-time 0 0 0
                 (string-to-number (match-string 3 date))
                 (string-to-number (match-string 2 date))
                 (string-to-number (match-string 1 date)))
    (days-to-time 1))))

(defun henri-journal-expense--table-row-cells (line)
  "Return trimmed Org table cells from LINE, or nil when LINE is not data."
  (when (string-match-p "\\`[ \t]*|" line)
    (unless (string-match-p "\\`[ \t]*|[-+ \t]+|[ \t]*\\'" line)
      (mapcar #'string-trim
              (split-string
               (replace-regexp-in-string
                "\\`[ \t]*|\\|[ \t]*|[ \t]*\\'" "" line)
               "|")))))

(defun henri-journal-expense--header-index (header name)
  "Return zero-based index of NAME in HEADER."
  (cl-position name header :test #'string=))

(defun henri-journal-expense--parse-amount (value)
  "Parse expense amount VALUE, returning a number or nil."
  (let ((text (replace-regexp-in-string "[,，[:space:]]+" "" value)))
    (when (string-match "\\`[+-]?[0-9]+\\(?:\\.[0-9]+\\)?\\'" text)
      (string-to-number text))))

(defun henri-journal-expense--format-amount (amount)
  "Format AMOUNT compactly for Org tables."
  (let ((text (format "%.2f" amount)))
    (setq text (replace-regexp-in-string "\\.?0+\\'" "" text))
    (if (string-empty-p text) "0" text)))

(defun henri-journal-expense--sanitize-cell (value)
  "Return VALUE safe enough for an Org table cell."
  (replace-regexp-in-string "|" "¦" (or value "")))

(defun henri-journal-large-transaction--from-text (date type text)
  "Return a large transaction parsed from DATE, TYPE, and TEXT.
TEXT should use: item amount category/notes."
  (let ((content (string-trim (or text ""))))
    (when (and date
               (not (string-empty-p content))
               (string-match "\\([0-9]+\\(?:[.,][0-9]+\\)?\\)" content))
      (let* ((amount-text (match-string 1 content))
             (item (string-trim (substring content 0 (match-beginning 1))))
             (category (string-trim (substring content (match-end 1))))
             (amount (henri-journal-expense--parse-amount amount-text))
             )
        (when amount
          (list :date date
                :type type
                :item (if (string-empty-p item) "未命名" item)
                :amount amount
                :category category
                :source "大额收支"))))))

(defun henri-journal-expense--collect-from-file (file)
  "Collect expense entries from monthly journal FILE.
Return a plist with :entries and :invalid-count."
  (let ((entries nil)
        (large-transactions nil)
        (invalid-count 0)
        current-date
        expense-date
        expense-source
        header
        item-index
        amount-index
        category-index
        in-expense-table
        in-large-transaction-block)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (cond
           ((string-match "^\\*\\{2\\}[ \t]+\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\b" line)
            (setq current-date (match-string 1 line)
                  in-expense-table nil
                  in-large-transaction-block nil
                  header nil))
           ((string-match "^\\*+[ \t]+\\(.+\\)$" line)
            (let ((heading (match-string 1 line)))
              (setq in-expense-table
                    (string-match-p henri-journal-expense-heading-regexp heading)
                    in-large-transaction-block nil
                    expense-source (string-trim heading)
                    expense-date (cond
                                  ((not current-date) nil)
                                  ((string-match-p "昨日" heading)
                                   (henri-journal-expense--previous-date current-date))
                                  (t current-date))
                    header nil
                    item-index nil
                    amount-index nil
                    category-index nil)))
           ((string-match "^[ \t]*-[ \t]+大额收支[ \t]*$" line)
            (setq in-large-transaction-block t
                  in-expense-table nil
                  header nil))
           ((and in-large-transaction-block
                 (string-match "^[ \t]+-[ \t]+\\(收入\\|支出\\)[：:][ \t]*\\(.*\\)$" line))
            (when-let ((transaction
                        (henri-journal-large-transaction--from-text
                         current-date
                         (match-string 1 line)
                         (match-string 2 line))))
              (push transaction large-transactions)))
           ((and in-large-transaction-block
                 (string-match "^[^ \t]" line))
            (setq in-large-transaction-block nil))
           (in-expense-table
            (let ((cells (henri-journal-expense--table-row-cells line)))
              (cond
               ((and cells
                     (member "项目" cells)
                     (member "金额" cells)
                     (member "类别" cells))
                (setq header cells
                      item-index (henri-journal-expense--header-index header "项目")
                      amount-index (henri-journal-expense--header-index header "金额")
                      category-index (henri-journal-expense--header-index header "类别")))
               ((and cells header item-index amount-index category-index expense-date)
                (let* ((item (or (nth item-index cells) ""))
                       (amount-text (or (nth amount-index cells) ""))
                       (category (or (nth category-index cells) ""))
                       (amount (and amount-text
                                    (henri-journal-expense--parse-amount amount-text))))
                  (unless (and (string-empty-p item)
                               (or (null amount-text) (string-empty-p amount-text))
                               (string-empty-p category))
                    (if amount
                        (let ((entry (list :date expense-date
                                           :item item
                                           :amount amount
                                           :category category
                                           :source expense-source)))
                          (push entry entries)
                          (when (> amount henri-journal-large-transaction-threshold)
                            (push (append (list :type "支出") entry)
                                  large-transactions)))
                      (cl-incf invalid-count)))))))))
          (forward-line 1))))
    (list :entries (nreverse entries)
          :large-transactions (nreverse large-transactions)
          :invalid-count invalid-count)))

(defun henri-journal-expense--summarize (entries key)
  "Summarize ENTRIES by plist KEY."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (let ((name (or (plist-get entry key) "")))
        (puthash name
                 (+ (gethash name table 0)
                    (plist-get entry :amount))
                 table)))
    (sort
     (let (rows)
       (maphash (lambda (name amount)
                  (push (cons name amount) rows))
                table)
       rows)
     (lambda (a b) (string< (car a) (car b))))))

(defun henri-journal-expense--insert-detail-table (entries)
  "Insert detail table for expense ENTRIES."
  (insert "#+ATTR_LATEX: :environment longtable :align p{2.2cm}p{1.7cm}r p{5.2cm}p{2.3cm}\n")
  (insert "| 日期 | 项目 | 金额 | 类别 | 来源 |\n")
  (insert "|------+------+------+------+------|\n")
  (dolist (entry entries)
    (insert
     (format "| %s | %s | %s | %s | %s |\n"
             (plist-get entry :date)
             (henri-journal-expense--sanitize-cell (plist-get entry :item))
             (henri-journal-expense--format-amount (plist-get entry :amount))
             (henri-journal-expense--sanitize-cell (plist-get entry :category))
             (henri-journal-expense--sanitize-cell (plist-get entry :source))))))

(defun henri-journal-expense--insert-summary-table (rows label)
  "Insert summary ROWS with first column LABEL."
  (insert "#+ATTR_LATEX: :environment longtable :align p{7cm}r\n")
  (insert (format "| %s | 金额 |\n" label))
  (insert "|------+------|\n")
  (dolist (row rows)
    (insert
     (format "| %s | %s |\n"
             (henri-journal-expense--sanitize-cell (car row))
             (henri-journal-expense--format-amount (cdr row))))))

(defun henri-journal-expense--insert-daily-total-table (entries total)
  "Insert daily totals for ENTRIES, ending with TOTAL."
  (insert "#+ATTR_LATEX: :environment longtable :align p{7cm}r\n")
  (insert "| 日期 | 金额 |\n")
  (insert "|------+------|\n")
  (dolist (row (henri-journal-expense--summarize entries :date))
    (insert
     (format "| %s | %s |\n"
             (henri-journal-expense--sanitize-cell (car row))
             (henri-journal-expense--format-amount (cdr row)))))
  (insert "|------+------|\n")
  (insert (format "| 总计 | %s |\n"
                  (henri-journal-expense--format-amount total))))

(defun henri-journal-large-transaction--sort (transactions)
  "Return TRANSACTIONS sorted by date and type."
  (sort (copy-sequence transactions)
        (lambda (a b)
          (let ((date-a (plist-get a :date))
                (date-b (plist-get b :date)))
            (if (string= date-a date-b)
                (string< (plist-get a :type) (plist-get b :type))
              (string< date-a date-b))))))

(defun henri-journal-large-transaction--insert-table (transactions)
  "Insert an Org table for large TRANSACTIONS."
  (insert "#+ATTR_LATEX: :environment longtable :align p{2.2cm}p{1.2cm}p{3cm}r p{4cm}p{2cm}\n")
  (insert "| 日期 | 类型 | 项目 | 金额 | 类别/备注 | 来源 |\n")
  (insert "|------+------+------+------+-----------+------|\n")
  (dolist (transaction (henri-journal-large-transaction--sort transactions))
    (insert
     (format "| %s | %s | %s | %s | %s | %s |\n"
             (plist-get transaction :date)
             (henri-journal-expense--sanitize-cell (plist-get transaction :type))
             (henri-journal-expense--sanitize-cell (plist-get transaction :item))
             (henri-journal-expense--format-amount (plist-get transaction :amount))
             (henri-journal-expense--sanitize-cell (plist-get transaction :category))
             (henri-journal-expense--sanitize-cell (plist-get transaction :source))))))

(defun henri-journal-large-transaction--read-existing-file ()
  "Read existing large transaction records from the global file."
  (let (transactions)
    (when (file-exists-p henri-journal-large-transactions-file)
      (with-temp-buffer
        (insert-file-contents henri-journal-large-transactions-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
                 (cells (henri-journal-expense--table-row-cells line)))
            (when (and cells
                       (= (length cells) 6)
                       (not (member "日期" cells)))
              (let ((amount (henri-journal-expense--parse-amount (nth 3 cells))))
                (when amount
                  (push (list :date (nth 0 cells)
                              :type (nth 1 cells)
                              :item (nth 2 cells)
                              :amount amount
                              :category (nth 4 cells)
                              :source (nth 5 cells))
                        transactions))))
            (forward-line 1)))))
    (nreverse transactions)))

(defun henri-journal-large-transaction--merge-month (month transactions)
  "Merge TRANSACTIONS for MONTH into the existing global records."
  (henri-journal-large-transaction--sort
   (append
    (cl-remove-if
     (lambda (transaction)
       (string-prefix-p month (or (plist-get transaction :date) "")))
     (henri-journal-large-transaction--read-existing-file))
    transactions)))

(defun henri-journal-large-transaction--render-file (transactions)
  "Return Org text for the global large TRANSACTIONS file."
  (with-temp-buffer
    (insert "#+TITLE: 大额收支管理\n")
    (insert "#+LATEX_CLASS: ctexart\n")
    (insert "#+LATEX_HEADER: \\usepackage{longtable}\n")
    (insert "#+OPTIONS: toc:t num:nil H:3\n")
    (insert "#+STARTUP: content\n\n")
    (insert "* 大额收支记录\n")
    (if transactions
        (henri-journal-large-transaction--insert-table transactions)
      (insert "暂无大额收支记录。\n"))
    (buffer-string)))

(defun henri-journal-large-transaction-write-file (month transactions)
  "Update MONTH in the large transaction management file with TRANSACTIONS."
  (let ((merged-transactions
         (henri-journal-large-transaction--merge-month month transactions)))
    (make-directory (file-name-directory henri-journal-large-transactions-file) t)
    (write-region
     (henri-journal-large-transaction--render-file merged-transactions)
     nil henri-journal-large-transactions-file nil 'silent)
    merged-transactions))

(defun henri-journal-expense--render-bill (month entries large-transactions invalid-count)
  "Return Org bill text for MONTH from ENTRIES."
  (let ((total (cl-loop for entry in entries sum (plist-get entry :amount))))
    (with-temp-buffer
      (insert (format "#+TITLE: %s 花销账单\n" month))
      (insert "#+LATEX_CLASS: ctexart\n")
      (insert "#+LATEX_HEADER: \\usepackage{longtable}\n")
      (insert "#+OPTIONS: toc:t num:nil H:3\n")
      (insert "#+STARTUP: content\n\n")
      (insert "* 明细\n")
      (if entries
          (henri-journal-expense--insert-detail-table entries)
        (insert "本月暂无花销记录。\n"))
      (insert "\n* 按项目汇总\n")
      (henri-journal-expense--insert-summary-table
       (henri-journal-expense--summarize entries :item)
       "项目")
      (insert "\n* 按类别汇总\n")
      (henri-journal-expense--insert-summary-table
       (henri-journal-expense--summarize entries :category)
       "类别")
      (insert "\n* 每日汇总\n")
      (henri-journal-expense--insert-daily-total-table entries total)
      (insert "\n* 大额收支\n")
      (if large-transactions
          (henri-journal-large-transaction--insert-table large-transactions)
        (insert "本月暂无大额收支记录。\n"))
      (when (> invalid-count 0)
        (insert (format "\n* 跳过记录\n%d 条花销记录金额无法解析，已跳过。\n"
                        invalid-count)))
      (buffer-string))))

(defun henri-journal-expense-generate-bill (month)
  "Generate monthly expense bill for MONTH (YYYY-MM), returning output file."
  (let* ((time (henri-journal-expense--parse-month month))
         (journal-file (henri-journal-monthly-file 'diary time))
         (bill-file (henri-journal-expense-bill-file month)))
    (unless (file-exists-p journal-file)
      (user-error "未找到月份 journal 文件: %s" journal-file))
    (let* ((result (henri-journal-expense--collect-from-file journal-file))
           (entries (plist-get result :entries))
           (large-transactions (plist-get result :large-transactions))
           (invalid-count (plist-get result :invalid-count)))
      (make-directory henri-journal-expense-bills-directory t)
      (write-region
       (henri-journal-expense--render-bill
        month entries large-transactions invalid-count)
       nil bill-file nil 'silent)
      (henri-journal-large-transaction-write-file month large-transactions)
      (message "已生成 %s（%d 条，跳过 %d 条）"
               bill-file (length entries) invalid-count)
      bill-file)))

(defun henri/journal-expense-generate-current-month ()
  "Generate expense bill for the current monthly Journal."
  (interactive)
  (find-file
   (henri-journal-expense-generate-bill
    (henri-journal-month-string))))

(defun henri/journal-expense-generate-month (month)
  "Generate expense bill for selected MONTH.
When called from a journal or bill buffer, default to that buffer's month."
  (interactive
   (let* ((months (henri-journal-expense--available-months))
          (default-month (henri-journal-expense--default-month)))
     (list
      (completing-read
       (format "生成账单月份（默认 %s）: " default-month)
       months nil nil nil nil default-month))))
  (find-file (henri-journal-expense-generate-bill month)))

(defun henri/journal-expense-regenerate-all ()
  "Regenerate expense bills for all monthly Journal files."
  (interactive)
  (let ((months (henri-journal-expense--available-months)))
    (dolist (month months)
      (henri-journal-expense-generate-bill month))
    (message "已重建 %d 个月度花销账单" (length months))))

;; =============================================================================
;; Agenda 自定义视图

;; 统一 Agenda 视图名称和结构
(setq org-agenda-custom-commands
      '(("d" "今日 Dashboard"
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-start-day nil)
                      (org-agenda-overriding-header "今日安排:")))
          (todo "DOING"
                ((org-agenda-overriding-header "进行中:")))
          (todo "WAITING"
                ((org-agenda-overriding-header "等待中:")))
          (tags-todo "CATEGORY=\"inbox\""
                     ((org-agenda-overriding-header "Inbox:"))))
         ((org-agenda-compact-blocks t)))

        ("w" "本周计划"
         ((agenda "" ((org-agenda-span 'week)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-overriding-header "本周安排:")))
          (todo "DOING"
                ((org-agenda-overriding-header "进行中:")))
          (todo "WAITING"
                ((org-agenda-overriding-header "等待中:"))))
         ((org-agenda-compact-blocks t)))

        ("p" "项目任务"
         ((tags-todo "CATEGORY=\"projects\""
                     ((org-agenda-overriding-header "项目任务:"))))
         ((org-agenda-compact-blocks t)))

        ("i" "Inbox 清理"
         ((tags-todo "CATEGORY=\"inbox\""
                     ((org-agenda-overriding-header "Inbox:"))))
         ((org-agenda-compact-blocks t)))

        ("j" "日志概览"
         ((agenda "" ((org-agenda-span 'week)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-show-all-dates t)
                      (org-agenda-files (henri-journal-agenda-files))))
          (tags "diary"
                ((org-agenda-sorting-strategy '(time-up priority-down))
                 (org-agenda-files (henri-journal-agenda-files))
                 (org-agenda-prefix-format "  %i %?-12t% s")
                 (org-agenda-overriding-header "📔 个人日记:")))
          (tags "work"
                ((org-agenda-sorting-strategy '(time-up priority-down))
                 (org-agenda-files (henri-journal-agenda-files))
                 (org-agenda-prefix-format "  %i %?-12t% s")
                 (org-agenda-overriding-header "💼 工作记录:")))
          (tags "study"
                ((org-agenda-sorting-strategy '(time-up priority-down))
                 (org-agenda-files (henri-journal-agenda-files))
                 (org-agenda-prefix-format "  %i %?-12t% s")
                 (org-agenda-overriding-header "📚 学习卡片:"))))
         ((org-agenda-compact-blocks t)))

        ("J" "个人日记"
         ((tags "diary"
                ((org-agenda-sorting-strategy '(time-up priority-down))
                 (org-agenda-files (henri-journal-agenda-files))
                 (org-agenda-overriding-header "📔 个人日记条目:"))))
         ((org-agenda-compact-blocks t)))

        ("W" "工作记录"
         ((tags "work"
                ((org-agenda-sorting-strategy '(time-up priority-down))
                 (org-agenda-files (henri-journal-agenda-files))
                 (org-agenda-overriding-header "💼 工作记录:"))))
         ((org-agenda-compact-blocks t)))

        ("L" "学习卡片"
         ((tags "study"
                ((org-agenda-sorting-strategy '(time-up priority-down))
                 (org-agenda-files (henri-journal-agenda-files))
                 (org-agenda-overriding-header "📚 学习卡片:"))))
         ((org-agenda-compact-blocks t)))))

;; =============================================================================
;; 日历集成

;; 优化日历集成功能
(defun henri/calendar-open-journal ()
  "在日历中选择日期后打开对应的日志条目"
  (interactive)
  (let* ((date (calendar-cursor-to-date))
         (day (nth 1 date))
         (month (nth 0 date))
         (year (nth 2 date))
         (date-str (format "%04d-%02d-%02d" year month day))
         (journal-type (completing-read "视角 (均打开同一天 journal): "
                                        '(("个人日记" . "diary")
                                          ("工作记录" . "work")
                                          ("学习卡片" . "study"))
                                        nil t)))
    (henri/view-journal-by-date
     (cond ((string= journal-type "个人日记") "diary")
           ((string= journal-type "工作记录") "work")
           ((string= journal-type "学习卡片") "study")
           (t journal-type))
     date-str)))

(add-hook 'calendar-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'henri/calendar-open-journal)))

;; =============================================================================
;; 快捷键设置

(defun henri/org-agenda-dashboard ()
  "Open Henri's daily GTD dashboard."
  (interactive)
  (org-agenda nil "d"))

(defun henri/today-summary ()
  "Open Journal, Roam daily, and Agenda as a three-pane daily review."
  (interactive)
  (let ((today (format-time-string "%Y-%m-%d")))
    (delete-other-windows)
    (henri/view-diary-by-date today)
    (split-window-right)
    (other-window 1)
    (if (fboundp 'org-roam-dailies-goto-today)
        (org-roam-dailies-goto-today)
      (message "org-roam dailies is not available"))
    (split-window-below)
    (other-window 1)
    (henri/org-agenda-dashboard)
    (balance-windows)))

(global-set-key (kbd "C-c c") 'org-capture)            ;; 快速捕获
(global-set-key (kbd "C-c a") 'org-agenda)             ;; 打开议程视图
(global-set-key (kbd "C-c o a") 'henri/org-agenda-dashboard) ;; 今日 Dashboard
(global-set-key (kbd "C-c o i") 'henri/open-agenda-inbox)     ;; 打开 Inbox
(global-set-key (kbd "C-c o t") 'henri/open-agenda-tasks)     ;; 打开 Tasks
(global-set-key (kbd "C-c o p") 'henri/open-agenda-projects)  ;; 打开 Projects
(global-set-key (kbd "C-c o s") 'henri/today-summary)         ;; 今日三栏总览
(global-set-key (kbd "C-c j s") 'henri/search-journal)      ;; 搜索日志
(global-set-key (kbd "C-c j d") 'henri/view-diary-by-date)  ;; 直接查看个人日记
(global-set-key (kbd "C-c j e") 'henri/journal-expense-generate-month) ;; 选择月份生成花销账单

;; =============================================================================
;; Journal LaTeX 文档类注册（从 org-latex.el 解耦）
;; 模板文件：lisp/writing/LaTeX/themes/journal/journal.cls

(defvar org-journal-theme-directory
  (expand-file-name "lisp/writing/LaTeX/themes/journal/" user-emacs-directory)
  "Directory containing the vendored Journal LaTeX class.")

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("journal"
                 "\\documentclass[UTF8,11pt]{ctexart}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\usepackage[open=true]{bookmark}
\\usepackage{fontspec}
\\usepackage{xeCJK}
\\setCJKmainfont{KingHwa_OldSong}[BoldFont=KingHwa_OldSong,ItalicFont=KingHwa_OldSong,AutoFakeBold=2,AutoFakeSlant=0.15]
\\setCJKsansfont{KingHwa_OldSong}
\\setCJKmonofont{KingHwa_OldSong}
\\usepackage[paperwidth=176mm,paperheight=250mm,top=2cm,bottom=2cm,left=2cm,right=2cm]{geometry}
\\linespread{1.15}
\\setlength{\\parskip}{0.4em}
\\setcounter{secnumdepth}{0}
\\setcounter{tocdepth}{3}
\\hypersetup{bookmarks=true,bookmarksopen=true,bookmarksnumbered=false,colorlinks=true,linkcolor=blue!60!black,urlcolor=blue!80!black}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section{%s}")
                 ("\\subsection{%s}" . "\\subsection{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection{%s}")
                 ("\\paragraph{%s}" . "\\paragraph{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph{%s}"))))

;; =============================================================================
;; 期刊PDF导出配置

(defun org-journal--journal-latex-export-p (backend info)
  "Return non-nil when BACKEND and INFO describe a Journal LaTeX export."
  (and (org-export-derived-backend-p backend 'latex)
       (or (string= (plist-get info :latex-class) "journal")
           (save-excursion
             (goto-char (point-min))
             (re-search-forward "^#\\+LATEX_CLASS:[ \t]+journal\\b" nil t))
           (and buffer-file-name
                (string-match-p "journal\\|diary" buffer-file-name)))))

(defun org-journal-apply-latex-export-options (options backend)
  "Force Journal diary PDF layout OPTIONS for LaTeX BACKEND exports."
  (when (org-journal--journal-latex-export-p backend options)
    (setq options (plist-put options :with-toc t))
    ;; Let Org emit normal section commands so LaTeX can build the TOC and
    ;; PDF outline.  The Journal class hides printed numbers via secnumdepth.
    (setq options (plist-put options :section-numbers t))
    (setq options (plist-put options :headline-levels 5)))
  options)

(add-hook 'org-export-filter-options-functions
          #'org-journal-apply-latex-export-options)


(provide 'org-journal)

;;; org-journal.el ends here
