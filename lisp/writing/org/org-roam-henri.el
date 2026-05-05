;;; org-roam-henri.el --- Org-roam knowledge base setup -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Keywords: org, org-roam, writing, notes

;;; Commentary:

;; General-purpose Org-roam knowledge base for daily thinking, durable notes,
;; references, projects, people, and maps.  This module intentionally keeps
;; Agenda, Journal, and Academic writing as separate workflows.

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-element)
(require 'subr-x)

(defgroup henri-org-roam nil
  "General Org-roam knowledge base."
  :group 'henri-writing)

(defconst henri-org-roam--subdirectories
  '("inbox" "daily" "notes" "references" "projects" "people" "maps")
  "Purpose-based subdirectories under `henri-org-roam-directory'.")

(defconst henri-org-roam-lifecycle-tags
  '("seedling" "budding" "evergreen")
  "Lifecycle tags used for durable Org-roam notes.")

(defconst henri-org-roam--file-head
  "#+title: ${title}\n#+filetags: :%s:\n:PROPERTIES:\n:CREATED:  %%U\n:UPDATED:  %%U\n:END:\n\n"
  "File head template for regular Org-roam nodes.")

(defconst henri-org-roam--file-head-seedling
  "#+title: ${title}\n#+filetags: :%s:seedling:\n:PROPERTIES:\n:CREATED:  %%U\n:UPDATED:  %%U\n:END:\n\n"
  "File head template for Org-roam nodes that start as seedling (notes, inbox).")

(defconst henri-org-roam--daily-head
  "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n:PROPERTIES:\n:CREATED:  %U\n:UPDATED:  %U\n:END:\n\n* 今日记录\n\n* Done\n\n* 想法 (会沉淀为 notes)\n\n* 待抽取节点\n"
  "File head template for Org-roam daily notes.")

(defun henri-org-roam-directory ()
  "Return the normalized Org-roam root directory."
  (file-name-as-directory (file-truename henri-org-roam-directory)))

(defun henri-org-roam-file (relative-path)
  "Return RELATIVE-PATH under `henri-org-roam-directory'."
  (expand-file-name relative-path (henri-org-roam-directory)))

(defun henri-org-roam--timestamp ()
  "Return an inactive Org timestamp for metadata."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun henri-org-roam--buffer-file-in-roam-p ()
  "Return non-nil when the current buffer file is under Org-roam."
  (and buffer-file-name
       (file-in-directory-p (expand-file-name buffer-file-name)
                            (henri-org-roam-directory))))

(defun henri-org-roam--ensure-time-metadata ()
  "Ensure CREATED and UPDATED metadata exist in the file property drawer."
  (let ((timestamp (henri-org-roam--timestamp)))
    (save-excursion
      (goto-char (point-min))
      (org-id-get-create)
      (unless (org-entry-get (point-min) "CREATED")
        (org-entry-put (point-min) "CREATED" timestamp))
      (org-entry-put (point-min) "UPDATED" timestamp))))

(defun henri-org-roam-update-time-metadata ()
  "Refresh UPDATED metadata for Org-roam files before saving."
  (when (and (derived-mode-p 'org-mode)
             (henri-org-roam--buffer-file-in-roam-p))
    (henri-org-roam--ensure-time-metadata)))

(defun henri-org-roam-enable-buffer-metadata-hook ()
  "Enable Roam metadata maintenance for the current Org buffer."
  (when (henri-org-roam--buffer-file-in-roam-p)
    (add-hook 'before-save-hook #'henri-org-roam-update-time-metadata nil t)))

(defun henri-org-roam-ensure-directories ()
  "Create the Org-roam root and purpose-based subdirectories."
  (interactive)
  (dolist (dir (cons "" henri-org-roam--subdirectories))
    (let ((path (henri-org-roam-file dir)))
      (unless (file-directory-p path)
        (make-directory path t)))))

(defun henri-org-roam-ensure-directories-advice (&rest _)
  "Ensure Roam directories before interactive Roam entrypoints need them."
  (henri-org-roam-ensure-directories))

(defun henri-org-roam-open-directory ()
  "Open `henri-org-roam-directory' in Dired."
  (interactive)
  (henri-org-roam-ensure-directories)
  (dired (henri-org-roam-directory)))

(defun henri-org-roam-open-inbox ()
  "Open the Org-roam inbox directory."
  (interactive)
  (henri-org-roam-ensure-directories)
  (dired (henri-org-roam-file "inbox")))

(defun henri-org-roam--safe-slug (title)
  "Return a filesystem-safe slug for Org-roam TITLE."
  (let ((slug (replace-regexp-in-string "[^[:alnum:]\u4e00-\u9fff]+" "-" title)))
    (string-trim (downcase slug) "-+" "-+")))

(defun henri-org-roam--current-title ()
  "Return the current Org heading title or file title."
  (if (org-before-first-heading-p)
      (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE"))))
          (file-name-base (or buffer-file-name "Org note")))
    (org-get-heading t t t t)))

(defun henri-org-roam--filetags ()
  "Return current buffer filetags as a list."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+filetags:[ \t]*\\(.*\\)$" nil t)
      (split-string (match-string 1) ":" t "[ \t\n\r]+"))))

(defun henri-org-roam--set-filetags (tags)
  "Set current buffer filetags to TAGS."
  (save-excursion
    (goto-char (point-min))
    (let ((line (format "#+filetags: :%s:\n" (string-join tags ":"))))
      (if (re-search-forward "^#\\+filetags:.*$" nil t)
          (replace-match (string-trim-right line) t t)
        (while (looking-at "^#\\+title:.*$")
          (forward-line 1))
        (insert line)))))

(defun henri-org-roam-cycle-lifecycle-tag ()
  "Cycle the current Roam file through seedling, budding, and evergreen."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command works in Org buffers"))
  (let* ((tags (henri-org-roam--filetags))
         (current (seq-find (lambda (tag)
                              (member tag henri-org-roam-lifecycle-tags))
                            tags))
         (next (or (cadr (member current henri-org-roam-lifecycle-tags))
                   (car henri-org-roam-lifecycle-tags)))
         (clean-tags (seq-remove (lambda (tag)
                                   (member tag henri-org-roam-lifecycle-tags))
                                 tags)))
    (henri-org-roam--set-filetags (append clean-tags (list next)))
    (message "Org-roam lifecycle: %s" next)))

(defun henri-org-roam--subtree-body ()
  "Return the body text of the current Org subtree."
  (save-excursion
    (org-back-to-heading t)
    (let ((start (save-excursion
                   (forward-line 1)
                   (while (and (not (eobp))
                               (looking-at-p "^[ \t]*:\\(PROPERTIES\\|LOGBOOK\\):[ \t]*$"))
                     (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                       (forward-line 1)))
                   (point)))
          (end (save-excursion
                 (org-end-of-subtree t t)
                 (point))))
      (string-trim (buffer-substring-no-properties start end)))))

(defun henri-org-roam-extract-pending (title)
  "Extract the current daily/inbox heading into a durable Roam note TITLE."
  (interactive
   (progn
     (unless (org-at-heading-p)
       (org-back-to-heading t))
     (list (read-string "Extract to note title: " (henri-org-roam--current-title)))))
  (unless (and buffer-file-name
               (or (file-in-directory-p (file-truename buffer-file-name)
                                        (file-truename (henri-org-roam-file "daily")))
                   (file-in-directory-p (file-truename buffer-file-name)
                                        (file-truename (henri-org-roam-file "inbox")))))
    (user-error "Run this from a Roam daily or inbox heading"))
  (henri-org-roam-ensure-directories)
  (org-back-to-heading t)
  (let* ((source-buffer (current-buffer))
         (source-title (henri-org-roam--current-title))
         (source-id (org-id-get-create))
         (body (henri-org-roam--subtree-body))
         (target-title (if (string-empty-p title) source-title title))
         (target-file (henri-org-roam-file
                       (format "notes/%s.org"
                               (henri-org-roam--safe-slug target-title)))))
    (when (file-exists-p target-file)
      (user-error "Target note already exists: %s" target-file))
    (with-current-buffer (find-file-noselect target-file)
      (erase-buffer)
      (insert (format "#+title: %s\n#+filetags: :note:seedling:\n:PROPERTIES:\n:CREATED:  %s\n:UPDATED:  %s\n:END:\n\n"
                      target-title
                      (henri-org-roam--timestamp)
                      (henri-org-roam--timestamp)))
      (insert (format "- Source: [[id:%s][%s]]\n\n" source-id source-title))
      (unless (string-empty-p body)
        (insert body "\n"))
      (goto-char (point-min))
      (org-mode)
      (let ((target-id (org-id-get-create)))
        (save-buffer)
        (with-current-buffer source-buffer
          (org-back-to-heading t)
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (insert (format "- Extracted note: [[id:%s][%s]]\n" target-id target-title))
          (save-buffer))))
    (when (fboundp 'org-roam-db-sync)
      (org-roam-db-sync))
    (find-file target-file)
    (message "Extracted Roam note: %s" target-file)))

(defun henri-org-roam--package-available-p (package)
  "Return non-nil when PACKAGE can be loaded from the current load path."
  (locate-library (symbol-name package)))

(defun henri-org-roam--capture-template-entries (&optional templates)
  "Return key and description pairs from Org-roam TEMPLATES."
  (delq nil
        (mapcar (lambda (template)
                  (when (and (consp template)
                             (stringp (car template))
                             (stringp (cadr template))
                             (> (length template) 2))
                    (cons (car template) (cadr template))))
                (or templates org-roam-capture-templates))))

(defun henri-org-roam--capture-template-summary (&optional templates)
  "Return a compact summary for Org-roam TEMPLATES."
  (string-join
   (mapcar (lambda (entry)
             (format "%s=%s" (car entry) (cdr entry)))
           (henri-org-roam--capture-template-entries templates))
   "  "))

(defun henri-org-roam-show-template-keys ()
  "Show Org-roam capture template keys."
  (interactive)
  (with-help-window "*Henri Org-roam Templates*"
    (princ "Org-roam capture template keys\n\n")
    (princ "Regular node templates (`C-c n f` / `C-c n c`)\n\n")
    (dolist (entry (henri-org-roam--capture-template-entries))
      (princ (format "%-4s %s\n" (car entry) (cdr entry))))
    (when (boundp 'org-roam-dailies-capture-templates)
      (princ "\nDaily templates (`C-c n j`)\n\n")
      (dolist (entry (henri-org-roam--capture-template-entries
                      org-roam-dailies-capture-templates))
        (princ (format "%-4s %s\n" (car entry) (cdr entry)))))))

(defun henri-org-roam--message-template-hint (&rest args)
  "Show a compact template hint before Org-roam capture using ARGS."
  (unless (plist-get args :keys)
    (let ((summary (henri-org-roam--capture-template-summary
                    (plist-get args :templates))))
      (unless (string-empty-p summary)
        (message "Org-roam templates: %s" summary)))))

(use-package org-roam
  :ensure t
  :demand t
  :custom
  (org-roam-directory (henri-org-roam-directory))
  (org-roam-dailies-directory "daily/")
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:20}" 'face 'org-tag)))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c n c" . org-roam-capture)
         ("C-c n g" . org-roam-graph)
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n t" . org-roam-dailies-goto-today)
         ("C-c n y" . org-roam-dailies-goto-yesterday)
         ("C-c n m" . org-roam-dailies-goto-tomorrow)
         ("C-c n E" . henri-org-roam-extract-pending)
         ("C-c n v" . henri-org-roam-cycle-lifecycle-tag)
         ("C-c n d" . henri-org-roam-open-directory)
         ("C-c n x" . henri-org-roam-open-inbox)
         ("C-c n ?" . henri-org-roam-show-template-keys))
  :config
  (require 'org-roam-dailies)
  (setq org-roam-dailies-directory "daily/")
  (dolist (fn '(org-roam-capture
                org-roam-node-find
                org-roam-dailies-capture-today
                org-roam-dailies-goto-today
                org-roam-dailies-goto-yesterday
                org-roam-dailies-goto-tomorrow))
    (advice-remove fn #'henri-org-roam-ensure-directories-advice)
    (advice-add fn :before #'henri-org-roam-ensure-directories-advice))
  (setq org-roam-capture-templates
        `(("n" "note / 概念笔记" plain
           "%?"
           :target (file+head "notes/${slug}.org"
                              ,(format henri-org-roam--file-head-seedling "note"))
           :unnarrowed t)
          ("i" "inbox / 临时收集" plain
           "%?"
           :target (file+head "inbox/${slug}.org"
                              ,(format henri-org-roam--file-head-seedling "inbox"))
           :unnarrowed t)
          ("r" "reference / 资料笔记" plain
           "* Metadata\n- Source:\n- Author:\n- Date:\n- Type:\n\n* 摘要\n%?\n\n* 我的理解\n\n* 相关节点\n"
           :target (file+head "references/${slug}.org"
                              ,(format henri-org-roam--file-head "reference"))
           :unnarrowed t)
          ("p" "project / 项目笔记" plain
           "* 目标\n%?\n\n* 当前状态\n\n* 下一步\n\n* 相关节点\n"
           :target (file+head "projects/${slug}.org"
                              ,(format henri-org-roam--file-head "project"))
           :unnarrowed t)
          ("m" "map / 索引地图" plain
           "* 核心节点\n%?\n\n* 相关项目\n\n* 参考资料\n"
           :target (file+head "maps/${slug}.org"
                              ,(format henri-org-roam--file-head "map"))
           :unnarrowed t)
          ("e" "person / 人物笔记" plain
           "* 简介\n%?\n\n* 关键思想/事件\n\n* 相关节点\n"
           :target (file+head "people/${slug}.org"
                              ,(format henri-org-roam--file-head "person"))
           :unnarrowed t)))
  (advice-remove 'org-roam-capture- #'henri-org-roam--message-template-hint)
  (advice-add 'org-roam-capture- :before #'henri-org-roam--message-template-hint)
  (setq org-roam-dailies-capture-templates
        `(("d" "daily" entry
           "* %<%H:%M> %?\n"
           :target (file+head "%<%Y-%m-%d>.org"
                              ,henri-org-roam--daily-head))))
  (add-hook 'org-mode-hook #'henri-org-roam-enable-buffer-metadata-hook)
  (when (file-directory-p (henri-org-roam-directory))
    (org-roam-db-autosync-mode 1)))

(use-package consult-org-roam
  :if (henri-org-roam--package-available-p 'consult-org-roam)
  :ensure nil
  :after org-roam
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :bind (("C-c n s" . consult-org-roam-search)
         ("C-c n r" . consult-org-roam-backlinks)
         ("C-c n l" . consult-org-roam-forward-links))
  :config
  (consult-org-roam-mode 1))

(use-package citar-org-roam
  :if (and (bound-and-true-p henri-org-roam-enable-citar-integration)
           (henri-org-roam--package-available-p 'citar)
           (henri-org-roam--package-available-p 'citar-org-roam))
  :ensure nil
  :after (citar org-roam)
  ;; This is intentionally opt-in.  The default Citar notes path is
  ;; Academic/Reading; enabling this makes Citar create Roam reference nodes.
  :custom
  (citar-org-roam-subdir "references/")
  :config
  (citar-org-roam-mode 1))

(provide 'org-roam-henri)

;;; org-roam-henri.el ends here
