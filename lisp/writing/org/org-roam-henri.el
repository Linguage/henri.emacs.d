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
(require 'subr-x)

(defgroup henri-org-roam nil
  "General Org-roam knowledge base."
  :group 'henri-writing)

(defconst henri-org-roam--subdirectories
  '("inbox" "daily" "notes" "references" "projects" "people" "maps")
  "Purpose-based subdirectories under `henri-org-roam-directory'.")

(defconst henri-org-roam--file-head
  "#+title: ${title}\n#+filetags: :%s:\n:PROPERTIES:\n:ID:       %%(org-id-new)\n:CREATED:  %%U\n:UPDATED:  %%U\n:END:\n\n"
  "File head template for regular Org-roam nodes.")

(defconst henri-org-roam--daily-head
  "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n:PROPERTIES:\n:ID:       %(org-id-new)\n:CREATED:  %U\n:UPDATED:  %U\n:END:\n\n* 今日记录\n\n* Done\n\n* 想法\n\n* 待抽取节点\n"
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

(defun henri-org-roam--metadata-limit ()
  "Return the end position of the file-level metadata area."
  (save-excursion
    (goto-char (point-min))
    (or (re-search-forward "^\\* " nil t)
        (point-max))))

(defun henri-org-roam--ensure-time-metadata ()
  "Ensure CREATED and UPDATED metadata exist in the file property drawer."
  (let ((timestamp (henri-org-roam--timestamp))
        (limit (henri-org-roam--metadata-limit)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^:PROPERTIES:[ \t]*$" limit t)
          (let ((drawer-start (point))
                (drawer-end (save-excursion
                              (when (re-search-forward "^:END:[ \t]*$" limit t)
                                (let ((marker (copy-marker (line-beginning-position))))
                                  (set-marker-insertion-type marker t)
                                  marker)))))
            (when drawer-end
              (goto-char drawer-start)
              (unless (re-search-forward "^:CREATED:[ \t]+.*$" drawer-end t)
                (goto-char drawer-end)
                (insert (format ":CREATED:  %s\n" timestamp)))
              (goto-char drawer-start)
              (if (re-search-forward "^:UPDATED:[ \t]+.*$" drawer-end t)
                  (replace-match (format ":UPDATED:  %s" timestamp) t t)
                (goto-char drawer-end)
                (insert (format ":UPDATED:  %s\n" timestamp)))
              (set-marker drawer-end nil)))
        (let (id created updated)
          (goto-char (point-min))
          (while (looking-at "^#\\+")
            (forward-line 1))
          (let ((insert-point (point)))
            (while (looking-at "^[ \t]*$")
              (forward-line 1))
            (let ((metadata-start (point)))
              (while (looking-at "^:\\(ID\\|CREATED\\|UPDATED\\):[ \t]+\\(.*\\)$")
                (let ((key (match-string 1))
                      (value (match-string 2)))
                  (pcase key
                    ("ID" (setq id value))
                    ("CREATED" (setq created value))
                    ("UPDATED" (setq updated value))))
                (forward-line 1))
              (when (> (point) metadata-start)
                (delete-region metadata-start (point))))
            (goto-char insert-point)
            (insert (format ":PROPERTIES:\n%s:CREATED:  %s\n:UPDATED:  %s\n:END:\n"
                            (if id (format ":ID:       %s\n" id) "")
                            (or created timestamp)
                            (or updated timestamp)))))))))

(defun henri-org-roam-update-time-metadata ()
  "Refresh UPDATED metadata for Org-roam files before saving."
  (when (and (derived-mode-p 'org-mode)
             (henri-org-roam--buffer-file-in-roam-p))
    (henri-org-roam--ensure-time-metadata)))

(defun henri-org-roam-ensure-directories ()
  "Create the Org-roam root and purpose-based subdirectories."
  (interactive)
  (dolist (dir (cons "" henri-org-roam--subdirectories))
    (let ((path (henri-org-roam-file dir)))
      (unless (file-directory-p path)
        (make-directory path t)))))

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
         ("C-c n d" . henri-org-roam-open-directory)
         ("C-c n x" . henri-org-roam-open-inbox)
         ("C-c n ?" . henri-org-roam-show-template-keys))
  :config
  (require 'org-roam-dailies)
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-capture-templates
        `(("n" "note / 概念笔记" plain
           "%?"
           :target (file+head "notes/${slug}.org"
                              ,(format henri-org-roam--file-head "note"))
           :unnarrowed t)
          ("i" "inbox / 临时收集" plain
           "%?"
           :target (file+head "inbox/${slug}.org"
                              ,(format henri-org-roam--file-head "inbox"))
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
  (add-hook 'before-save-hook #'henri-org-roam-update-time-metadata)
  (when (file-directory-p (henri-org-roam-directory))
    (org-roam-db-autosync-mode 1)))

(defun henri-org-roam-ensure-directories-maybe ()
  "Create Org-roam directories during interactive startup only."
  (unless noninteractive
    (henri-org-roam-ensure-directories)
    (when (fboundp 'org-roam-db-autosync-mode)
      (org-roam-db-autosync-mode 1))))

(add-hook 'after-init-hook #'henri-org-roam-ensure-directories-maybe)

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
