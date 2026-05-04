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

(defun henri-org-roam-directory ()
  "Return the normalized Org-roam root directory."
  (file-name-as-directory (file-truename henri-org-roam-directory)))

(defun henri-org-roam-file (relative-path)
  "Return RELATIVE-PATH under `henri-org-roam-directory'."
  (expand-file-name relative-path (henri-org-roam-directory)))

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
         ("C-c n x" . henri-org-roam-open-inbox))
  :init
  (henri-org-roam-ensure-directories)
  :config
  (require 'org-roam-dailies)
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-capture-templates
        '(("n" "note / 概念笔记" plain
           "%?"
           :target (file+head "notes/${slug}.org"
                              ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :note:\n\n")
           :unnarrowed t)
          ("r" "reference / 资料笔记" plain
           "* Metadata\n- Source:\n- Author:\n- Date:\n- Type:\n\n* 摘要\n%?\n\n* 我的理解\n\n* 相关节点\n"
           :target (file+head "references/${slug}.org"
                              ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :reference:\n\n")
           :unnarrowed t)
          ("p" "project / 项目笔记" plain
           "* 目标\n%?\n\n* 当前状态\n\n* 下一步\n\n* 相关节点\n"
           :target (file+head "projects/${slug}.org"
                              ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :project:\n\n")
           :unnarrowed t)
          ("m" "map / 索引地图" plain
           "* 核心节点\n%?\n\n* 相关项目\n\n* 参考资料\n"
           :target (file+head "maps/${slug}.org"
                              ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :map:\n\n")
           :unnarrowed t)
          ("e" "person / 人物笔记" plain
           "* 简介\n%?\n\n* 关键思想/事件\n\n* 相关节点\n"
           :target (file+head "people/${slug}.org"
                              ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :person:\n\n")
           :unnarrowed t)))
  (setq org-roam-dailies-capture-templates
        '(("d" "daily" entry
           "* %<%H:%M> %?\n"
           :target (file+head "%<%Y-%m-%d>.org"
                              ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n* 今日记录\n\n* Done\n\n* 想法\n\n* 待抽取节点\n"))))
  (org-roam-db-autosync-mode 1))

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
