;;; markdown-template.el --- Markdown YAML front-matter 模板 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: writing, markdown, template

;;; Commentary:

;; 提供博客 front-matter 模板与新文章创建命令。
;; 目前支持 Hugo / Jekyll 两种风格。

;;; Code:

(declare-function markdown-mode "markdown-mode")

;; ---------------------------------------------------------------------------
;; 模板数据

(defconst henri/md--templates
  '((hugo . "---
title: %s
date: %s
draft: true
tags: []
categories: []
---\n\n")
    (jekyll . "---
layout: post
title: %s
date: %s
tags: []
categories: []
---\n\n"))
  "Markdown front-matter 模板集合。")

;; ---------------------------------------------------------------------------
;; 命令

;;;###autoload
(defun henri/md-insert-front-matter (style)
  "在当前位置插入指定 STYLE 的 YAML front-matter。"
  (interactive
   (list (completing-read "选择模板风格: " '("hugo" "jekyll") nil t)))
  (unless (eq major-mode 'markdown-mode)
    (user-error "当前不是 markdown-mode"))
  (let ((tmpl (alist-get (intern style) henri/md--templates)))
    (unless tmpl
      (user-error "未知模板风格: %s" style))
    (insert (format tmpl
                    (read-string "标题: " (file-name-sans-extension
                                           (or (buffer-file-name)
                                               (buffer-name))))
                    (format-time-string "%Y-%m-%dT%H:%M:%S%z")))))

;;;###autoload
(defun henri/md-new-post (dir style)
  "在 DIR 目录下创建一篇带 front-matter 的新 Markdown 文章。"
  (interactive
   (list (read-directory-name "文章目录: "
                              (and (boundp 'henri-notes-directory)
                                   henri-notes-directory)
                              nil t)
         (completing-read "模板风格: " '("hugo" "jekyll") nil t)))
  (let* ((title (read-string "文章标题: "))
         (slug (replace-regexp-in-string "[^a-zA-Z0-9\\-]" "-"
                                         (downcase title)))
         (date (format-time-string "%Y-%m-%d"))
         (fname (format "%s-%s.md" date slug))
         (fpath (expand-file-name fname dir)))
    (when (file-exists-p fpath)
      (user-error "文件已存在: %s" fpath))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (find-file fpath)
    (markdown-mode)
    (let ((tmpl (alist-get (intern style) henri/md--templates)))
      (insert (format tmpl title (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))
    (save-buffer)
    (message "[henri/md-template] 已创建: %s" fpath)))

;; ---------------------------------------------------------------------------
;; 键位绑定（可选，不抢占常用键位）

(defvar markdown-mode-map)
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c m T i") #'henri/md-insert-front-matter)
  (define-key markdown-mode-map (kbd "C-c m T n") #'henri/md-new-post))

(provide 'markdown-template)
;;; markdown-template.el ends here
