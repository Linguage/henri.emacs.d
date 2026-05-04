;;; markdown-nav.el --- Markdown 导航与结构（TOC / outline） -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: writing, markdown, navigation, toc

;;; Commentary:

;; Markdown 文档结构增强：
;;   - markdown-toc      -- 自动目录生成与刷新
;;   - outline-minor-mode -- heading 折叠
;;   - consult-imenu     -- 大纲跳转

;;; Code:

;; ---------------------------------------------------------------------------
;; markdown-toc 集成

;;;###autoload
(defun henri/md-toc-insert ()
  "在当前位置插入 markdown-toc 自动目录。"
  (interactive)
  (unless (eq major-mode 'markdown-mode)
    (user-error "当前不是 markdown-mode"))
  (if (fboundp 'markdown-toc-generate-toc)
      (markdown-toc-generate-toc)
    (user-error "未加载 markdown-toc：请安装该包（M-x package-install RET markdown-toc RET）")))

;;;###autoload
(defun henri/md-toc-refresh ()
  "刷新当前文档中的 markdown-toc 目录。"
  (interactive)
  (unless (eq major-mode 'markdown-mode)
    (user-error "当前不是 markdown-mode"))
  (if (fboundp 'markdown-toc-refresh-toc)
      (markdown-toc-refresh-toc)
    (user-error "未加载 markdown-toc：请安装该包")))

;;;###autoload
(defun henri/md-toc-delete ()
  "删除当前文档中的 markdown-toc 目录。"
  (interactive)
  (unless (eq major-mode 'markdown-mode)
    (user-error "当前不是 markdown-mode"))
  (if (fboundp 'markdown-toc-delete-toc)
      (markdown-toc-delete-toc)
    ;; 若 markdown-toc 未安装，提供手动删除兜底
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "<!-- markdown-toc start -->" nil t)
        (let ((beg (line-beginning-position)))
          (when (re-search-forward "<!-- markdown-toc end -->" nil t)
            (delete-region beg (line-end-position))
            (message "[henri/md-toc] 已删除目录")))))))

;; ---------------------------------------------------------------------------
;; outline-minor-mode

(defun henri/md--setup-outline ()
  "为 markdown-mode 配置 outline-minor-mode。"
  (setq-local outline-regexp "^#+ ")
  (setq-local outline-level
              (lambda ()
                (save-excursion
                  (looking-at "^#+ ")
                  (- (match-end 0) (match-beginning 0) 1))))
  (outline-minor-mode 1))

;;;###autoload
(add-hook 'markdown-mode-hook #'henri/md--setup-outline)

;; ---------------------------------------------------------------------------
;; consult-imenu / consult-outline

;;;###autoload
(defun henri/md-outline ()
  "使用 consult 进行 Markdown 大纲跳转。"
  (interactive)
  (unless (eq major-mode 'markdown-mode)
    (user-error "当前不是 markdown-mode"))
  (cond
   ((fboundp 'consult-outline)
    (consult-outline))
   ((fboundp 'consult-imenu)
    (consult-imenu))
   ((fboundp 'imenu)
    (call-interactively #'imenu))
   (t
    (user-error "未安装 consult / imenu，无法跳转大纲"))))

;; ---------------------------------------------------------------------------
;; 键位绑定

(defvar markdown-mode-map)
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c m t i") #'henri/md-toc-insert)
  (define-key markdown-mode-map (kbd "C-c m t r") #'henri/md-toc-refresh)
  (define-key markdown-mode-map (kbd "C-c m t d") #'henri/md-toc-delete)
  (define-key markdown-mode-map (kbd "C-c m o")   #'henri/md-outline))

(provide 'markdown-nav)
;;; markdown-nav.el ends here
