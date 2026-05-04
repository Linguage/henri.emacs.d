;;; latex-minimal.el --- 最小化 LaTeX 配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (auctex "14.0"))

;;; Commentary:

;; LaTeX 编辑工作台配置。
;; Org PDF 导出变量由 `org-latex.el' 维护，本文件只负责 .tex/AUCTeX、
;; 本机 TeX 工具链兜底、PDF 查看和快速输入。

;;; Code:

(require 'subr-x)

;; =============================================================================
;; TeX 工具链路径

(defgroup henri-latex nil
  "LaTeX writing configuration."
  :group 'henri-writing
  :prefix "henri-latex-")

(defcustom henri-latex-texbin-directory "/Library/TeX/texbin"
  "macOS BasicTeX/MacTeX executable directory used as a PATH fallback."
  :type 'directory
  :group 'henri-latex)

(defun henri-latex-ensure-texbin ()
  "Add `henri-latex-texbin-directory' to `exec-path' and PATH when present."
  (when (file-directory-p henri-latex-texbin-directory)
    (add-to-list 'exec-path henri-latex-texbin-directory)
    (let ((path (getenv "PATH")))
      (unless (and path
                   (member henri-latex-texbin-directory
                           (split-string path path-separator t)))
        (setenv "PATH"
                (if (and path (not (string-empty-p path)))
                    (concat henri-latex-texbin-directory path-separator path)
                  henri-latex-texbin-directory))))))

(henri-latex-ensure-texbin)

;; =============================================================================
;; 基础 AUCTeX 配置

(use-package tex
  :ensure auctex
  :mode (("\\.tex\\'" . LaTeX-mode)
         ("\\.latex\\'" . LaTeX-mode))
  :config
  ;; 基础设置
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  
  ;; 编译设置
  (add-to-list 'TeX-command-list
               '("LatexMk XeLaTeX"
                 "latexmk -xelatex -synctex=1 -interaction=nonstopmode %s"
                 TeX-run-TeX nil t
                 :help "Run latexmk with XeLaTeX"))
  (setq TeX-command-default "LatexMk XeLaTeX")
  
  ;; 启用基本模式
  (add-hook 'LaTeX-mode-hook #'font-lock-mode)
  (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
  
  ;; 简单的快捷键
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-c") 'TeX-command-master)
              (local-set-key (kbd "C-c C-v") 'TeX-view))))

;; =============================================================================
;; 公式输入加速

(use-package cdlatex
  :ensure t
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (org-mode . org-cdlatex-mode)))

;; =============================================================================
;; 主题支持

(require 'latex-themes-simple)
(require 'latex-templates)

;; =============================================================================
;; PDF 查看器设置

(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
(setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))

(provide 'latex-minimal)

;;; latex-minimal.el ends here
