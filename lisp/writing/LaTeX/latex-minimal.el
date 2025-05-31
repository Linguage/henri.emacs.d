;;; latex-minimal.el --- 最小化 LaTeX 配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (auctex "14.0"))

;;; Commentary:

;; 最小化的 LaTeX 配置，主要服务于 Org-mode PDF 导出
;; 包含基本的 AUCTeX 支持和编译功能

;;; Code:

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
  
  ;; 编译设置
  (setq LaTeX-command "pdflatex -synctex=1")
  
  ;; 启用基本模式
  (add-hook 'LaTeX-mode-hook #'font-lock-mode)
  (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
  
  ;; 简单的快捷键
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-c") 'TeX-command-master)
              (local-set-key (kbd "C-c C-v") 'TeX-view))))

;; =============================================================================
;; 中文支持

(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t ("pdflatex"))
        ("T1" "fontenc" t ("pdflatex"))
        ("" "graphicx" t)
        ("" "grffile" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("" "hyperref" nil)
        ("UTF8" "ctex" t)))

;; =============================================================================
;; 主题支持

(require 'latex-themes-simple)

;; =============================================================================
;; PDF 查看器设置

(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
(setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))

(provide 'latex-minimal)

;;; latex-minimal.el ends here
