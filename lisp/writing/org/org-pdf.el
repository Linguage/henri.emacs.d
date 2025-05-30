;;; org-pdf.el --- Org Mode PDF 工具配置 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Keywords: org, pdf, tools

;;; Commentary:

;; Org Mode PDF 相关配置，包含：
;; - pdf-tools 配置
;; - PDF 查看和导出
;; - 自动打开生成的 PDF

;;; Code:

;; =============================================================================
;; PDF 工具配置

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  ;; 设置 PDF-tools 兼容性
  ;; 禁用 PDF 查看模式下的行号显示
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (display-line-numbers-mode -1))))

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;; 修改 org-open-pdf-after-export 函数确保使用 pdf-tools
(defun org-open-pdf-after-export (backend)
  "Open the generated PDF file in pdf-tools after org-mode export."
  (when (eq backend 'latex)
    (let ((pdf-file (concat (file-name-sans-extension buffer-file-name) ".pdf")))
      (when (file-exists-p pdf-file)
        (find-file pdf-file)
        ;; 确保在此处关闭行号显示
        (when (bound-and-true-p display-line-numbers-mode)
          (display-line-numbers-mode -1))))))

(add-hook 'org-export-after-processing-hook 'org-open-pdf-after-export)

(provide 'org-pdf)

;;; org-pdf.el ends here
