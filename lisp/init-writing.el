;; 启用的扩展有：
;; - markdown-mode
;; - markdown-preview-mode
;; - grip-mode
;; 待添加：
;; org-mode
;; AucTex

;;-------------------------
;; 配置markdown语法支持
;;-------------------------
(use-package markdown-mode
  :ensure t) ;; 依赖markdown-mode包
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(provide 'init-markdown)

(use-package markdown-preview-mode
  :ensure t)
;; 使用 grip-mode 进行实时预览
;; (use-package grip-mode
;;   :ensure t
;;   :bind ("C-c C-p" . grip-mode)
;;   :config
;; ;  (setq grip-github-user "your-github-username"
;; ;       grip-github-password "your-github-token"))
;; )
;; ;; 自动启动 markdown-mode 和 grip-mode
;; (add-hook 'markdown-mode-hook 'grip-mode)

(provide 'init-writing)