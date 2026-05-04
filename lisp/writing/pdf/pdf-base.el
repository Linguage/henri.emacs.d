;;; pdf-base.el --- pdf-tools 子模块 -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (pdf-tools "1.0"))
;; Keywords: pdf, tools

;;; Commentary:

;; writing 模块下的 PDF 子模块，集中管理 pdf-tools 与相关辅助：
;;
;;   - 在 Emacs buffer 内渲染 PDF（poppler）
;;   - 与 LaTeX/Org PDF 导出 SyncTeX 联动
;;   - 与 citar / org-noter / org-roam 的下游集成留给各自模块
;;
;; 由 `init-writing.el' 在 `henri-pdf-enable-tools' 为非 nil 时加载。
;;
;; 首次安装提示：
;;   brew install poppler automake autoconf pkg-config
;; 之后 M-x pdf-tools-install 触发本地编译 epdfinfo。

;;; Code:

(require 'lib-system)

(defun henri/pdf-check-deps ()
  "报告 pdf-tools 与 epdfinfo 的可用状态。"
  (interactive)
  (let* ((feature-loaded (featurep 'pdf-tools))
         (epdfinfo (and (boundp 'pdf-info-epdfinfo-program)
                        pdf-info-epdfinfo-program))
         (epdfinfo-ok (and epdfinfo (file-executable-p epdfinfo))))
    (message "[pdf-tools] feature: %s | epdfinfo: %s | poppler: %s"
             (if feature-loaded "loaded" "not loaded")
             (if epdfinfo-ok (format "OK (%s)" epdfinfo)
               (format "缺失（%s）；请 brew install poppler 后 M-x pdf-tools-install"
                       (or epdfinfo "未配置")))
             (if (henri/executable-p "pkg-config") "OK" "缺失（brew install pkg-config）"))))

;; -----------------------------------------------------------------------------
;; pdf-tools 主体

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; 仅在图形终端尝试初始化；epdfinfo 缺失时静默失败，避免启动报错。
  (when (and (display-graphic-p) (fboundp 'pdf-tools-install))
    (condition-case err
        (pdf-tools-install-noverify)
      (error (message "[pdf-tools] 初始化失败：%s（请检查 epdfinfo / poppler）"
                      (error-message-string err)))))

  ;; 平滑滚动 + 高分屏渲染
  (setq pdf-view-display-size 'fit-page)
  (setq pdf-view-resize-factor 1.1)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil)

  ;; display-line-numbers-mode 被 global-display-line-numbers-mode 自动开启，
  ;; 但 pdf-view 与它不兼容。在 hook 里关掉之前，pdf-tools 会先检测并警告。
  ;; 解决：把它从不兼容列表中移除，改由 hook 主动关闭。
  (setq pdf-view-incompatible-modes
        (delq 'display-line-numbers-mode pdf-view-incompatible-modes))

  (defun henri/pdf-disable-line-numbers ()
    "在 pdf-view-mode buffer 中关闭 display-line-numbers / hl-line。"
    (display-line-numbers-mode -1)
    (when (bound-and-true-p hl-line-mode)
      (hl-line-mode -1)))
  (add-hook 'pdf-view-mode-hook #'henri/pdf-disable-line-numbers)

  ;; SyncTeX：LaTeX 编译完跳到 PDF 中对应位置
  (when (boundp 'pdf-sync-forward-display-action)
    (setq pdf-sync-forward-display-action '(display-buffer-reuse-window))
    (setq pdf-sync-backward-display-action '(display-buffer-reuse-window))))

;; -----------------------------------------------------------------------------
;; saveplace：重开 PDF 时回到上次阅读位置（软依赖，包不可用时静默跳过）

(with-eval-after-load 'pdf-tools
  (when (locate-library "saveplace-pdf-view")
    (require 'saveplace-pdf-view nil t)
    (when (fboundp 'save-place-mode)
      (save-place-mode 1))))

;; -----------------------------------------------------------------------------
;; 键位与 which-key 提示

(with-eval-after-load 'pdf-tools
  (define-key pdf-view-mode-map (kbd "C-c m c") #'henri/pdf-check-deps)
  ;; 主题反色（夜间阅读）
  (when (fboundp 'pdf-view-midnight-mode)
    (define-key pdf-view-mode-map (kbd "C-c m n") #'pdf-view-midnight-mode)))

(provide 'pdf-base)

;;; pdf-base.el ends here
