;;; markdown-lint.el --- Markdown lint（flymake 后端） -*- lexical-binding: t -*-

;; Author: Henri
;; Maintainer: Henri
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: writing, markdown, lint

;;; Commentary:

;; 可选的 markdownlint 集成，默认关闭（需 node 依赖）。
;; 仅在 `henri-md-enable-lint' 为 t 且 markdownlint CLI 可用时生效。

;;; Code:

(require 'cl-lib)
(require 'lib-system)

;; ---------------------------------------------------------------------------
;; flymake 后端

(defvar-local henri/md-lint--proc nil)

(defun henri/md-lint--report (source buffer)
  "为 SOURCE 文件生成 flymake 诊断报告，输出到 BUFFER。"
  (lambda (proc _event)
    (when (eq 'exit (process-status proc))
      (unwind-protect
          (if (with-current-buffer buffer (eq proc henri/md-lint--proc))
              (with-current-buffer buffer
                (save-excursion
                  (goto-char (point-min))
                  (let (diags)
                    (while (search-forward-regexp
                            "^\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\)? \\([A-Z0-9]+\\)/\\([^ ]+\\) \\(.+\\)$"
                            nil t)
                      (let* ((line (string-to-number (match-string 2)))
                             (col (string-to-number (or (match-string 3) "1")))
                             (rule (match-string 4))
                             (msg (string-trim (match-string 6)))
                             (diag (flymake-make-diagnostic
                                    source
                                    (save-excursion
                                      (goto-char (point-min))
                                      (forward-line (1- line))
                                      (forward-char (1- col))
                                      (point))
                                    (save-excursion
                                      (goto-char (point-min))
                                      (forward-line (1- line))
                                      (line-end-position))
                                    :warning
                                    (format "[markdownlint %s] %s" rule msg))))
                        (push diag diags)))
                    (funcall (process-get proc 'flymake-report)
                             (nreverse diags)))))
            (flymake-log :warning "Canceling obsolete lint proc %s" proc))
        (kill-buffer (process-buffer proc))))))

;;;###autoload
(defun henri/md-lint-backend (report-fn &rest _args)
  "Flymake backend for markdownlint."
  (when (process-live-p henri/md-lint--proc)
    (kill-process henri/md-lint--proc))
  (let ((source (current-buffer))
        (file (buffer-file-name)))
    (if (not file)
        (funcall report-fn :panic :explanation "Buffer not visiting a file")
      (setq henri/md-lint--proc
            (make-process
             :name "henri-md-lint"
             :command (list "markdownlint" file)
             :buffer (generate-new-buffer " *henri-md-lint*")
             :noquery t
             :connection-type 'pipe
             :sentinel (henri/md-lint--report source (current-buffer))))
      (process-put henri/md-lint--proc 'flymake-report report-fn))))

;; ---------------------------------------------------------------------------
;; 注册与初始化

;;;###autoload
(defun henri/md-lint-setup ()
  "在当前 markdown 缓冲区注册 flymake lint 后端。"
  (when (and (boundp 'henri-md-enable-lint) henri-md-enable-lint
             (henri/executable-p "markdownlint"))
    (add-hook 'flymake-diagnostic-functions #'henri/md-lint-backend nil t)
    (flymake-mode 1)))

(add-hook 'markdown-mode-hook #'henri/md-lint-setup)

(provide 'markdown-lint)
;;; markdown-lint.el ends here
