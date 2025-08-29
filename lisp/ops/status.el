;;; status.el --- Status / diagnostics helpers -*- lexical-binding: t -*-

;;; Commentary:
;; Utilities to inspect which modules/features are active.

;;; Code:

(require 'cl-lib)

(defvar henri--org-submodules
  '((org-base . henri-org-enable-base)
    (org-latex . henri-org-enable-latex)
    (org-journal . henri-org-enable-journal)
    (org-html . henri-org-enable-html)
    (org-academic . henri-org-enable-academic))
  "Org submodule + toggle variable mapping.")

(defun henri/list-active-org-modules ()
  "Return an alist of (FEATURE . status) for Org-related submodules."
  (cl-loop for (feat . var) in henri--org-submodules
           collect (cons feat (list :enabled (and (boundp var) (symbol-value var))
                                     :loaded (featurep feat)))))

(defun henri/module-status-report ()
  "Generate a structured status report for key components."
  (let* ((org (henri/list-active-org-modules))
         (lsp (cl-loop for b in (buffer-list)
                       when (buffer-local-value 'eglot--managed-mode b)
                       collect (buffer-name b)))
         (themes custom-enabled-themes)
         (large (and (boundp 'henri-large-file-threshold) henri-large-file-threshold))
         (fmt (and (boundp 'henri-lsp-auto-format) henri-lsp-auto-format)))
    (list :org org
          :lsp-buffer-count (length lsp)
          :themes themes
          :auto-format fmt
          :large-file-threshold large)))

(defun henri/show-module-status (&optional raw)
  "Echo core module status. With RAW (prefix), insert into a buffer as lisp form."
  (interactive "P")
  (let ((report (henri/module-status-report)))
    (if raw
        (with-current-buffer (get-buffer-create "*Henri-Status*")
          (erase-buffer)
          (prin1 report (current-buffer))
          (goto-char (point-min))
          (display-buffer (current-buffer)))
      (message "[henri] org=%s lsp=%d theme=%s auto-format=%s"
               (mapcar (lambda (p) (format "%s:%s/%s" (car p)
                                           (plist-get (cdr p) :enabled)
                                           (plist-get (cdr p) :loaded)))
                       (plist-get report :org))
               (plist-get report :lsp-buffer-count)
               (plist-get report :themes)
               (plist-get report :auto-format)))))

(provide 'status)
;;; status.el ends here
