;;; backup.el --- Centralized backup & autosave management -*- lexical-binding: t -*-

;;; Commentary:
;; Provides optional centralized backup/auto-save handling controlled by
;; `henri-backup-enable'.

;;; Code:

(require 'paths)

(defvar henri-backup-directory
  (expand-file-name "backups/" henri-var-directory))
(defvar henri-autosave-directory
  (expand-file-name "autosave/" henri-var-directory))

(defun henri/setup-backup-system ()
  "Configure centralized backups & auto-saves if enabled."
  (when (and (boundp 'henri-backup-enable) henri-backup-enable)
    (dolist (dir (list henri-backup-directory henri-autosave-directory))
      (unless (file-directory-p dir)
        (make-directory dir t)))
    (setq backup-directory-alist `((".*" . ,henri-backup-directory))
          backup-by-copying t
          version-control t
          delete-old-versions t
          kept-new-versions 6
          kept-old-versions 2
          auto-save-default t
          auto-save-file-name-transforms `((".*" ,henri-autosave-directory t))
          create-lockfiles nil)))

(add-hook 'after-init-hook #'henri/setup-backup-system)

(provide 'backup)
;;; backup.el ends here
