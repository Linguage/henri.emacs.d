;;; paths.el --- Machine-local runtime directories -*- lexical-binding: t -*-

;;; Commentary:
;; Conservative layout under `user-emacs-directory' (no no-littering).
;; Creates missing dirs once at load time; see README / c.md for git policy.

;;; Code:

(defgroup henri-runtime nil
  "Machine-local paths under the Emacs user directory."
  :group 'henri-core :prefix "henri-")

(defcustom henri-var-directory
  (expand-file-name "var" user-emacs-directory)
  "Root for backups, autosaves, and similar."
  :type 'directory
  :group 'henri-runtime)

(defcustom henri-local-cache-directory
  (expand-file-name ".local/cache" user-emacs-directory)
  "Optional cache namespace (packages may still use their defaults)."
  :type 'directory
  :group 'henri-runtime)

(defcustom henri-local-etc-directory
  (expand-file-name ".local/etc" user-emacs-directory)
  "Optional local etc namespace."
  :type 'directory
  :group 'henri-runtime)

(defcustom henri-tree-sitter-directory
  (expand-file-name "tree-sitter" user-emacs-directory)
  "Tree-sitter parser directory (created if missing)."
  :type 'directory
  :group 'henri-runtime)

(defcustom henri-rime-directory
  (expand-file-name "rime" user-emacs-directory)
  "Rime user data directory for emacs-rime."
  :type 'directory
  :group 'henri-runtime)

(defun henri/ensure-runtime-directories ()
  "Create standard machine-local directories if they do not exist."
  (dolist (dir (list henri-var-directory
                      (expand-file-name "backups" henri-var-directory)
                      (expand-file-name "autosave" henri-var-directory)
                      henri-local-cache-directory
                      henri-local-etc-directory
                      henri-tree-sitter-directory
                      henri-rime-directory
                      (expand-file-name "transient" user-emacs-directory)))
    (unless (file-directory-p dir)
      (ignore-errors (make-directory dir t)))))

(henri/ensure-runtime-directories)

(provide 'paths)
;;; paths.el ends here
