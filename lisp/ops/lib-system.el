;;; lib-system.el --- OS / exec helpers -*- lexical-binding: t -*-

;;; Commentary:
;; Small utilities with no external package deps.

;;; Code:

(defun henri/executable-p (program)
  "Non-nil when PROGRAM is a nonempty string present on `exec-path'."
  (and (stringp program)
       (executable-find program)))

(defun henri/markdown-executable-p (program)
  "Backward-compatible alias for `henri/executable-p'."
  (henri/executable-p program))

(defun henri/get-os-type ()
  "Return a symbol: macos, linux, wsl, windows, or unknown."
  (cond
   ((eq system-type 'darwin) 'macos)
   ((eq system-type 'gnu/linux)
    (if (string-match "Microsoft" (shell-command-to-string "uname -r"))
        'wsl
      'linux))
   ((eq system-type 'windows-nt) 'windows)
   (t 'unknown)))

(provide 'lib-system)
;;; lib-system.el ends here
