;;; init-dashboard.el --- Startup dashboard & notes shortcuts -*- lexical-binding: t -*-

;;; Commentary:
;; Henri startup buffer (`initial-buffer-choice'), random ASCII logos, and
;; small helpers for opening the notes tree (also used by `counsel' binding
;; in `init-managing').  `init.el' loads `init-custom' before this file so
;; `henri-notes-directory' is defined.

;;; Code:

(defvar henri-notes-directory)

(defvar henri/default-notes-directory nil
  "Expanded notes directory; set from `henri-notes-directory'.")
(setq henri/default-notes-directory (expand-file-name henri-notes-directory))

(defun henri/open-notes-directory ()
  "Open Henri's notes directory in Dired."
  (interactive)
  (dired henri/default-notes-directory))

(defun henri/open-journal-directory ()
  "Open Henri's Journal directory in Dired."
  (interactive)
  (dired (expand-file-name "Journal/" henri/default-notes-directory)))

(defun henri/find-file-in-notes ()
  "Find file starting from `henri/default-notes-directory'."
  (interactive)
  (let ((default-directory henri/default-notes-directory))
    (if (fboundp 'counsel-find-file)
        (counsel-find-file)
      (call-interactively #'find-file))))

(defun henri/dashboard-insert-action (label action)
  "Insert dashboard LABEL as a button invoking ACTION."
  (insert-text-button
   label
   'action (lambda (_button) (call-interactively action))
   'follow-link t
   'help-echo (format "Run %s" action))
  (insert "\n"))

(defconst henri/dashboard-logos
  '(("      ___           ___           ___           ___           ___     "
     "     /\\  \\         /\\__\\         /\\  \\         /\\  \\         /\\  \\    "
     "    /::\\  \\       /::|  |       /::\\  \\       /::\\  \\       /::\\  \\   "
     "   /:/\\:\\  \\     /:|:|  |      /:/\\:\\  \\     /:/\\:\\  \\     /:/\\ \\  \\  "
     "  /::\\~\\:\\  \\   /:/|:|__|__   /::\\~\\:\\  \\   /:/  \\:\\  \\   _\\:\\~\\ \\  \\ "
     " /:/\\:\\ \\:\\__\\ /:/ |::::\\__\\ /:/\\:\\ \\:\\__\\ /:/__/ \\:\\__\\ /\\ \\:\\ \\ \\__\\"
     " \\:\\~\\:\\ \\/__/ \\/__/~~/:/  / \\/__\\:\\/:/  / \\:\\  \\  \\/__/ \\:\\ \\:\\ \\/__/"
     "  \\:\\ \\:\\__\\         /:/  /       \\::/  /   \\:\\  \\        \\:\\ \\:\\__\\  "
     "   \\:\\ \\/__/        /:/  /        /:/  /     \\:\\  \\        \\:\\/:/  /  "
     "    \\:\\__\\         /:/  /        /:/  /       \\:\\__\\        \\::/  /   "
     "     \\/__/         \\/__/         \\/__/         \\/__/         \\/__/    ")
    ("      ___           ___           ___           ___                 "
     "     /\\__\\         /\\  \\         /\\__\\         /\\  \\          ___   "
     "    /:/  /        /::\\  \\       /::|  |       /::\\  \\        /\\  \\  "
     "   /:/__/        /:/\\:\\  \\     /:|:|  |      /:/\\:\\  \\       \\:\\  \\ "
     "  /::\\  \\ ___   /::\\~\\:\\  \\   /:/|:|  |__   /::\\~\\:\\  \\      /::\\__\\"
     " /:/\\:\\  /\\__\\ /:/\\:\\ \\:\\__\\ /:/ |:| /\\__\\ /:/\\:\\ \\:\\__\\  __/:/\\/__/"
     " \\/__\\:\\/:/  / \\:\\~\\:\\ \\/__/ \\/__|:|/:/  / \\/_|::\\/:/  / /\\/:/  /   "
     "      \\::/  /   \\:\\ \\:\\__\\       |:/:/  /     |:|::/  /  \\::/__/    "
     "      /:/  /     \\:\\ \\/__/       |::/  /      |:|\\/__/    \\:\\__\\    "
     "     /:/  /       \\:\\__\\         /:/  /       |:|  |       \\/__/    "
     "     \\/__/         \\/__/         \\/__/         \\|__|                "))
  "ASCII logos displayed on Henri's dashboard.")

(defun henri/dashboard-random-logo ()
  "Return one random ASCII logo from `henri/dashboard-logos'."
  (nth (random (length henri/dashboard-logos)) henri/dashboard-logos))

(defun henri/dashboard-insert-centered (text &optional face)
  "Insert TEXT centered in the current window, optionally using FACE."
  (let* ((width (max 80 (window-width)))
         (padding (max 0 (/ (- width (string-width text)) 2))))
    (insert (make-string padding ?\s))
    (insert (if face (propertize text 'face face) text))
    (insert "\n")))

(defun henri/dashboard ()
  "Create and return Henri's startup dashboard buffer."
  (let ((buffer (get-buffer-create "*Henri Dashboard*"))
        (dashboard-logo (henri/dashboard-random-logo)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n")
        (dolist (line dashboard-logo)
          (henri/dashboard-insert-centered line 'font-lock-keyword-face))
        (insert "\n")
        (henri/dashboard-insert-centered "henri.emacs.d" 'font-lock-function-name-face)
        (henri/dashboard-insert-centered "Personal writing and coding workspace")
        (insert "\n  Notes root: " henri/default-notes-directory "\n\n")
        (henri/dashboard-insert-action "  [RET] Open Notes" #'henri/open-notes-directory)
        (henri/dashboard-insert-action "        Open Journal" #'henri/open-journal-directory)
        (henri/dashboard-insert-action "        Find File in Notes" #'henri/find-file-in-notes)
        (insert "\n  Shortcuts\n")
        (insert "  C-x C-f  Find file (default)\n")
        (insert "  C-c f n  Find file in Notes\n")
        (insert "  C-c c    Org capture\n")
        (insert "  C-c a    Org agenda\n")
        (insert "  C-c h 0  Apply default HTML theme\n")
        (goto-char (point-min))
        (special-mode)
        (setq-local display-line-numbers nil)
        ;; `default-directory' 仅当笔记根存在时使用，否则 C-x C-f 仍落在合理目录。
        (setq-local default-directory
                    (if (file-directory-p henri/default-notes-directory)
                        henri/default-notes-directory
                      user-emacs-directory))))
    buffer))

;; 启动缓冲：依赖 `initial-buffer-choice'，不再 advice `dired' 或定时清理。
(setq initial-buffer-choice #'henri/dashboard)
(when (file-directory-p henri/default-notes-directory)
  (setq default-directory henri/default-notes-directory))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
