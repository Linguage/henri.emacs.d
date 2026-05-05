;;; notes-sync.el --- Bulk Git sync for notes repositories -*- lexical-binding: t -*-

;;; Commentary:
;; Async one-shot Git workflows for the personal notes tree.
;;
;; Drives `henri-notes-sync-repos' (relative names under
;; `henri-notes-directory') via `make-process'.  All output is appended to a
;; single `*Notes Sync*' buffer; multiple repos run in parallel.
;;
;; Commands:
;;   `henri/notes-pull-all'  -- git pull --rebase --autostash for each repo
;;   `henri/notes-sync-all'  -- add -A; commit (if dirty); rebase; push
;;   `henri/notes-status-all'-- print short porcelain status for each repo
;;   `henri/notes-magit'     -- prompt for one repo and open `magit-status'
;;
;; Bound under the existing `C-c g' (git) prefix:
;;   C-c g N u  -- pull all
;;   C-c g N s  -- sync all
;;   C-c g N S  -- status overview
;;   C-c g N m  -- magit-status for a single repo

;;; Code:

(require 'cl-lib)

(defvar henri-notes-directory)
(defvar henri-notes-sync-repos)
(defvar henri-notes-sync-commit-message-format)

(defconst henri-notes-sync--buffer-name "*Notes Sync*")

(defun henri-notes-sync--repo-path (repo)
  "Return absolute directory for REPO (a relative name under notes root)."
  (expand-file-name repo (expand-file-name henri-notes-directory)))

(defun henri-notes-sync--valid-repos ()
  "Return list of (NAME . ABS-PATH) for configured repos that look like Git checkouts."
  (cl-loop for name in henri-notes-sync-repos
           for dir = (henri-notes-sync--repo-path name)
           when (file-directory-p (expand-file-name ".git" dir))
           collect (cons name dir)))

(defun henri-notes-sync--log-buffer ()
  "Return (and reveal) the shared sync log buffer."
  (let ((buf (get-buffer-create henri-notes-sync--buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (setq buffer-read-only nil))
    buf))

(defun henri-notes-sync--log (fmt &rest args)
  "Append a formatted line to the sync log buffer."
  (let ((buf (henri-notes-sync--log-buffer))
        (line (apply #'format fmt args)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert line)
      (unless (string-suffix-p "\n" line) (insert "\n")))))

(defun henri-notes-sync--make-sentinel (label)
  "Return a process sentinel that reports completion of LABEL to the log."
  (lambda (proc event)
    (when (memq (process-status proc) '(exit signal))
      (let ((code (process-exit-status proc)))
        (henri-notes-sync--log "── [%s] finished (exit=%d, %s)"
                               label code (string-trim event))))))

(defun henri-notes-sync--run (label dir shell-cmd)
  "Run SHELL-CMD inside DIR asynchronously, tagging output with LABEL."
  (let* ((buf (henri-notes-sync--log-buffer))
         (default-directory (file-name-as-directory dir)))
    (henri-notes-sync--log "▶ [%s] %s" label shell-cmd)
    (make-process
     :name (format "notes-sync:%s" label)
     :buffer buf
     :command (list (or (and (boundp 'henri-shell) henri-shell)
                        shell-file-name "/bin/sh")
                    "-c" shell-cmd)
     :noquery t
     :sentinel (henri-notes-sync--make-sentinel label))))

(defun henri-notes-sync--ensure-repos ()
  "Return validated repos or signal a friendly user error."
  (let ((repos (henri-notes-sync--valid-repos)))
    (unless repos
      (user-error
       "[henri] No git repos found under %s (check `henri-notes-sync-repos')"
       (expand-file-name henri-notes-directory)))
    repos))

;;;###autoload
(defun henri/notes-pull-all ()
  "Run `git pull --rebase --autostash' on every configured notes repo."
  (interactive)
  (let ((repos (henri-notes-sync--ensure-repos)))
    (henri-notes-sync--log "═══ pull-all @ %s" (format-time-string "%F %T"))
    (display-buffer (henri-notes-sync--log-buffer))
    (pcase-dolist (`(,name . ,dir) repos)
      (henri-notes-sync--run
       name dir "git pull --rebase --autostash"))))

;;;###autoload
(defun henri/notes-sync-all (&optional message)
  "Stage, commit (if dirty), rebase and push every configured notes repo.
With prefix arg, prompt for a custom commit MESSAGE."
  (interactive
   (list (when current-prefix-arg
           (read-string "Commit message: "
                        (format-time-string
                         henri-notes-sync-commit-message-format)))))
  (let* ((repos (henri-notes-sync--ensure-repos))
         (msg (or message
                  (format-time-string henri-notes-sync-commit-message-format))))
    (henri-notes-sync--log "═══ sync-all @ %s  msg=%S"
                           (format-time-string "%F %T") msg)
    (display-buffer (henri-notes-sync--log-buffer))
    (pcase-dolist (`(,name . ,dir) repos)
      (henri-notes-sync--run
       name dir
       (format
        (concat "git add -A "
                "&& (git diff --cached --quiet "
                "    || git commit -m %s) "
                "&& git pull --rebase --autostash "
                "&& git push")
        (shell-quote-argument msg))))))

;;;###autoload
(defun henri/notes-status-all ()
  "Print short porcelain status for every configured notes repo."
  (interactive)
  (let ((repos (henri-notes-sync--ensure-repos)))
    (henri-notes-sync--log "═══ status @ %s" (format-time-string "%F %T"))
    (display-buffer (henri-notes-sync--log-buffer))
    (pcase-dolist (`(,name . ,dir) repos)
      (henri-notes-sync--run
       name dir
       "git status --short --branch && echo '— ahead/behind —' && git rev-list --left-right --count @{u}...HEAD 2>/dev/null || true"))))

;;;###autoload
(defun henri/notes-magit (repo)
  "Open `magit-status' for one of the configured notes REPO directories."
  (interactive
   (list (let* ((repos (henri-notes-sync--ensure-repos))
                (names (mapcar #'car repos)))
           (completing-read "Notes repo: " names nil t))))
  (let ((dir (cdr (assoc repo (henri-notes-sync--valid-repos)))))
    (unless dir (user-error "[henri] Unknown notes repo: %s" repo))
    (if (require 'magit nil t)
        (magit-status-setup-buffer dir)
      (let ((default-directory (file-name-as-directory dir)))
        (vc-dir dir)))))

;; -- Keybindings ------------------------------------------------------------
;; Live under the existing `C-c g' (git) prefix.  `N' = "Notes" sub-prefix.
(global-set-key (kbd "C-c g N u") #'henri/notes-pull-all)
(global-set-key (kbd "C-c g N s") #'henri/notes-sync-all)
(global-set-key (kbd "C-c g N S") #'henri/notes-status-all)
(global-set-key (kbd "C-c g N m") #'henri/notes-magit)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c g N" "+notes-sync"))

(provide 'notes-sync)
;;; notes-sync.el ends here
