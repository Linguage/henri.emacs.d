;;; init-custom.el --- User customization groups & defcustoms -*- lexical-binding: t -*-

;;; Commentary:
;; Central place for defgroup/defcustom so other modules can rely on them.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defgroup henri-core nil
  "Core configuration for Henri's Emacs."
  :group 'convenience :prefix "henri-")

(defgroup henri-theme nil
  "Theme / UI customization."
  :group 'henri-core :prefix "henri-")

(defgroup henri-programming nil
  "Programming related customization."
  :group 'henri-core :prefix "henri-")

(defgroup henri-writing nil
  "Writing / Org / Markdown customization."
  :group 'henri-core :prefix "henri-")

(defgroup henri-performance nil
  "Performance & large file behavior."
  :group 'henri-core :prefix "henri-")

;; Theme strategy -------------------------------------------------------------
(defcustom henri-theme-mode 'time
  "Theme selection strategy.
- time   : day/night switch by hour.
- random : pick random theme avoiding recent repeat.
- fixed  : always use `henri-theme-fixed-theme'."
  :type '(choice (const time) (const random) (const fixed))
  :group 'henri-theme)

(defcustom henri-theme-day-theme 'doom-acario-light
  "Daytime theme (09:00–18:00)."
  :type 'symbol :group 'henri-theme)

(defcustom henri-theme-night-theme 'doom-one
  "Night theme (18:00–09:00)."
  :type 'symbol :group 'henri-theme)

(defcustom henri-theme-fixed-theme 'doom-one
  "Theme used when `henri-theme-mode' is `fixed'."
  :type 'symbol :group 'henri-theme)

(defcustom henri-theme-random-recent-window 4
  "Number of previously used themes to avoid when randomizing."
  :type 'integer :group 'henri-theme)

(defcustom henri-enable-centaur-tabs t
  "Enable centaur-tabs integration."
  :type 'boolean :group 'henri-theme)

(defcustom henri-enable-magit t
  "Enable Magit (VCS interface)."
  :type 'boolean :group 'henri-programming)

(defcustom henri-enable-leetcode t
  "Enable leetcode integration."
  :type 'boolean :group 'henri-programming)

(defcustom henri-enable-grip t
  "Enable grip-mode for GitHub Markdown preview."
  :type 'boolean :group 'henri-writing)

;; Org sub-module toggles ----------------------------------------------------
(defcustom henri-org-enable-base t
  "Enable base Org configuration module (required for others)."
  :type 'boolean :group 'henri-writing)
(defcustom henri-org-enable-latex t
  "Enable Org LaTeX/PDF export enhancements."
  :type 'boolean :group 'henri-writing)
(defcustom henri-org-enable-journal t
  "Enable Org journal / agenda enhancements."
  :type 'boolean :group 'henri-writing)
(defcustom henri-org-enable-html t
  "Enable Org HTML export theming module."
  :type 'boolean :group 'henri-writing)
(defcustom henri-org-enable-academic t
  "Enable Org academic writing module."
  :type 'boolean :group 'henri-writing)

(defcustom henri-lsp-auto-format t
  "Auto format buffer on save in eglot managed buffers."
  :type 'boolean :group 'henri-programming)

(defcustom henri-lsp-format-size-threshold 500000
  "Maximum buffer size (bytes) to still auto format with LSP."
  :type 'integer :group 'henri-programming)

(defcustom henri-large-file-threshold 5000000
  "Threshold (bytes) above which large-file optimizations apply."
  :type 'integer :group 'henri-performance)

(defcustom henri-backup-enable t
  "Whether to enable centralized backups (future phase)."
  :type 'boolean :group 'henri-performance)

(defcustom henri-health-report-on-startup t
  "Emit a health report after startup if non-nil."
  :type 'boolean :group 'henri-core)

(defvar henri--recent-themes nil "Recently used themes (most recent first).")

(defvar henri-available-themes
  '(doom-Iosvkem doom-acario-dark doom-acario-light doom-challenger-deep
    doom-city-lights doom-dark+ doom-dracula doom-ephemeral doom-fairy-floss
    doom-gruvbox doom-horizon doom-laserwave doom-manegarm doom-material
    doom-molokai doom-monokai-classic doom-monokai-pro doom-monokai-spectrum
    doom-moonlight doom-nord-light doom-nord doom-nova doom-oceanic-next
    doom-one-light doom-one doom-opera-light doom-opera doom-outrun-electric
    doom-palenight doom-peacock doom-rouge doom-snazzy doom-solarized-dark
    doom-solarized-light doom-sourcerer doom-spacegrey doom-tomorrow-day
    doom-tomorrow-night doom-vibrant doom-wilmersdorf)
  "Candidate themes for random/time strategies.")

(defun henri--time-daytime-p ()
  "Return non-nil if current hour is considered daytime."
  (let ((h (string-to-number (format-time-string "%H"))))
    (and (>= h 9) (< h 18))))

(defun henri--select-theme ()
  "Select a theme symbol according to `henri-theme-mode'."
  (pcase henri-theme-mode
    ('fixed henri-theme-fixed-theme)
    ('time (if (henri--time-daytime-p) henri-theme-day-theme henri-theme-night-theme))
    ('random (henri--random-theme))
    (_ henri-theme-fixed-theme)))

(defun henri--random-theme ()
  "Pick a random theme avoiding the last N recent ones."
  (let* ((avoid (cl-subseq henri--recent-themes 0 (min henri-theme-random-recent-window
                                                      (length henri--recent-themes))))
         (pool (seq-remove (lambda (th) (member th avoid)) henri-available-themes)))
    (unless pool (setq pool henri-available-themes))
    (nth (random (length pool)) pool)))

(defun henri/apply-current-theme (&optional force)
  "Apply theme chosen by strategy. FORCE to bypass recent list avoidance."
  (interactive "P")
  (let* ((chosen (if force (henri--select-theme) (henri--select-theme))))
    (dolist (th custom-enabled-themes) (disable-theme th))
    (load-theme chosen t)
    (setq henri--recent-themes (cons chosen (delete chosen henri--recent-themes)))
    (run-hooks 'henri-theme-changed-hook)
    (message "[henri] Theme applied: %s" chosen)))

(defun henri/refresh-theme-if-needed ()
  "Re-evaluate theme strategy (used for hourly refresh under time mode)."
  (when (eq henri-theme-mode 'time)
    (henri/apply-current-theme)))

(run-at-time "1 hour" 3600 #'henri/refresh-theme-if-needed)

(provide 'init-custom)
;;; init-custom.el ends here
