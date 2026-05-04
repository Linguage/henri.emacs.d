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

(defgroup henri-experiments nil
  "Experimental features (can be unstable)."
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

(defcustom henri-enable-grip nil
  "Enable grip-mode for GitHub Markdown preview (requires grip CLI + GitHub token)."
  :type 'boolean :group 'henri-writing)

(defcustom henri-enable-rime nil
  "Enable emacs-rime input method integration."
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

;; Markdown sub-module toggles -----------------------------------------------
(defcustom henri-md-enable-export t
  "启用 Markdown → PDF/HTML/docx 导出模块 `markdown-export'."
  :type 'boolean :group 'henri-writing)

(defcustom henri-md-enable-toc t
  "启用 Markdown 目录/导航模块 `markdown-nav'（含 markdown-toc、outline）。"
  :type 'boolean :group 'henri-writing)

(defcustom henri-md-enable-notes t
  "启用 Markdown 笔记增强模块 `markdown-notes'（截图、拖拽、字数统计）。"
  :type 'boolean :group 'henri-writing)

(defcustom henri-md-enable-lint nil
  "启用 Markdown lint 模块 `markdown-lint'（需 markdownlint CLI）。"
  :type 'boolean :group 'henri-writing)

(defcustom henri-md-enable-template nil
  "启用 Markdown 博客 front-matter 模板模块 `markdown-template'。"
  :type 'boolean :group 'henri-writing)

(defcustom henri-md-pdf-engine "xelatex"
  "Markdown → PDF 使用的 pandoc LaTeX 引擎。
可选 \"xelatex\" 或 \"tectonic\"."
  :type '(choice (const "xelatex") (const "tectonic"))
  :group 'henri-writing)

(defcustom henri-md-export-output-dir nil
  "导出输出目录；nil 表示与源文件同目录。"
  :type '(choice (const :tag "与源文件同目录" nil)
                 (directory :tag "指定目录"))
  :group 'henri-writing)

;; PDF 子模块开关 ---------------------------------------------------------------
(defcustom henri-pdf-enable-tools t
  "启用 pdf-tools 子模块 `pdf-base'（在 Emacs 内查看 PDF）。
首次使用需 `brew install poppler automake autoconf pkg-config' 并执行
`M-x pdf-tools-install' 完成 epdfinfo 本地编译。"
  :type 'boolean :group 'henri-writing)

(defcustom henri-org-cjk-serif-family "TsangerJinKai02"
  "Org 模式中文正文使用的衬线字体族名，须与 `font-family-list' 中某项完全一致。
为 nil 时按 `henri--org-cjk-serif-candidates'（见 `visual-fonts.el'）顺序自动探测，
例如已安装的「思源宋体」「Noto Serif CJK」或系统「宋体-简」等。"
  :type '(choice (const :tag "自动探测" nil) string)
  :group 'henri-writing)

(defcustom henri-org-cjk-sans-family nil
  "Org 模式标题、文档标题等使用的无衬线中文字体族名，须与 `font-family-list' 中某项完全一致。
为 nil 时按 `henri--org-cjk-sans-candidates'（见 `visual-fonts.el'）自动探测。"
  :type '(choice (const :tag "自动探测" nil) string)
  :group 'henri-writing)

(defcustom henri-lsp-auto-format t
  "Auto format buffer on save in eglot managed buffers."
  :type 'boolean :group 'henri-programming)

(defcustom henri-lsp-format-size-threshold 500000
  "Maximum buffer size (bytes) to still auto format with LSP."
  :type 'integer :group 'henri-programming)

(defcustom henri-eglot-auto-major-modes
  '(c-mode c++-mode c-ts-mode c++-ts-mode python-mode python-ts-mode fortran-mode)
  "Major modes in which `prog-mode-hook' runs `eglot-ensure'.
Extend this list when you add `eglot-server-programs' entries in
`init-programming.el'.  `emacs-lisp-mode' is intentionally excluded."
  :type '(repeat symbol)
  :group 'henri-programming)

(defcustom henri-large-file-threshold 5000000
  "Threshold (bytes) above which large-file optimizations apply."
  :type 'integer :group 'henri-performance)

(defcustom henri-large-file-hard-threshold (* 10 1024 1024)
  "File size (bytes) above which pre-read mitigation runs (see `lib-files').
This pairs with `henri-large-file-detected-bytes' and `so-long-minor-mode'."
  :type 'integer :group 'henri-performance)

(defcustom henri-active-profile
  (or (getenv "HENRI_PROFILE") (system-name))
  "Profile name loaded from \"profile-<name>.el\" in `user-emacs-directory'.
Override with environment variable HENRI_PROFILE."
  :type 'string :group 'henri-core)

(defcustom henri-font-default-size 140
  "Default `:height' for face `default' (1/10 pt) via `lib-fonts'."
  :type 'integer :group 'henri-theme)

(defcustom henri-font-big-size 220
  "Presentation height used by `henri-big-font-mode'."
  :type 'integer :group 'henri-theme)

(defcustom henri-buffer-blacklist-prefixes
  '("*epc" "*helm" "*Helm" "*Compile-Log*" "*lsp" "*company"
    "*Flycheck" "*tramp" " *Mini" "*help" "*straight" " *temp" "*Help"
    "*mybuf" "*Warnings*" "*Messages*" "*scratch*" "*Completions*"
    "*Async-native-compile-log*" "*eshell*" "*shell*" "*terminal*" "*vterm*"
    "*markdown-preview*" "*grip-")
  "Prefix matches for buffer names considered hidden (tabs, see `henri-buffer-real-p')."
  :type '(repeat string) :group 'henri-core)

(defcustom henri-large-file-disable-modes '(flycheck font-lock tree-sitter eglot)
  "List of subsystems to disable for large files.
Symbols understood: flycheck font-lock tree-sitter eglot line-numbers." :type '(set (const flycheck) (const font-lock) (const tree-sitter) (const eglot) (const line-numbers)) :group 'henri-performance)

(defcustom henri-large-file-minor-highlighting-level 1
  "Value for `font-lock-maximum-decoration' in large buffers." :type 'integer :group 'henri-performance)

(defcustom henri-experimental-completion-backend 'ivy
  "Completion stack selection (experimental)." :type '(choice (const ivy) (const vertico) (const corfu)) :group 'henri-experiments)

(defcustom henri-enable-byte-compile-check t
  "Whether to run batch byte-compile in CI helper scripts." :type 'boolean :group 'henri-performance)

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
  (require 'doom-themes nil t)
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

;; Only once even if `init-custom' is reloaded.
(defvar henri--theme-refresh-timer nil)
(unless henri--theme-refresh-timer
  (setq henri--theme-refresh-timer
        (run-at-time "1 hour" 3600 #'henri/refresh-theme-if-needed)))

(defun henri/select-theme (theme)
  "Interactively pick THEME (symbol) from `henri-available-themes' and apply it as fixed mode."
  (interactive (list (intern (completing-read "Theme: " (mapcar #'symbol-name henri-available-themes)))))
  (setq henri-theme-mode 'fixed
    henri-theme-fixed-theme theme)
  (henri/apply-current-theme t))

;; Portable paths -------------------------------------------------------------

(defgroup henri-paths nil
  "Filesystem locations (tune per machine)."
  :group 'henri-core :prefix "henri-")

(defcustom henri-shell
  (or (executable-find "zsh") "/bin/sh")
  "Shell for subprocesses, `quickrun-shell', and `exec-path-from-shell'.
Prefers zsh when found; else /bin/sh (portable on NixOS, servers, Termux)."
  :type 'file
  :group 'henri-paths)

(defcustom henri-notes-directory "~/Documents/EmacsNotes/"
  "Root directory for notes, journal, and other personal writing trees."
  :type 'directory
  :group 'henri-paths)

(defcustom henri-projects-directory "~/projects/"
  "Root searched by `projectile-project-search-path'."
  :type 'directory
  :group 'henri-paths)

(defcustom henri-leetcode-directory "~/leetcode/"
  "Directory used by `leetcode' package for solutions."
  :type 'directory
  :group 'henri-paths)

(defcustom henri-conda-home "~/miniconda3/"
  "Conda installation prefix (same as CONDA_ROOT for many installs)."
  :type 'directory
  :group 'henri-paths)

(defcustom henri-conda-envs-directory "~/.conda/envs"
  "Environment directory for pyvenv WORKON_HOME-style layouts."
  :type 'directory
  :group 'henri-paths)

(defcustom henri-conda-default-env "Henri_env"
  "Default conda env name activated from Python hook when available."
  :type 'string
  :group 'henri-paths)

(defcustom henri-org-html-themes-directory
  (expand-file-name "lisp/writing/org/org-html-themes" user-emacs-directory)
  "Local checkout of org-html-themes (see `lisp/writing/org/install-themes.sh')."
  :type 'directory
  :group 'henri-paths)

(provide 'init-custom)
;;; init-custom.el ends here
