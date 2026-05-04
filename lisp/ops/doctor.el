;;; doctor.el --- Dependency / environment checks -*- lexical-binding: t -*-

;;; Code:

(require 'lib-fonts)

(defun henri--doctor-check (kind name pred)
  (princ (format "  %-7s %-32s %s\n" kind name (if pred "OK" "MISSING"))))

(defun henri--doctor-feature-status (feature)
  "Return \"loaded\", \"available\", or \"MISSING\" for FEATURE symbol."
  (let ((name (symbol-name feature)))
    (cond ((featurep feature) "loaded")
          ((locate-library name) "available")
          (t "MISSING"))))

(defun henri--doctor-print-feat (feature)
  (princ (format "  %-8s %-22s %s\n" "feat"
                 (symbol-name feature)
                 (henri--doctor-feature-status feature))))

(defun henri/doctor ()
  "Show executables, fonts, directories, and common features."
  (interactive)
  (let ((buf (get-buffer-create "*Henri Doctor*")))
    (with-current-buffer buf
      (erase-buffer)
      (let ((standard-output (current-buffer)))
        (princ "[henri] doctor\n\n")
        (princ "Executables:\n")
        (dolist (e '("git" "rg" "fd" "clangd" "pylsp" "fortls" "octave"
                     "pandoc" "grip" "node" "markdownlint"
                     "xelatex" "tectonic" "pngpaste"
                     "pkg-config" "automake" "autoconf"))
          (henri--doctor-check "exec" e (executable-find e)))
        (princ "\nFonts:\n")
        (dolist (f '("Cascadia Code NF" "JetBrains Mono"
                     "PingFang SC" "Noto Sans CJK SC"))
          (henri--doctor-check "font" f (henri/font-family-available-p f)))
        (princ "\nDirectories:\n")
        (dolist (d (list (and (boundp 'henri-notes-directory) henri-notes-directory)
                         (and (boundp 'henri-projects-directory) henri-projects-directory)
                         (and (boundp 'henri-conda-home) henri-conda-home)
                         (and (boundp 'henri-org-html-themes-directory)
                              henri-org-html-themes-directory)))
          (when d
            (henri--doctor-check "dir" d (file-directory-p (expand-file-name d)))))
        (princ "\nFeatures:\n")
        (dolist (f '(eglot treesit doom-themes which-key markdown-mode
                     magit diff-hl org markdown-toc pdf-tools))
          (henri--doctor-print-feat f))
        (princ "\nProfile / theme:\n")
        (let ((themes (when (boundp 'custom-enabled-themes)
                        custom-enabled-themes)))
          (princ (format "  profile=%S  theme-mode=%S  enabled-themes=%S\n"
                         (when (boundp 'henri-active-profile) henri-active-profile)
                         (when (boundp 'henri-theme-mode) henri-theme-mode)
                         themes)))))
    (pop-to-buffer buf)))

(global-set-key (kbd "C-c h d") #'henri/doctor)

(provide 'doctor)
;;; doctor.el ends here
