;;; lib-files.el --- Large files, buffer predicates -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)

;; Forward-declare (defined in init-custom.el); survives load-order / byte-compile quirks.
(defvar henri-large-file-threshold)
(defvar henri-large-file-hard-threshold)
(defvar henri-large-file-disable-modes)
(defvar henri-large-file-minor-highlighting-level)
(defvar henri-buffer-blacklist-prefixes)

(defun henri--prepare-for-large-files-a (size _op filename &rest _)
  "Run before `abort-if-file-too-large' for a user-visible heads-up.
So-long is handled by `global-so-long-mode' (`henri-first-file-hook')."
  (when (and (numberp size)
             (boundp 'henri-large-file-hard-threshold)
             (> size henri-large-file-hard-threshold))
    (message "[henri] Large file (~%d MB): %s"
             (/ size (* 1024 1024))
             (or filename ""))))

(when (fboundp 'advice-add)
  (advice-add 'abort-if-file-too-large :before #'henri--prepare-for-large-files-a))

(defun henri-buffer-real-p (buffer-or-name)
  "Non-nil if BUFFER-OR-NAME should appear in tabs / buffer UIs."
  (let* ((buf (cond ((bufferp buffer-or-name) buffer-or-name)
                    (t (get-buffer buffer-or-name))))
         (name (and buf (buffer-name buf))))
    (and name
         (not (string-prefix-p " " name))
         (cl-notany (lambda (p) (string-prefix-p p name))
                    (or (and (boundp 'henri-buffer-blacklist-prefixes)
                             henri-buffer-blacklist-prefixes)
                        '()))
         (or (not (string-prefix-p "magit" name))
             (file-name-extension name)))))

(defun henri/large-file-optimizations ()
  "Apply performance tweaks when buffer size exceeds `henri-large-file-threshold'."
  (when (and (boundp 'henri-large-file-threshold)
             buffer-file-name
             (> (buffer-size) henri-large-file-threshold))
    (when (and (boundp 'henri-large-file-disable-modes)
               (memq 'line-numbers henri-large-file-disable-modes)
               (bound-and-true-p display-line-numbers-mode))
      (display-line-numbers-mode -1))
    (when (and (memq 'flycheck henri-large-file-disable-modes)
               (bound-and-true-p flycheck-mode))
      (flycheck-mode -1))
    (when (and (memq 'eglot henri-large-file-disable-modes)
               (boundp 'eglot--managed-mode)
               eglot--managed-mode)
      (setq-local eglot--managed-mode nil)
      (message "[henri] Disabled eglot for large file: %s" (buffer-name)))
    (when (memq 'tree-sitter henri-large-file-disable-modes)
      (when (boundp 'treesit-font-lock-feature-list)
        (setq-local treesit-font-lock-feature-list '((comment) (string) (keyword))))
      (when (boundp 'treesit-font-lock-level)
        (setq-local treesit-font-lock-level 1)))
    (when (and (memq 'font-lock henri-large-file-disable-modes)
               (boundp 'font-lock-maximum-decoration))
      (setq-local font-lock-maximum-decoration henri-large-file-minor-highlighting-level))
    (setq-local bidi-display-reordering nil
                bidi-paragraph-direction 'left-to-right)
    (message "[henri] Large file optimizations applied (size=%d)." (buffer-size))))

(add-hook 'find-file-hook #'henri/large-file-optimizations)

(defun henri/restore-from-large-file ()
  "Re-enable common minor modes after `henri/large-file-optimizations'."
  (interactive)
  (when (not display-line-numbers-mode) (display-line-numbers-mode 1))
  (when (fboundp 'flycheck-mode) (flycheck-mode 1))
  (when (and (fboundp 'eglot-ensure) (not (bound-and-true-p eglot--managed-mode)))
    (eglot-ensure))
  (when (boundp 'treesit-font-lock-feature-list)
    (kill-local-variable 'treesit-font-lock-feature-list))
  (when (boundp 'treesit-font-lock-level)
    (kill-local-variable 'treesit-font-lock-level))
  (kill-local-variable 'font-lock-maximum-decoration)
  (message "[henri] Restored modes for current buffer."))

(use-package so-long
  :ensure nil
  :hook (henri-first-file-hook . global-so-long-mode))

(provide 'lib-files)
;;; lib-files.el ends here
