;;; lib-files.el --- Large files, buffer predicates -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)

(defvar henri-large-file-detected-bytes nil
  "If non-nil, file was large before insert; see `henri--prepare-for-large-files-a'.")
(put 'henri-large-file-detected-bytes 'permanent-local t)

(defun henri--prepare-for-large-files-a (size _op filename &rest _)
  "Run before `abort-if-file-too-large' to flag very large FILENAME."
  (when (and (numberp size)
             (boundp 'henri-large-file-hard-threshold)
             (> size henri-large-file-hard-threshold))
    (ignore-errors
      (setq-local henri-large-file-detected-bytes size))
    (message "[henri] Large file (~%d MB): %s"
             (/ size (* 1024 1024))
             (or filename ""))))

(when (fboundp 'advice-add)
  (advice-add 'abort-if-file-too-large :before #'henri--prepare-for-large-files-a))

(defun henri/so-long-after-large-file-detected ()
  "Turn on `so-long-minor-mode' when this buffer was flagged as oversized."
  (when (and henri-large-file-detected-bytes buffer-file-name)
    (when (fboundp 'so-long-minor-mode)
      (so-long-minor-mode 1))
    (kill-local-variable 'henri-large-file-detected-bytes)))

(add-hook 'find-file-hook #'henri/so-long-after-large-file-detected)

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
