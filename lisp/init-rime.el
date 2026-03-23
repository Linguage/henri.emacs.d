;;; init-rime.el --- Rime input method configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Configures emacs-rime for native, zero-latency Chinese input.

;;; Code:

(use-package rime
  :ensure t
  :custom
  (default-input-method "rime")
  (rime-user-data-dir (expand-file-name "rime" user-emacs-directory))
  ;; Show candidates using a popup frame, follows cursor natively
  (rime-show-candidate 'posframe)
  ;; Prevent rime candidates from grabbing absolute focus
  (rime-posframe-properties '((internal-border-width . 2)
                              (background-color . "#333333")
                              (foreground-color . "#dcdccc")))
  :bind
  ;; Press Ctrl+\ to toggle Emacs native input method (Rime)
  ("C-\\" . toggle-input-method))

(provide 'init-rime)
;;; init-rime.el ends here
