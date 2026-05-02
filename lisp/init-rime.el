;;; init-rime.el --- Rime input method configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Configures emacs-rime for native, zero-latency Chinese input.

;;; Code:

(use-package popup
  :ensure t
  :defer t)

(use-package posframe
  :ensure t
  :defer t)

(use-package rime
  :ensure t
  :after (popup posframe)
  :custom
  (default-input-method "rime")
  (rime-user-data-dir (directory-file-name (expand-file-name
                                            henri-rime-directory)))
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
