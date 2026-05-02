;;; visual-themes.el --- Theme packages (doom-themes) -*- lexical-binding: t -*-

;;; Commentary:
;; `henri/apply-current-theme' and theme strategy remain in `init-custom.el'.
;; Register `henri-theme-changed-hook' before first theme application so fonts
;; re-apply after `run-hooks'.

;;; Code:

(defvar henri-theme-changed-hook nil
  "Hook run after `henri/apply-current-theme' applies a theme.")

(add-hook 'henri-theme-changed-hook #'henri/apply-fonts)

(use-package doom-themes
  :ensure t
  :demand t
  :config
  (when (fboundp 'doom-themes-org-config)
    (doom-themes-org-config))
  (when (fboundp 'henri/apply-current-theme)
    (henri/apply-current-theme)))

(provide 'visual-themes)

;;; visual-themes.el ends here
