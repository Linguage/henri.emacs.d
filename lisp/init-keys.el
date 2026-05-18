;;; init-keys.el --- Henri keybinding governance -*- lexical-binding: t -*-

;;; Commentary:
;; Central prefix labels and a few cross-module global bindings.  Mode-local
;; bindings stay in their own modules so their ownership remains obvious.

;;; Code:

(defun henri/keybindings--legacy-message (old-key new-key)
  "Show a migration message from OLD-KEY to NEW-KEY."
  (message "[henri] %s 已迁移到 %s" old-key new-key))

(defun henri/toggle-line-numbers ()
  "Toggle global line numbers."
  (interactive)
  (if (bound-and-true-p global-display-line-numbers-mode)
      (global-display-line-numbers-mode -1)
    (global-display-line-numbers-mode 1)))

(defun henri/load-theme-interactively ()
  "Select and load an Emacs theme interactively."
  (interactive)
  (call-interactively #'load-theme))

(defun henri/open-keybindings-doc ()
  "Open the keybindings reference document."
  (interactive)
  (find-file (expand-file-name "docs/tutorials/keybindings.md"
                               user-emacs-directory)))

(global-set-key (kbd "C-c t t") #'henri/load-theme-interactively)
(global-set-key (kbd "C-c t b") #'henri-big-font-mode)
(global-set-key (kbd "C-c t l") #'henri/toggle-line-numbers)
(global-set-key (kbd "C-c h k") #'henri/open-keybindings-doc)
(global-set-key (kbd "C-c l") #'org-store-link)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c a" "agenda"
    "C-c A" "+academic"
    "C-c b" "+buffer"
    "C-c c" "capture"
    "C-c d" "+debug"
    "C-c e" "+eglot"
    "C-c f" "+file"
    "C-c g" "+git"
    "C-c g m" "+smerge"
    "C-c h" "+help"
    "C-c j" "+journal"
    "C-c l" "store-link"
    "C-c m" "+mode"
    "C-c m d" "+document"
    "C-c m e" "+md-export"
    "C-c m h" "+org-html"
    "C-c m i" "+md-insert"
    "C-c m l" "+org-latex"
    "C-c m s" "+md-theme"
    "C-c m t" "+md-toc"
    "C-c m T" "+md-template"
    "C-c m v" "+org-view"
    "C-c m x" "org-checkbox"
    "C-c n" "+roam"
    "C-c o" "+org"
    "C-c r" "+rime"
    "C-c s" "+search"
    "C-c t" "+toggle"
    "C-c w" "+window"))

(provide 'init-keys)

;;; init-keys.el ends here
