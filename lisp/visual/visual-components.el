;;; visual-components.el --- Basic UI, modeline, icons, tabs -*- lexical-binding: t -*-

;;; Commentary:
;; Visual chrome loaded after `visual-themes' so `doom-themes-after-load-theme-hook'
;; and faces are available.  Font keybindings and `henri/apply-fonts' on GUI startup.

;;; Code:

;; =============================================================================
;; 基础界面设置

(add-to-list 'default-frame-alist '(top . 0.5))
(add-to-list 'default-frame-alist '(left . 0.5))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

(global-display-line-numbers-mode)
(column-number-mode)
(global-hl-line-mode t)

(setq inhibit-startup-screen t)
(setq auto-save-default nil)
(setq backup-inhibited t)
(setq ring-bell-function 'ignore)

(global-auto-revert-mode t)
(electric-pair-mode 1)
(global-subword-mode 1)

;; =============================================================================
;; 状态栏 / 图标

(use-package nerd-icons
  :ensure t
  :demand t)

(use-package doom-modeline
  :ensure t
  :demand t
  :after nerd-icons
  :init
  (setq doom-modeline-height 28
        doom-modeline-bar-width 3
        doom-modeline-window-width-limit 85
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-buffer-encoding nil
        doom-modeline-indent-info nil
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-irc nil
        doom-modeline-lsp nil
        doom-modeline-env-version nil
        doom-modeline-check-simple-format t
        doom-modeline-vcs-max-length 12)
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-theme 'doom-modeline-light))

(use-package all-the-icons
  :ensure t
  :config
  (unless (member "all-the-icons" (font-family-list))
    (when (display-graphic-p)
      (message "[henri] all-the-icons fonts not found; run M-x all-the-icons-install-fonts if icons look wrong."))))

;; windows系统需要安装NerdFontsSymbolsOnly字体，否则会出现图标显示异常

(when window-system
  (henri/apply-fonts))

(global-set-key (kbd "C-=") #'henri/font-size-adjust)
(global-set-key (kbd "C--")
                (lambda (n) (interactive "p")
                  (henri/font-size-adjust (- (or n 1)))))
(global-set-key (kbd "C-c F r") #'henri/font-size-reset)
(global-set-key (kbd "C-c F b") #'henri-big-font-mode)

;; =============================================================================
;; 标签页 (Centaur Tabs)

(when (and (boundp 'henri-enable-centaur-tabs) henri-enable-centaur-tabs)
  (use-package centaur-tabs
    :ensure t
    :defer t
    :if (display-graphic-p)
    :preface
    (defun henri/setup-centaur-tabs ()
      "安全地设置和启动 centaur-tabs。"
      (interactive)
      (run-with-idle-timer
       2 nil
       (lambda ()
         (when (and (boundp 'henri-enable-centaur-tabs)
                    henri-enable-centaur-tabs
                    (display-graphic-p)
                    (not (bound-and-true-p centaur-tabs-mode)))
           (require 'centaur-tabs nil t)
           (centaur-tabs-mode 1)
           (message "Centaur-tabs 已启用")
           (henri/setup-tabs-mouse-support)
           (henri/maybe-enable-tabs-icons)))))

    (defun henri/setup-tabs-mouse-support ()
      "设置标签页的鼠标滚轮切换。"
      (global-set-key [mouse-4] 'centaur-tabs-backward)
      (global-set-key [mouse-5] 'centaur-tabs-forward))

    (defun henri/maybe-enable-tabs-icons ()
      "在图标字体可用时启用 centaur-tabs 图标。"
      (when (and (featurep 'all-the-icons)
                 (display-graphic-p))
        (condition-case err
            (when (and (> (length (font-family-list)) 0)
                       (or (member "all-the-icons" (font-family-list))
                           (member "All The Icons" (font-family-list))))
              (setq centaur-tabs-set-icons t)
              (when (bound-and-true-p centaur-tabs-mode)
                (centaur-tabs-mode -1)
                (centaur-tabs-mode 1))
              (message "Centaur-tabs 图标已启用"))
          (error
           (message "图标启用失败: %s" err)))))

    (defun henri/centaur-tabs-theme ()
      "使 centaur-tabs 与当前 doom 主题对齐。"
      (when (and (display-graphic-p)
                 (bound-and-true-p centaur-tabs-mode))
        (condition-case nil
            (centaur-tabs-headline-match)
          (error nil))))
    :bind (("C-<prior>" . centaur-tabs-backward)
           ("C-<next>" . centaur-tabs-forward))
    :init
    (when (display-graphic-p)
      (add-hook 'henri-first-buffer-hook #'henri/setup-centaur-tabs))
    :config
    (setq centaur-tabs-style "bar"
          centaur-tabs-height 32
          centaur-tabs-set-icons nil
          centaur-tabs-show-new-tab-button t
          centaur-tabs-set-close-button t
          centaur-tabs-close-button "×"
          centaur-tabs-new-tab-button "+"
          centaur-tabs-set-modified-marker t
          centaur-tabs-modified-marker "●"
          centaur-tabs-adjust-buffer-order t
          centaur-tabs-enable-buffer-alphabetical-reordering t
          centaur-tabs-enable-ido-completion nil)
    (defun centaur-tabs-buffer-groups ()
      "自定义缓冲区分组规则。"
      (list
       (cond
        ((derived-mode-p 'org-mode) "Org")
        ((or (derived-mode-p 'prog-mode)
             (derived-mode-p 'python-mode)
             (derived-mode-p 'emacs-lisp-mode)
             (derived-mode-p 'js-mode)
             (derived-mode-p 'c-mode)
             (derived-mode-p 'java-mode))
         "Programming")
        ((or (derived-mode-p 'text-mode)
             (derived-mode-p 'markdown-mode)
             (derived-mode-p 'latex-mode))
         "Text")
        ((derived-mode-p 'dired-mode) "Dired")
        ((string-equal "*" (substring (buffer-name) 0 1)) "System")
        (t "General"))))
    (defun centaur-tabs-hide-tab (x)
      "隐藏不需要显示标签页的缓冲区（共用 `henri-buffer-real-p'）。"
      (not (henri-buffer-real-p x)))
    (add-hook 'doom-themes-after-load-theme-hook #'henri/centaur-tabs-theme)))

(provide 'visual-components)

;;; visual-components.el ends here
