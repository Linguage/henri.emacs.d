;;;
;; ============
;; 扩展包管理 ;;
;; ============
;; 扩展包链接修改之后，可以使用package-refresh-contents进行刷新
(require 'package)
(setq package-archives '(
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu-devel" . "https://elpa.gnu.org/devel/")
                         ))
; (setq package-archives '(("tunu_gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;                          ("tunu_nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
;                          ("tunu_melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
; (package-refresh-contents)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))


;; 调用管理设置
(load-file (expand-file-name "lisp/init-managing.el" user-emacs-directory))
;; 调用风格设置
(load-file (expand-file-name "lisp/init-styling.el" user-emacs-directory))
;; 调用本目录下的lisp文件夹中的init-programming.el文件，加载语言服务器相关配置
(load-file (expand-file-name "lisp/init-programming.el" user-emacs-directory))
;; 调用本目录下的lisp文件夹中的init-writing.el文件，加载语言服务器相关配置
(load-file (expand-file-name "lisp/init-writing.el" user-emacs-directory))

;; 键位配置
;; which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Evil-mode
(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)
(evil-mode 1)

;; Set zsh as the default shell
(setq shell-file-name "/bin/zsh")  ;; 确保这是 zsh 的实际路径
(setq explicit-shell-file-name shell-file-name)
(add-to-list 'exec-path "/bin")  ;; 更新 exec-path，如果 zsh 不在默认路径中
(setenv "SHELL" shell-file-name)


(use-package exec-path-from-shell
  :ensure t
  :config
  ;; Initialize environment variables from the shell
  (exec-path-from-shell-initialize)
  ;; Copy PATH and other necessary environment variables
  (exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-copy-env "ZSH")
  (exec-path-from-shell-copy-env "CONDA_PREFIX")
  (exec-path-from-shell-copy-env "CONDA_DEFAULT_ENV")
  (exec-path-from-shell-copy-env "SHELL"))


;; 配置 eshell
(defun open-eshell-and-split-windows ()
  "Open eshell and split windows for optimal layout."
  (interactive)
  (split-window-right)                ;; 在右侧分割窗口
  (other-window 1)                    ;; 切换到右侧窗口
  (split-window-below)                ;; 在右侧窗口分割出下方窗口
  (eshell)                            ;; 打开 eshell
  (other-window 1)                    ;; 切换到上方窗口
  (quickrun))                         ;; 打开 quickrun

;; 设置初始布局
(defun my-setup-windows ()
  "Setup my custom window layout."
  (interactive)
  (delete-other-windows)              ;; 关闭其他窗口
  ; (neotree-show)                      ;; 打开 neotree
  (open-eshell-and-split-windows))    ;; 打开 eshell 和 quickrun

;; 在 Emacs 启动时设置窗口布局
(add-hook 'emacs-startup-hook 'my-setup-windows)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
;;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
