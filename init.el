;; ============
;; 扩展包管理 ;;
;; ============
;; 扩展包链接修改之后，可以使用package-refresh-contents进行刷新
(require 'package)
(setq package-archives '(
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
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

;; ============
;; 初始配置 ;;
;; ============
;; 设置Emacs的外观
(load-theme 'modus-operandi t) ; 使用Modus Operandi主题，这是一个现代化且舒适的主题
(menu-bar-mode -1) ; 关闭菜单栏
(tool-bar-mode -1) ; 关闭工具栏
(scroll-bar-mode -1) ; 关闭滚动条
(global-display-line-numbers-mode) ; 显示行号
(column-number-mode) ; 显示列号
(global-hl-line-mode t) ; 高亮当前行
(blink-cursor-mode 0) ; 关闭光标闪烁

;; 提高Emacs的性能
(setq gc-cons-threshold 100000000) ; 增加GC阈值，提升性能
(setq inhibit-startup-screen t) ; 禁用启动屏幕
(setq auto-save-default nil) ; 禁止自动保存文件
(setq backup-inhibited t) ; 禁止备份文件
(setq ring-bell-function 'ignore) ; 关闭提示音

;; 设置快捷键
(global-set-key (kbd "C-x C-b") 'ibuffer) ; 使用ibuffer代替默认的buffer列表
(global-set-key (kbd "M-x") 'counsel-M-x) ; 使用counsel提供更好的M-x功能

;; 其他个性化设置
(setq-default indent-tabs-mode nil) ; 使用空格而不是制表符缩进

;; 启用更快的模式
(global-auto-revert-mode t) ; 自动重新加载文件
(electric-pair-mode 1) ; 自动补全括号
(global-subword-mode 1) ; 在单词内部移动光标


;; 键位配置
;; Evil-mode
(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)
(evil-mode 1)

;; 调用管理设置
(load-file (expand-file-name "lisp/init-managing.el" user-emacs-directory))
;; 调用风格设置
(load-file (expand-file-name "lisp/init-styling.el" user-emacs-directory))
;; 调用本目录下的lisp文件夹中的init-programming.el文件，加载语言服务器相关配置
(load-file (expand-file-name "lisp/init-programming.el" user-emacs-directory))
;; 调用本目录下的lisp文件夹中的init-writing.el文件，加载语言服务器相关配置
(load-file (expand-file-name "lisp/init-writing.el" user-emacs-directory))



(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
;;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
