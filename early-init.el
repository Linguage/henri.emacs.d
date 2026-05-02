;;; early-init.el --- Early startup optimizations -*- lexical-binding: t; no-byte-compile: t -*-

;; This file is loaded before init.el (Emacs 27+). Keep it minimal and side-effect light.

;; Increase GC threshold during startup for speed.
(setq gc-cons-threshold (* 128 1024 1024)
      gc-cons-percentage 0.6)

;; Defer package initialization; we'll do it explicitly in init.el
(setq package-enable-at-startup nil)

;; Temporarily disable file-name handlers (restore in init.el)
(defvar henri--saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Inhibit frame resizing & some UI elements early.
(setq frame-inhibit-implied-resize t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Shape the first graphical frame before it is displayed.
(setq frame-resize-pixelwise t)
(add-to-list 'initial-frame-alist '(width . 118))
(add-to-list 'initial-frame-alist '(height . 38))
(add-to-list 'initial-frame-alist '(top . 0.5))
(add-to-list 'initial-frame-alist '(left . 0.5))
(add-to-list 'initial-frame-alist '(alpha . (95 . 95)))
(add-to-list 'default-frame-alist '(width . 118))
(add-to-list 'default-frame-alist '(height . 38))
(add-to-list 'default-frame-alist '(top . 0.5))
(add-to-list 'default-frame-alist '(left . 0.5))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; 图形界面下不显示工具栏（`tool-bar-lines' 在首帧创建前即生效；不依赖
;; `display-graphic-p'，因部分 macOS 构建下 early-init 阶段该谓词仍为 nil）。
(add-to-list 'initial-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))

;; On macOS, opening files/directories from the app should reuse the initial
;; frame instead of popping an extra one during startup.
(when (eq system-type 'darwin)
  (setq ns-pop-up-frames nil))

;; Disable unneeded UI early (can be re-enabled later if desired).
(when (display-graphic-p)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode 0))

;; 若 early-init 阶段未关掉工具栏/菜单栏（`display-graphic-p' 为 nil），在首帧就绪后再关一次。
(add-hook 'window-setup-hook
          (lambda ()
            (when (display-graphic-p)
              (menu-bar-mode -1)
              (tool-bar-mode -1)
              (scroll-bar-mode -1)
              (blink-cursor-mode 0))))

;; Silence native-comp warnings during early phase (optional)
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors nil))

(provide 'early-init)
;;; early-init.el ends here
