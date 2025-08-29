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

;; Disable unneeded UI early (can be re-enabled later if desired).
(when (display-graphic-p)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode 0))

;; Silence native-comp warnings during early phase (optional)
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors nil))

(provide 'early-init)
;;; early-init.el ends here
