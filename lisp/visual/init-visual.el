;;; init-visual.el --- Visual layer entry (fonts, themes, components) -*- lexical-binding: t -*-

;;; Commentary:
;; Loads `lisp/visual/` in order: fonts (definitions + Org body font hooks),
;; themes (`doom-themes` + theme strategy hook), then UI components.
;; `init-styling.el` remains a thin compatibility wrapper that requires this file.

;;; Code:

(require 'visual-fonts)
(require 'visual-themes)
(require 'visual-components)

(provide 'init-visual)

;;; init-visual.el ends here
