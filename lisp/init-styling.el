;;; init-styling.el --- Compatibility entry for visual modules -*- lexical-binding: t -*-

;;; Commentary:
;; Visual configuration is split under `lisp/visual/' (`init-visual.el`).
;; This file remains so `init.el' can keep `(require 'init-styling)'.

;;; Code:

(require 'init-visual)

(provide 'init-styling)

;;; init-styling.el ends here
