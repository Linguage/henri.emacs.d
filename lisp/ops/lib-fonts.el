;;; lib-fonts.el --- Compatibility shim for `visual-fonts' -*- lexical-binding: t -*-

;;; Commentary:
;; Font helpers live in `lisp/visual/visual-fonts.el'.  Keep requiring `lib-fonts'
;; so `doctor' and startup order stay stable.

;;; Code:

(require 'visual-fonts)

(provide 'lib-fonts)

;;; lib-fonts.el ends here
