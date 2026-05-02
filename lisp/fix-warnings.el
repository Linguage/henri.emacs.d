;;; fix-warnings.el --- Startup warning noise reduction -*- lexical-binding: t -*-

;;; Commentary:
;; Keep this file minimal: hide only compile-time noise, not runtime warnings.

;;; Code:

(setq byte-compile-warnings '(not obsolete cl-functions))

(with-eval-after-load 'warnings
  (add-to-list 'warning-suppress-types '(comp))
  (add-to-list 'warning-suppress-types '(bytecomp))
  (add-to-list 'warning-suppress-types '(obsolete)))

(setq use-package-verbose nil
      use-package-compute-statistics nil)

(provide 'fix-warnings)
;;; fix-warnings.el ends here
