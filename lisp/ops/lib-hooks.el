;;; lib-hooks.el --- Staged startup hooks -*- lexical-binding: t -*-

;;; Commentary:
;; `henri-first-input-hook`, `henri-first-buffer-hook`, and
;; `henri-first-file-hook' run once, before first user input / first buffer
;; switch / first `find-file', similar to Doom's doom-first-*-hook.

;;; Code:

(defcustom henri-first-input-hook nil
  "Normal hook run once before the first interactive command."
  :type 'hook :group 'henri-core)

(defcustom henri-first-buffer-hook nil
  "Normal hook run once on the first `window-buffer-change-functions' event."
  :type 'hook :group 'henri-core)

(defcustom henri-first-file-hook nil
  "Normal hook run once on the first `find-file-hook' driven open."
  :type 'hook :group 'henri-core)

(defun henri-add-transient-hook! (target-hook fn)
  "Add FN to TARGET-HOOK; FN runs once via a wrapper removed after first fire."
  (let (runner)
    (setq runner
          (lambda (&rest _)
            (remove-hook target-hook runner)
            (funcall fn)))
    (add-hook target-hook runner)))

(defun henri--run-first-input-hooks ()
  (run-hooks 'henri-first-input-hook)
  (remove-hook 'pre-command-hook #'henri--run-first-input-hooks))

(defun henri--run-first-buffer-hooks ()
  (run-hooks 'henri-first-buffer-hook)
  (remove-hook 'window-buffer-change-functions #'henri--run-first-buffer-hooks))

(defun henri--run-first-file-hooks ()
  (run-hooks 'henri-first-file-hook)
  (remove-hook 'find-file-hook #'henri--run-first-file-hooks))

(add-hook 'pre-command-hook #'henri--run-first-input-hooks)
(add-hook 'window-buffer-change-functions #'henri--run-first-buffer-hooks)
(add-hook 'find-file-hook #'henri--run-first-file-hooks)

(provide 'lib-hooks)
;;; lib-hooks.el ends here
