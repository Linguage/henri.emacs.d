;;; profiles.el --- Optional profile-<name>.el -*- lexical-binding: t -*-

;;; Code:

(defun henri/load-profile ()
  "Load \"profile-<henri-active-profile>.el\" from `user-emacs-directory' if present."
  (interactive)
  (when (and (boundp 'henri-active-profile) (stringp henri-active-profile))
    (let ((file (locate-user-emacs-file
                 (format "profile-%s.el" henri-active-profile))))
      (when (file-readable-p file)
        (load file nil 'nomessage)
        (message "[henri] profile loaded: %s" henri-active-profile)))))

(provide 'profiles)
;;; profiles.el ends here
