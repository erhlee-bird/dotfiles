;;; my-backup.el --- Handle backup settings.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(setq auto-save-default nil)
(setq make-backup-files t)
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(global-auto-revert-mode t)  ;; Reload automatically from disk.
(setq vc-follow-symlinks t)

(provide 'my-backup)
;;; my-backup.el ends here
