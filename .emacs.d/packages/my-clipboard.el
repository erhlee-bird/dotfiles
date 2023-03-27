;;; my-clipboard.el --- Initialize the clipboard.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(use-package select
  :ensure t
  :config
  (setq select-enable-primary t)
  (setq select-enable-clipboard t)
  (delete-selection-mode t)) ;; Replace selection rather than inserting.

(provide 'my-clipboard)
;;; my-clipboard.el ends here
