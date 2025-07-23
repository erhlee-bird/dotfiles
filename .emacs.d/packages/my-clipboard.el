;;; my-clipboard.el --- Initialize the clipboard.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(use-package select
  :custom
  (select-enable-primary t)
  (select-enable-clipboard t)
  :hook
  ;; Replace selection rather than inserting.
  (after-init . delete-selection-mode))

(use-package xclip
  :hook
  (after-init . xclip-mode))

(provide 'my-clipboard)
;;; my-clipboard.el ends here
