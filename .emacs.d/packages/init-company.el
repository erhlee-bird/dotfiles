;;; init-company.el --- Customize company-mode.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(use-package company
  :ensure t
  :diminish ""
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-tooltip-idle-delay 0.2
        company-minimum-prefix-length 3))

(provide 'init-company)
;;; init-company.el ends here
