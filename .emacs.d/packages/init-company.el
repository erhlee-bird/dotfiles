;;; init-company.el --- Customize company-mode.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(use-package company
  :bind
  (:map space-ide-keymap
        ("TAB" . company-complete))
  :custom
  (company-idle-delay 0.2)
  (company-tooltip-idle-delay 0.2)
  (company-minimum-prefix-length 1)
  :defer 2
  :defines space-ide-keymap
  :diminish ""
  :init
  (global-company-mode))

(provide 'init-company)
;;; init-company.el ends here
