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
(use-package company-lsp
  :ensure t
  :after (lsp-mode company)
  :init
  (add-to-list 'company-backends 'company-lsp)
  :config
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

(provide 'init-company)
;;; init-company.el ends here
