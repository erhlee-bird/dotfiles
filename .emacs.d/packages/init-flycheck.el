;;; init-flycheck.el --- Customize flycheck.
;;; Commentary:

;;; Code:

(use-package flycheck
  :bind
  (:map space-ide-keymap ("c" . flycheck-next-error))
  :defer 2
  :defines space-ide-keymap
  :init
  (global-flycheck-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
