;;; init-flycheck.el --- Customize flycheck.
;;; Commentary:

;;; Code:

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package my-keybindings
  :after flycheck
  :commands make-map
  :defines space-ide-keymap
  :config
  (make-map space-ide-keymap
            '(("c" 'flycheck-next-error))))

(use-package cc-mode
  :after flycheck
  :config
  (add-hook 'c++-mode-hook
            (lambda () (setq flycheck-gcc-language-standard "c++11"))))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
