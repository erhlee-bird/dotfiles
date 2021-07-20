;; init-lsp.el --- Customize lsp-mode.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(use-package lsp-mode
  :ensure t
  :after (projectile)
  :defines lsp-message-project-root-warning
  :config
  (setq-default lsp-pyls-configuration-sources ["flake8"])
  (setq lsp-enable-snippet nil)
  (setq lsp-message-project-root-warning t)
  (setq lsp-prefer-flymake nil)
  (require 'lsp-clients)
  (add-hook 'python-mode-hook 'lsp)
  )

(use-package lsp-ui
  :ensure t
  :after (lsp-mode flycheck)
  :init
  ; lsp-ui lags horribly with larger projects.
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )
(use-package pyvenv :ensure t)

(use-package lsp-go
  :after (lsp-mode)
  :commands lsp-go-enable
  :init
  (add-hook 'go-mode-hook #'lsp-go-enable))
(use-package lsp-javascript-typescript
  :after (lsp-mode)
  :commands lsp-javascript-typescript-enable
  :init
  (add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'js3-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'rjsx-mode-hook #'lsp-javascript-typescript-enable))
(use-package lsp-haskell
  :after lsp-mode
  :commands lsp-haskell-enable
  :init
  (add-hook 'haskell-mode-hook 'lsp-haskell-enable))
(use-package ccls
  :hook ((c-mode c++-mode) .
         (lambda () (require 'ccls) (lsp))))
(use-package my-keybindings
  :after (lsp-mode)
  :commands make-map
  :defines space-ide-keymap
  :config
  (make-map space-ide-keymap
            '(("d" #'lsp-find-definition)
              ("l" #'lsp-mode)
              ("r" #'lsp-find-references)
              ("R" #'lsp-restart-workspace)
              ("u" #'lsp-ui-mode)
              ("v" #'pyvenv-activate)
              ("x" #'xref-pop-marker-stack))))

(provide 'init-lsp)
;;; init-lsp.el ends here
