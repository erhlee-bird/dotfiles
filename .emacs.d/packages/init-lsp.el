;; init-lsp.el --- Customize lsp-mode.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(use-package lsp-mode
  :ensure t
  :after projectile
  :commands lsp-format-buffer
  :defines lsp-message-project-root-warning
  :config
  (setq-default lsp-pyls-configuration-sources ["flake8"])
  (setq lsp-completion-enable t)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-enable-indentation t)
  (setq lsp-enable-snippet nil)
  (setq lsp-lens-enable t)
  (setq lsp-message-project-root-warning t))

(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :config
  (setq treemacs-space-between-root-nodes nil))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode flycheck)
  :init
                                        ; lsp-ui lags horribly with larger projects.
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t))

(use-package cider
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook 'lsp)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'lsp-format-buffer)))
  :config
  (setq cider-test-show-report-on-success t)
  (set-variable 'cider-lein-parameters (concat "with-profile +dev repl :headless")))

(use-package elixir-mode
  :ensure t
  :init
  (add-hook 'elixir-mode-hook 'lsp)
  (add-hook 'elixir-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'elixir-format nil :local))))

(use-package pyvenv :ensure t)

(use-package lsp-go
  :after lsp-mode
  :commands lsp-go-enable
  :init
  (add-hook 'go-mode-hook #'lsp-go-enable))

(use-package lsp-haskell
  :after lsp-mode
  :commands lsp-haskell-enable
  :init
  (add-hook 'haskell-mode-hook 'lsp-haskell-enable))

(use-package lsp-tailwindcss
  :after lsp-mode
  :ensure t
  :init
  (add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save))

(use-package my-keybindings
  :commands make-map
  :defines space-ide-keymap
  :config
  (make-map space-ide-keymap
            '(("RET" #'lsp-execute-code-action)
              ("d" #'lsp-find-definition)
              ("f" #'lsp-format-buffer)
              ("F" #'web-mode-buffer-indent)
              ("l" #'lsp-mode)
              ("r" #'lsp-find-references)
              ("R" #'lsp-restart-workspace)
              ("u" #'lsp-ui-mode)
              ("v" #'pyvenv-activate)
              ("x" #'xref-pop-marker-stack))))

(provide 'init-lsp)
;;; init-lsp.el ends here
