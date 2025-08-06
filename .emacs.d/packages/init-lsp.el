;; init-lsp.el --- Customize lsp-mode.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(use-package lsp-mode
  :after projectile
  :bind
  (:map space-ide-keymap
        ("RET" . #'lsp-execute-code-action)
        ("d" . lsp-find-definition)
        ("f" . lsp-format-buffer)
        ("F" . web-mode-buffer-indent)
        ("l" . lsp-mode)
        ("r" . lsp-find-references)
        ("u" . lsp-ui-mode)
        ("x" . xref-go-back))
  :commands (lsp-execute-code-action
             lsp-format-buffer)
  :custom
  (lsp-completion-enable t)
  (lsp-completion-provier :none) ;; we use Corfu!
  (lsp-enable-file-watchers nil)
  (lsp-enable-indentation t)
  (lsp-enable-snippet nil)
  (lsp-enable-suggest-server-download nil)
  (lsp-lens-enable t)
  (lsp-message-project-root-warning t)
  :defer 2
  :defines (lsp-message-project-root-warning
            space-ide-keymap)
  :hook
  (prog-mode . lsp-deferred)
  :config
  (setq-default lsp-pyls-configuration-sources ["flake8"]))

(use-package lsp-ui
  :after (flycheck lsp-mode)
  :custom
  (lsp-ui-doc-enable t)
  :hook
  (lsp-mode . lsp-ui-mode))

(provide 'init-lsp)
;;; init-lsp.el ends here
