;;; init-github.el --- Customize GitHub integrations.
;;; Commentary:

;;; Code:

(use-package codespaces
  :bind
  (:map space-ide-keymap
        ("C" . codespaces-connect))
  :commands codespaces-setup
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (codespaces-setup)
  :custom
  ;; Assumes that you've setup ControlMaster in ~/.ssh/config.
  (tramp-ssh-controlmaster-options "")
  (vc-handled-backends '(Git))
  :defer 2
  :defines space-ide-keymap)

(provide 'init-github)
;;; init-github.el ends here
