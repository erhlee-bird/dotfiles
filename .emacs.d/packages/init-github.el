;;; init-github.el --- Customize GitHub integrations.
;;; Commentary:

;;; Code:

(use-package codespaces
  :ensure t
  :config
  (codespaces-setup)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; Assumes that you've setup ControlMaster in ~/.ssh/config.
  (setq tramp-ssh-controlmaster-options "")
  (setq vc-handled-backends '(Git)))

(use-package my-keybindings
  :after codespaces
  :commands make-map
  :defines space-ide-keymap
  :config
  (make-map space-ide-keymap
            '(("C" #'codespaces-connect))))

(provide 'init-github)
;;; init-github.el ends here
