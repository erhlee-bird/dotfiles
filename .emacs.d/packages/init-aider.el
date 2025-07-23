;;; init-aider.el --- Configuration for Emacs integrations for aider.
;;; Commentary:

;;; Code:

(use-package aidermacs
  :bind
  (:map space-keymap
        ("!" . aidermacs-transient-menu))
  :defines space-keymap)

(provide 'init-aider)
;;; init-aider.el ends here
