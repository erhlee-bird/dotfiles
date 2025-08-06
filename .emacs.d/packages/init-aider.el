;;; init-aider.el --- Configuration for Emacs integrations for aider.
;;; Commentary:

;;; Code:

(use-package aidermacs
  :bind
  (:map space-keymap
        ("!" . aidermacs-transient-menu)
        ("@" . aidermacs-switch-to-buffer)
        ("#" . aidermacs-open-prompt-file)
        :map vspace-keymap
        ("!" . aidermacs-transient-menu))
  :custom
  (aidermacs-backend 'vterm)
  (aidermacs-exit-kills-buffer t)
  (aidermacs-show-diff-after-change t)
  (aidermacs-watch-files t)
  :defines space-keymap vspace-keymap)

(provide 'init-aider)
;;; init-aider.el ends here
