;;; init-magit.el --- Configures magit.
;;; Commentary:

;;; Code:

(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function
        'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-log-arguments '("-n10" "--color" "--graph" "--decorate" "--patch")))

(use-package my-keybindings
  :after magit
  :commands make-map
  :defines space-magit-keymap
  :config
  (make-map space-magit-keymap
            '(("f" 'magit-file-popup)
              ("m" 'magit-status)
              ("n" 'smerge-next)
              ("p" 'smerge-prev)
              ("<" 'smerge-keep-mine)
              (">" 'smerge-keep-other)
              )))

(provide 'init-magit)
;;; init-magit.el ends here
