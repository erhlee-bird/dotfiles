;;; init-magit.el --- Configures magit.
;;; Commentary:

;;; Code:

(use-package magit
  :bind
  (:map space-magit-keymap
        ("f" . magit-file-popup)
        ("m" . magit-status)
        ("n" . smerge-next)
        ("p" . smerge-prev)
        ("<" . smerge-keep-upper)
        (">" . smerge-keep-lower)
        :map transient-map
        ("<escape>" . transient-quit-one))
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-log-arguments '("-n10" "--color" "--graph" "--decorate" "--patch"))
  :defines space-magit-keymap)

(provide 'init-magit)
;;; init-magit.el ends here
