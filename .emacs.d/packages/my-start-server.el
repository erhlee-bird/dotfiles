;;; my-start-server.el --- Starts Emacs as a server on initialization as needed.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;; Starts the server on first launch.

;;; Code:

(use-package server
  :ensure t
  :commands server-start
  :preface
  (defun my-start-server ()
    (interactive)
    (unless (and (fboundp 'server-running-p)
                 (server-running-p))
      (server-start)))
  :config
  (setq server-socket-dir "/tmp/emacs")
  (add-hook 'after-init-hook 'my-start-server))

(provide 'my-start-server)
;;; my-start-server.el ends here
