;;; my-start-server.el --- Starts Emacs as a server on initialization as needed.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;; Starts the server on first launch.

;;; Code:

(use-package server
  :commands server-start
  :custom
  (server-socket-dir "/tmp/emacs")
  :hook (after-init . my-start-server)
  :preface
  (defun my-start-server ()
    (interactive)
    (unless (and (fboundp 'server-running-p)
                 (server-running-p))
      (server-start))))

(provide 'my-start-server)
;;; my-start-server.el ends here
