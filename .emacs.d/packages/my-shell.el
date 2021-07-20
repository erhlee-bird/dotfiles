;;; my-shell.el -- Shell configuration settings.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  "Kill the terminal buffer after it's done rather than leaving it."
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defun spawn-new-shell ()
  "Spawn separate 'term' instances."
  (interactive)
  (ansi-term "bash"))

(defun spawn-shell-in (dir)
  "Open a new shell in DIR."
  (interactive)
  (let ((default-directory dir))
    (spawn-new-shell)))

(defun spawn-shell-in-git-dir ()
  "Spawn a shell in the git directory."
  (interactive)
  (let ((dir (locate-dominating-file default-directory ".git")))
    (when dir
      (spawn-shell-in (file-name-directory dir)))))

(use-package my-keybindings
  :defines space-keymap
  :config
  (define-key space-keymap (kbd "t") 'spawn-shell-in-git-dir)
  (define-key space-keymap (kbd "T") 'spawn-new-shell))

(provide 'my-shell)
;;; my-shell.el ends here
