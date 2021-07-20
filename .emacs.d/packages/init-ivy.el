;;; init-ivy.el --- Customize ivy mode.
;;; Commentary:

;;; Code:

(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d)")
  (setq ivy-wrap t)
  (setq counsel-git-cmd "rg --files")
  :config
  (ivy-mode 1))
(use-package magit
  :after ivy
  :config
  (setq magit-completing-read-function 'ivy-completing-read))
(use-package projectile
  :after ivy
  :commands projectile-save-known-projects
  :config
  (projectile-mode +1)
  (defun my-projectile-add-known-project ()
    "Hook the projectile add project function to save to file."
    (interactive)
    (call-interactively 'projectile-add-known-project)
    (projectile-save-known-projects))

  (setq projectile-completion-system 'ivy))
(use-package my-keybindings
  :after (ivy projectile)
  :commands make-map
  :defines space-keymap space-projectile-keymap
  :config
  (make-map space-keymap
            '(("/" 'counsel-ag)
              ("SPC" 'counsel-git)))
  (make-map space-projectile-keymap
            '(("a" 'my-projectile-add-known-project)
              ("c" 'projectile-cleanup-known-projects)
              ("k" 'projectile-kill-buffers)
              ("p" 'counsel-projectile-switch-project)
              ("r" 'projectile-remove-known-project))))

(provide 'init-ivy)
;;; init-ivy ends here
