;;; init-ivy.el --- Customize ivy mode.
;;; Commentary:

;;; Code:

(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)    ; Add recent files and bookmarks.
  (setq ivy-count-format "(%d/%d)")
  (setq ivy-wrap t)
  :config
  (ivy-mode 1))

(use-package magit
  :after ivy
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package projectile
  :after ivy
  :commands projectile-save-known-projects
  :init
  (setq projectile-enable-caching t)
  (setq projectile-git-command "rg -0 --files --color=never")
  (setq projectile-generic-command "rg -0 --files --color=never")
  (setq projectile-indexing-method 'hybrid)

  ; Ripgrep will use .gitignore to ignore files.
  ; (add-to-list 'projectile-globally-ignored-directories "*.elixir_ls")
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
            '(("/" 'counsel-rg)
              ("SPC" 'counsel-projectile)))
  (make-map space-projectile-keymap
            '(("a" 'my-projectile-add-known-project)
              ("c" 'projectile-cleanup-known-projects)
              ("k" 'projectile-kill-buffers)
              ("p" 'counsel-projectile-switch-project)
              ("r" 'projectile-remove-known-project))))

(provide 'init-ivy)
;;; init-ivy ends here