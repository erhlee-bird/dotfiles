;;; init-ivy.el --- Customize ivy mode.
;;; Commentary:

;;; Code:

(use-package counsel
  :config
  (setq counsel-rg-base-command (mapconcat 'identity
                                           '("rg"
                                             "--color=never"
                                             "--hidden"
                                             "--line-number"
                                             "--max-columns 240"
                                             "--no-heading"
                                             "--with-filename"
                                             "%s"
                                             "|| true")
                                           " "))
  :hook (after-init . projectile-mode))

(use-package ivy
  :bind
  (:map ivy-minibuffer-map
	    ("ESC" . minibuffer-keyboard-quit))
  :custom
  (ivy-count-format "(%d/%d)")
  (ivy-use-virtual-buffers t)    ; Add recent files and bookmarks.
  (ivy-wrap t)
  :hook (after-init . ivy-mode))

(use-package magit
  :after ivy
  :custom
  (magit-completing-read-function 'ivy-completing-read))

(use-package projectile
  :after ivy
  :commands projectile-save-known-projects
  :init
  (setq projectile-enable-caching t)
  ;; Ripgrep will use .gitignore to ignore files.
  (setq projectile-git-command "rg -0 --files --hidden --no-require-git --color=never")
  (setq projectile-generic-command "rg -0 --files --hidden --no-require-git --color=never")
  (setq projectile-indexing-method 'alien)
  :config
  (projectile-mode +1)
  (defun my-projectile-add-known-project ()
    "Hook the projectile add project function to save to file."
    (interactive)
    (call-interactively 'projectile-add-known-project)
    (projectile-save-known-projects))

  (defun my-projectile-recache ()
    "Invalidate the project cache and reinitialize it."
    (interactive)
    (execute-kbd-macro
     (kbd "C-u M-x counsel-projectile RET C-g")))

  (defun my-count-slashes (path)
    "Count the number of slashes in PATH."
    (seq-count (lambda (x) (char-equal x ?/)) path))

  (defun my-github-projectile-index ()
    "Index all of the downloaded GitHub projects into Projectile."
    (interactive)
    (message "Start Indexing downloaded GitHub projects")
    (let* ((default-directory (expand-file-name "~/.local/src/github.com/"))
           (min-depth (my-count-slashes default-directory)))
      (->> (directory-files-recursively default-directory
                                        ".*"
                                        t
                                        (lambda (x)
                                          (when (<= (my-count-slashes x)
                                                    (+ min-depth 1))
                                            t))
                                        nil)
           (seq-filter
            (lambda (x)
              (and (file-directory-p x)
                   (= (my-count-slashes x)
                      (+ min-depth 1)))))
           (mapc 'projectile-add-known-project)))
    (projectile-save-known-projects)
    (message "Finished Indexing GitHub projects."))

  (setq projectile-completion-system 'ivy))

(use-package my-keybindings
  :after (ivy projectile)
  :commands make-map
  :defines space-keymap space-projectile-keymap
  :config
  (make-map space-keymap
            '(("/" 'counsel-rg)
              ;; See https://github.com/ericdanan/counsel-projectile/issues/179
              ;; ("SPC" 'counsel-projectile)
              ("SPC" 'projectile-find-file)))
  (make-map space-projectile-keymap
            '(("a" 'my-projectile-add-known-project)
              ("c" 'projectile-cleanup-known-projects)
              ("G" 'my-github-projectile-index)
              ("k" 'projectile-kill-buffers)
              ("p" 'projectile-switch-project)
              ("r" 'projectile-remove-known-project)
              ("t" 'projectile-find-test-file)
              ("!" 'my-projectile-recache))))

(provide 'init-ivy)
;;; init-ivy.el ends here
