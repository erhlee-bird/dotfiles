;;; init-projectile.el --- Customize projectile.
;;; Commentary:

;;; Code:

(use-package counsel
  :custom
  (counsel-rg-base-command (mapconcat 'identity
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

;; TODO: Want to add a way to sort files in projectile-find-file by updated
;;       timestamp and most frequently visited.
(use-package projectile
  :bind
  (:map space-keymap
        ("SPC" . projectile-find-file)
        :map space-projectile-keymap
        ("a" . my-projectile-add-known-project)
        ("c" . projectile-cleanup-known-projects)
        ("G" . my-github-projectile-index)
        ("k" . projectile-kill-buffers)
        ("p" . projectile-switch-project)
        ("r" . projectile-remove-known-project)
        ("t" . projectile-find-test-file)
        ("!" . my-projectile-recache))
  :commands (->>
             projectile-save-known-projects)
  :custom
  (projectile-enable-caching t)
  ;; Ripgrep will use .gitignore to ignore files.
  (projectile-git-command (mapconcat 'identity
                                     '("rg"
                                       "-0"
                                       "--color=never"
                                       "--files"
                                       "--glob=!.git/"
                                       "--hidden"
                                       "--no-require-git")
                                     " "))
  (projectile-generic-command projectile-git-command)
  (projectile-indexing-method 'alien)
  :defines (space-keymap
            space-projectile-keymap)
  :hook
  (after-init . projectile-mode)
  :preface
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
    (message "Finished Indexing GitHub projects.")))

(use-package counsel-projectile :defer t)

(provide 'init-projectile)
;;; init-projectile.el ends here
