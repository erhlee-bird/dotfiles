;;; init-org.el --- Customize org mode.
;;; Commentary:

;;; Code:

(defvar-local main-org-file "refile.org")

;; Track the main org file.
(setq-default org-directory (expand-file-name "~/Documents/org"))
;; Make default org directory.
(unless (file-exists-p org-directory)
  (make-directory org-directory))
(setq-default org-agenda-files
              (append
               (file-expand-wildcards (expand-file-name "*.org" org-directory))
               (file-expand-wildcards (expand-file-name "*.org" "~/.local/share/org/"))))
(use-package org
  :after evil
  :bind
  (:map space-org-keymap
        ("A" . org-insert-heading-after-current)
        ("b" . (lambda ()
                 "Insert an org-babel block."
                 (interactive)
                 (org-insert-structure-template (completing-read "Block: " '("src" "example" "quote")))))
        ("c" . org-capture)
        ("e" . org-export-dispatch)
        ("f" . (lambda ()
                 "Switch to an agenda file."
                 (interactive)
                 ;; Also add any open buffer with a .org suffix to the
                 ;; completing-read options.
                 (let ((choices (append org-agenda-files
                                        (mapcar #'buffer-file-name
                                                (cl-remove-if-not
                                                 (lambda (buf)
                                                   (and (buffer-file-name buf)
                                                        (string-suffix-p ".org" (buffer-file-name buf))))
                                                 (buffer-list))))))
                   (find-file (completing-read "Choose agenda file: " choices)))))
        ("i" . org-insert-heading)
        ("I" . org-insert-heading-respect-content)
        ("j" . org-next-visible-heading)
        ("k" . org-previous-visible-heading)
        ("L" . org-toggle-link-display)
        ("o" . org-open-at-point)
        ("O" . my-org-open-at-point)
        ("p" . (lambda ()
                 "Insert an org link after appending a space."
                 (interactive)
                 (forward-char)
                 (insert-char ?\s)
                 (org-insert-link)))
        ("r" . org-refile)
        ;; Bind tag search and corresponding open all headings keybindings.
        ("s" . org-store-link)
        ("v" . org-cycle)
        ("V" . org-global-cycle)
        ("&" . org-mark-ring-goto)
        ("/" . org-occur)
        ("DEL" . org-mark-ring-goto)
        :map space-org-agenda-keymap
        ("A" . org-agenda)
        ("j" . journal-to-today)
        ("t" . org-todo)
        :map space-org-table-keymap
        ("a" . org-table-align)
        ("c" . org-table-blank-field)
        ("n" . org-table-next-field)
        ("p" . org-table-previous-field)
        ("N" . org-table-next-row)
        ("0" . org-table-beginning-of-field)
        ("$" . org-table-end-of-field)
        :map space-org-tree-keymap
        ("J" . org-move-subtree-up)
        ("K" . org-move-subtree-down)
        ("s" . org-sort-entries)
        ("t" . org-tags-sparse-tree)
        ("T" . (lambda ()
                 (interactive)
                 (org-cycle-set-startup-visibility)))
        ("z" . org-narrow-to-subtree)
        ("Z" . widen)
        :map space-org-time-keymap
        ("c" . org-clock-in)
        ("C" . org-clock-out)
        ("d" . org-clock-display)
        ("r" . org-resolve-clock)
        ("s" . org-time-stamp)
        ("S" . my-current-time-stamp)
        ("u" . org-clock-update-time-maybe))
  :commands ( evil-define-key
              org-current-level
              org-cycle-set-startup-visibility
              org-end-of-subtree
              org-find-exact-headline-in-buffer
              org-insert-heading-respect-content
              org-insert-structure-template
              org-insert-subheading
              org-open-at-point
              org-promote-subtree)
  :config
  (evil-define-key 'normal org-mode-map
    (kbd "RET") 'org-return-indent
    (kbd "TAB") 'org-cycle
    (kbd "M-TAB") 'org-global-cycle
    (kbd ",") space-org-keymap
    (kbd ">") 'org-metaright
    (kbd "<") 'org-metaleft)
  :custom
  (org-babel-min-lines-for-block-output 1)
  ;; Capture templates.
  (org-capture-templates
   `(("t"              ; hotkey
      "Todo entry"     ; name
      entry            ; type
                                        ; heading type and title
      (file+headline org-default-notes-file "Tasks")
                                        ; template
      "* TODO %?Task\nSummary")
     ("j"              ; hotkey
      "Journal Entry"  ; name
      entry            ; type
                                        ; heading type and title
      (file+headline org-default-notes-file "Refile")
                                        ; template
      "* %?")
     ("l"              ; hotkey
      "Link"           ; name
      plain            ; type
                                        ; heading type and title
      (file+headline org-default-notes-file "Refile")
                                        ; template
      "%? file:%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))\n")
     ))
  (org-default-notes-file (org-dir-join main-org-file))
  ;; Show time in hours as a float rather than in time.
  (org-duration-format '(("h" . t) (special . 2)))
  ;; Default to xdg-open for opening files externally.
  (org-file-apps '((remote . emacs)
                   (auto-mode . emacs)
                   (directory . emacs)
                   (system . "setsid -w xdg-open %s")
                   (t . system)))
  ;; Hide leading stars.
  (org-hide-leading-stars t)
  ;; Track when a TODO item was marked as done.
  (org-log-done 'time)
  ;; Log state changes into a LOGBOOK drawer.
  (org-log-into-drawer t)
  ;; Refile targets
  (org-refile-targets '((org-agenda-files :maxlevel . 4)))
  ;; Org follows links on ret.
  (org-return-follows-link t)
  ;; Have org-mode indent to the heading level.
  (org-startup-indented nil)
  ;; Set the TODO sequence.
  (org-todo-keyword-faces
   '(("TODO" . org-warning)
     ("STARTED" . "yellow")
     ("CANCELED" . "grey")))
  (org-todo-keywords
   '((sequence "TODO(t!)" "STARTED(s!)" "|" "CANCELED(c!)" "DONE(x!)")))
  :defines (evil-normal-state-map
            org-capture-templates
            space-org-agenda-keymap
            space-org-keymap
            space-org-table-keymap
            space-org-tree-keymap
            space-org-time-keymap)
  :hook
  (org-mode . auto-fill-mode)
  ;; Load babel functionality.
  (org-mode . (lambda ()
                (org-babel-do-load-languages
                 'org-babel-load-languages '((python . t)
                                             (shell . t)))))
  :preface
  (defun org-dir-join (file-name)
    "Get the org file path for FILE-NAME in the org-directory."
    (concat (file-name-as-directory org-directory) file-name))

  (defun my-current-time-stamp ()
    "Insert the current time stamp."
    (interactive)
    (let ((current-prefix-arg '(16))) ;; simulate C-u C-u
      (call-interactively 'org-time-stamp-inactive)))

  (defun journal-to-today ()
    "Create the base journal structure with two entry levels."
    (interactive)
                                        ; Remove the extra line.
    (setq-local org-blank-before-new-entry '((heading . nil)))
    (let ((top-headline "Journal")
          (today-headline (format-time-string "%Y-%m-%d %A"))
          (org-blank-before-new-entry '((heading . nil))))
      (if (not (org-find-exact-headline-in-buffer top-headline))
          (progn
            (org-insert-heading-respect-content nil)
            (insert top-headline)))
      (goto-char (org-find-exact-headline-in-buffer top-headline))
      (if (not (org-find-exact-headline-in-buffer today-headline))
          (progn
            (org-end-of-subtree)
            (end-of-line)
            (org-insert-subheading nil)
            (insert today-headline)
            (let ((current-level (org-current-level)))
              (while (> current-level 2)
                (org-promote-subtree)
                (setq current-level (org-current-level)))))
        (progn
          (goto-char (org-find-exact-headline-in-buffer today-headline))
          (end-of-line)))))

  (defun my-org-open-at-point ()
    (interactive)
    (let ((current-prefix-arg '(16))) ;; simulate C-u C-u
      (call-interactively #'org-open-at-point))))

;; Enable highlighting code blocks in HTML exports.
(use-package htmlize
  :after org)

(provide 'init-org)
;;; init-org.el ends here
