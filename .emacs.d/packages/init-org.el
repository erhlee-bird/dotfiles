;;; init-org.el --- Customize org mode.
;;; Commentary:

;;; Code:

(defvar-local main-org-file "refile.org")

(use-package org
  :after evil
  :bind
  (:map space-org-keymap
        ("A" . org-insert-heading-after-current)
        ("b" . (lambda ()
                 (interactive)
                 (org-insert-structure-template "src")))
        ("c" . org-capture)
        ("e" . org-export-dispatch)
        ("f" . org-switchb)
        ("i" . org-insert-heading)
        ("I" . org-insert-heading-respect-content)
        ("l" . org-latex-export-to-pdf)
        ("L" . org-toggle-link-display)
        ("o" . org-open-at-point)
        ("O" . my-org-open-at-point)
        ("p" . org-insert-link)
        ("r" . org-refile)
        ("s" . org-store-link)
        ("v" . org-cycle)
        ("V" . org-global-cycle)
        ("&" . org-mark-ring-goto)
        ("/" . org-match-sparse-time)
        ("DEL" . org-mark-ring-goto)
        :map space-org-agenda-keymap
        ("A" . org-agenda)
        ("e" . org-edit-agenda-file-list)
        ("f" . org-cycle-agenda-files)
        ("j" . journal-to-today)
        ("n" . org-agenda-file-to-front)
        ("N" . org-remove-file)
        ("t" . org-todo)
        :map space-org-table-keymap
        ("a" . org-table-align)
        ("c" . org-table-blank-field)
        ("n" . org-table-next-field)
        ("p" . org-table-previous-field)
        ("N" . org-table-next-row)
        ("0" . org-table-beginning-of-field)
        ("$" . org-table-end-of-field)
        :map space-org-time-keymap
        ("c" . org-clock-in)
        ("C" . org-clock-out)
        ("d" . org-clock-display)
        ("r" . org-resolve-clock)
        ("s" . org-time-stamp)
        ("S" . my-current-time-stamp)
        ("u" . org-clock-update-time-maybe))
  :commands ( evil-define-key
              org-find-exact-headline-in-buffer
              org-insert-heading-respect-content
              org-insert-structure-template
              org-insert-subheading
              org-open-at-point)
  :config
  ;; Make default org directory.
  (unless (file-exists-p org-directory)
    (make-directory org-directory))
  (unless (not (null org-agenda-files))
    (setq org-agenda-files (cons org-default-notes-file nil)))
  (evil-define-key 'normal org-mode-map
    (kbd "a") space-org-agenda-keymap
    (kbd "t") space-org-time-keymap
    (kbd "RET") 'org-return-indent
    (kbd "TAB") 'org-cycle
    (kbd "M-TAB") 'org-global-cycle
    (kbd ",") space-org-keymap
    (kbd ">") 'org-metaright
    (kbd "<") 'org-metaleft)
  :custom
  (org-directory (expand-file-name "~/.emacs.d/org"))
  ;; Track the main org file.
  (org-default-notes-file (org-dir-join main-org-file))
  ;; Hide leading stars.
  (org-hide-leading-stars t)
  ;; Have org-mode indent to the heading level.
  (org-startup-indented t)
  ;; Org follows links on ret.
  (org-return-follows-link t)
  ;; Set the TODO sequence.
  (org-todo-keywords '((sequence "TODO(t)" "DOING(d)" "|" "DONE(x)")))
  ;; Refile targets
  (org-refile-targets '((org-agenda-files :maxlevel . 4)))
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
  ;; Show time in hours as a float rather than in time.
  (org-duration-format '(("h" . t) (special . 2)))
  ;; Default to xdg-open for opening files externally.
  (org-file-apps '((remote . emacs)
                   (auto-mode . emacs)
                   (directory . emacs)
                   (system . "setsid -w xdg-open %s")
                   (t . system)))
  :defines (org-capture-templates
            space-org-agenda-keymap
            space-org-keymap
            space-org-table-keymap
            space-org-time-keymap)
  :hook
  (org-mode . auto-fill-mode)
  ;; Load babel functionality.
  (org-mode . (lambda ()
                (org-babel-do-load-languages
                 'org-babel-load-languages '((shell . t)))))
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
            (end-of-line)
            (org-insert-subheading nil)
            (insert today-headline)
            (kill-whole-line)
            (goto-char (point-max))
            (newline)
            (yank))
        (progn
          (goto-char (org-find-exact-headline-in-buffer today-headline))
          (end-of-line)))))

  (defun my-org-open-at-point ()
    (interactive)
    (let ((current-prefix-arg '(16))) ;; simulate C-u C-u
      (call-interactively #'org-open-at-point))))

(provide 'init-org)
;;; init-org.el ends here
