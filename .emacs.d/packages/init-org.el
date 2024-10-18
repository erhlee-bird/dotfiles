;;; init-org.el --- Customize org mode.
;;; Commentary:

;;; Code:

(defvar-local main-org-file "refile.org")

(use-package org
  :ensure t
  :defines org-capture-templates
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
      (call-interactively #'org-open-at-point)))
  :init
  (setq org-directory (expand-file-name "~/.emacs.d/org"))
  ;; Make default org directory.
  (unless (file-exists-p org-directory)
    (make-directory org-directory))
  ;; Track the main org file.
  (setq org-default-notes-file (org-dir-join main-org-file))
  ;; Hide leading stars.
  (setq org-hide-leading-stars t)
  ;; Have org-mode indent to the heading level.
  (setq org-startup-indented t)
  ;; Org follows links on ret.
  (setq org-return-follows-link t)
  ;; Set the TODO sequence.
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(d)" "|" "DONE(x)")))
  ;; Refile targets
  (setq org-refile-targets '((org-agenda-files :maxlevel . 4)))
  ;; Capture templates.
  (setq org-capture-templates
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
  (setq org-duration-format '(("h" . t) (special . 2)))
  (add-hook 'org-mode-hook 'auto-fill-mode)
  ;; Default to xdg-open for opening files externally.
  (setq org-file-apps '((remote . emacs)
                        (auto-mode . emacs)
                        (directory . emacs)
                        (system . "setsid -w xdg-open %s")
                        (t . system)))
  ;; Load babel functionality.
  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t)))
  :config
  (unless (not (null org-agenda-files))
    (setq org-agenda-files (cons org-default-notes-file nil))))

(use-package ox-latex
  :config
  (setq org-latex-pdf-process
        '("xelatex -interaction nonstopmode %f"
          "xelatex -interaction nonstopmode %f"))
  (setq org-latex-with-hyperref nil))

(use-package my-keybindings
  :after (evil org)
  :commands (evil-define-key make-map)
  :defines (space-org-keymap
            space-org-agenda-keymap
            space-org-table-keymap
            space-org-time-keymap)
  :config
  ;; Define space-org-keymap
  (make-map space-org-keymap
            '(("a" space-org-agenda-keymap)
              ("A" 'org-insert-heading-after-current)
              ("b" (lambda ()
                     (interactive)
                     (org-insert-structure-template "src")))
              ("c" 'org-capture)
              ("e" 'org-export-dispatch)
              ("f" 'org-switchb)
              ("i" 'org-insert-heading)
              ("I" 'org-insert-heading-respect-content)
              ("l" 'org-latex-export-to-pdf)
              ("L" 'org-toggle-link-display)
              ("o" 'org-open-at-point)
              ("O" 'my-org-open-at-point)
              ("p" 'org-insert-link)
              ("r" 'org-refile)
              ("s" 'org-store-link)
              ("t" space-org-time-keymap)
              ("T" space-org-table-keymap)
              ("v" 'org-cycle)
              ("V" 'org-global-cycle)
              ("&" 'org-mark-ring-goto)
              ("/" 'org-match-sparse-tree)
              ("DEL" 'org-mark-ring-goto)
              ))

  ;; Define space-org-agenda-keymap
  (make-map space-org-agenda-keymap
            '(("A" 'org-agenda)
              ("e" 'org-edit-agenda-file-list)
              ("f" 'org-cycle-agenda-files)
              ("j" 'journal-to-today)
              ("n" 'org-agenda-file-to-front)
              ("N" 'org-remove-file)
              ("t" 'org-todo)
              ))

  ;; Define space-org-table-keymap
  (make-map space-org-table-keymap
            '(("a" 'org-table-align)
              ("c" 'org-table-blank-field)
              ("n" 'org-table-next-field)
              ("p" 'org-table-previous-field)
              ("N" 'org-table-next-row)
              ("0" 'org-table-beginning-of-field)
              ("$" 'org-table-end-of-field)
              ))

  ;; Define space-org-time-keymap
  (make-map space-org-time-keymap
            '(("c" 'org-clock-in)
              ("C" 'org-clock-out)
              ("d" 'org-clock-display)
              ("r" 'org-resolve-clock)
              ("s" 'org-time-stamp)
              ("S" 'my-current-time-stamp)
              ("u" 'org-clock-update-time-maybe)))

  (evil-define-key 'normal org-mode-map
    (kbd "a") space-org-agenda-keymap
    (kbd "t") space-org-time-keymap
    (kbd "RET") 'org-return-indent
    (kbd "TAB") 'org-cycle
    (kbd "M-TAB") 'org-global-cycle
    (kbd ",") space-org-keymap
    (kbd ">") 'org-metaright
    (kbd "<") 'org-metaleft))

(provide 'init-org)
;;; init-org.el ends here
