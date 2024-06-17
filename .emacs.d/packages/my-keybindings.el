;;; my-keybindings.el --- Maps for defining key binds.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(defun add-key-tuple (tuple)
  "Unpack TUPLE and call add-key on it."
  (add-key (nth 0 tuple) (nth 1 tuple) (nth 2 tuple)))

(defun add-key (map key function)
  "Given a MAP, bind a KEY to a given FUNCTION."
  (define-key (eval map) (kbd key) (eval function)))

(defun make-map (kmap list)
  "Given a KMAP, bind the keys and functions in the LIST."
  (mapc 'add-key-tuple
        (mapcar (lambda (l) (cons 'kmap l)) list)))

(defmacro defkeymap (mapname)
  "Define a new keymap with the provided MAPNAME."
  `(defvar ,mapname (make-sparse-keymap)))

;; Space Keymap
(defkeymap space-display-keymap)
(defkeymap space-keymap)
(defkeymap space-ide-keymap)
(defkeymap space-init-keymap)
(defkeymap space-magit-keymap)
(defkeymap space-org-keymap)
(defkeymap space-org-agenda-keymap)
(defkeymap space-org-table-keymap)
(defkeymap space-org-time-keymap)
(defkeymap space-projectile-keymap)
(defkeymap space-rtags-keymap)
(defkeymap vspace-keymap)

(defun my-edit-file ()
  "Edit init file."
  (interactive)
  (find-file user-init-file))

(defun my-key-file ()
  "Edit key file."
  (interactive)
  (find-file "~/.emacs.d/packages/my-keybindings.el"))

(defun my-occur-region (start end)
  "Call occur on a highlighted region from START to END."
  (interactive "r")
  (occur (buffer-substring-no-properties start end)))

(defun my-update-packages ()
  "Update existing packages."
  (interactive)
  (package-list-packages)
  (package-menu-mark-upgrades)
  (package-menu-execute t)
  (kill-buffer "*Packages*")
  (kill-buffer "*Compile-Log*"))

(defun my-github-checkout (repo-pair)
  "Checkout a REPO-PAIR specified by a 'username/reponame' string, to local source tree."
  (interactive)
  (message (format "Checking out GitHub repo: %s" repo-pair))
  (let* ((repo-split (split-string repo-pair "/"))
         (user (car repo-split))
         (repo (cadr repo-split))
         (default-directory (expand-file-name "~/.local/src/github.com/")))
    ;; Create the user directory.
    (shell-command (format "mkdir -p %s" user))
    (let ((default-directory (format "%s/%s" default-directory user)))
      (shell-command (format "[ ! -d \"%s\" ] && git clone https://github.com/%s || :" repo repo-pair))
      (shell-command (format "[ -d \"%s\" ] && (cd %s && git pull) || :" repo repo)))))

(defun my-github-checkout-at-point ()
  "Check out the GitHub repo at point."
  (interactive)
  (my-github-checkout (thing-at-point 'filename)))

(defun my-eval-buffer ()
  "Evaluate the entire buffer."
  (interactive)
  (if (and (bound-and-true-p cider-mode)
           (fboundp 'cider-eval-buffer))
      (cider-eval-buffer)
    (eval-buffer)))

(defun my-eval-last-sexp ()
  "Evalute the last sexp."
  (interactive)
  (if (and (bound-and-true-p cider-mode)
           (fboundp 'cider-eval-sexp-up-to-point))
      (cider-eval-sexp-up-to-point nil)
    (eval-last-sexp nil)))

;; Define space-init-keymap
(make-map space-init-keymap
          '(("i" 'my-edit-file)
            ("k" 'my-key-file)
            ("l" 'package-list-packages)
            ("u" 'my-update-packages)))

;; Define space-keymap
(make-map space-keymap
          '(("b" 'switch-to-buffer)
            ("c" space-ide-keymap)
            ("d" space-display-keymap)
            ;; NB: Contextualize the eval based on the mode we're in.
            ("e" 'my-eval-last-sexp)
            ("E" 'my-eval-buffer)
            ("f" 'find-file)
            ("F" 'dired)
            ("G" 'my-github-checkout-at-point)
            ("i" space-init-keymap)
            ("K" 'kill-this-buffer)
            ("m" space-magit-keymap)
            ("o" space-org-keymap)
            ("O" 'occur)
            ("p" space-projectile-keymap)
            ("Q" 'kill-emacs)
            ("w" 'save-buffer)
            ("x" 'delete-window)
            ("X" 'delete-other-windows)
            (";" 'comment-line)
            ("TAB" 'indent-relative)))

;; Define vspace-keymap
(make-map vspace-keymap
          '(("a" 'align-regexp)
            ("o" 'my-occur-region)
            ("r" 'indent-rigidly)
            (";" 'comment-or-uncomment-region)))

(provide 'my-keybindings)
;;; my-keybindings.el ends here
