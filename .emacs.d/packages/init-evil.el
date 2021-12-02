;;; init-evil.el --- Customize evil.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

(use-package evil
  :ensure t
  :after undo-tree
  :commands evil-next-line evil-previous-line evil-delay
  :preface
  ; Utility line swap function.
  (defun my-swap-line (direction)
    "Swap the current line in the DIRECTION specified (up/down)."
    (interactive)
    (when (string= direction "down")
      (evil-next-line 1))
    (transpose-lines 1)
    (if (string= direction "down")
        (evil-previous-line 1)
      (evil-previous-line 2)))
  :init
  (evil-mode t)
  (evil-set-undo-system 'undo-tree)
  (setq evil-default-cursor t)
  :config
  ; Enable code folding.
  ; (hs-minor-mode t)
  ; Move down visual lines.
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  ; Swap lines up and down.
  (define-key evil-normal-state-map (kbd "C-k")
    (lambda () (interactive) (my-swap-line "up")))
  (define-key evil-normal-state-map (kbd "C-j")
    (lambda () (interactive) (my-swap-line "down")))
  ; Buffer movement.
  (define-key evil-normal-state-map (kbd "<right>") 'next-buffer)
  (define-key evil-normal-state-map (kbd "<left>") 'previous-buffer)
  ; ESC should always quit: http://stackoverflow.com/10166400/61435
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)
  ; Add hjkl bindings to Emacs.
  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "gg") 'evil-goto-first-line
    (kbd "G") 'evil-goto-line
    (kbd "/") 'evil-search-forward
    (kbd "?") 'evil-search-backward
    (kbd "n") 'evil-search-next
    (kbd "N") 'evil-search-previous)
  (evil-add-hjkl-bindings package-menu-mode-map 'emacs
    (kbd "gg") 'evil-goto-first-line
    (kbd "G") 'evil-goto-line
    (kbd "/") 'evil-search-forward
    (kbd "?") 'evil-search-backward
    (kbd "n") 'evil-search-next
    (kbd "N") 'evil-search-previous)
  ; Modes that should use Emacs state.
  (dolist (mode '(cider-stacktrace-mode
                  cider-test-report-mode
                  dired-mode
                  ;; XXX: this is too common
                  ;; fundamental-mode
                  git-rebase-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  ; Fix for evil overwriting Ctrl+D in term mode.
  (delete 'term-mode evil-insert-state-modes))

(use-package ivy
  :after evil
  :config
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit))

(use-package my-keybindings
  :after evil
  :defines space-keymap vspace-keymap
  :config
  (define-key evil-normal-state-map (kbd "SPC") space-keymap)
  (define-key evil-visual-state-map (kbd "SPC") vspace-keymap)
  (define-key space-keymap (kbd "h") 'evil-window-left)
  (define-key space-keymap (kbd "j") 'evil-window-down)
  (define-key space-keymap (kbd "k") 'evil-window-up)
  (define-key space-keymap (kbd "l") 'evil-window-right)
  (define-key space-keymap (kbd "v") 'evil-window-vsplit)
  (define-key space-keymap (kbd "V") 'evil-window-split))

(provide 'init-evil)
;;; init-evil.el ends here
