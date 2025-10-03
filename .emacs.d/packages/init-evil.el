;;; init-evil.el --- Customize evil.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(use-package undo-tree
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :diminish ""
  :hook (after-init . global-undo-tree-mode))

(use-package evil
  :after undo-tree
  :bind
  (:map evil-normal-state-map
                                        ; Move down visual lines.
	    ("j" . evil-next-visual-line)
	    ("k" . evil-previous-visual-line)
                                        ; Swap lines up and down.
	    ("C-k" . (lambda () (interactive) (my-swap-line "up")))
	    ("C-j" . (lambda () (interactive) (my-swap-line "down")))
                                        ; Buffer movement.
	    ("<right>" . next-buffer)
	    ("<left>" . previous-buffer)
	    :map space-keymap
	    ("h" . evil-window-left)
	    ("j" . evil-window-down)
	    ("k" . evil-window-up)
	    ("l" . evil-window-right)
	    ("v" . evil-window-vsplit)
	    ("V" . evil-window-split))
  :commands evil-next-line evil-previous-line evil-set-undo-system
  :defines space-keymap vspace-keymap
  :init
  (evil-mode t)
  (evil-set-undo-system 'undo-tree)
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
  :config
                                        ; Enable code folding.
                                        ; (hs-minor-mode t)
  (define-key evil-normal-state-map (kbd "SPC") space-keymap)
  (define-key evil-visual-state-map (kbd "SPC") vspace-keymap)
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
  (delete 'term-mode evil-insert-state-modes)
  :custom
  (evil-default-cursor t)
  (evil-shift-round nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t))

(provide 'init-evil)
;;; init-evil.el ends here
