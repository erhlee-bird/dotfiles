;;; my-format.el --- Formatting settings
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

; Indentation
(setq-default buffer-file-coding-system 'utf-8-unix
              indent-tabs-mode nil
              c-default-style "k&r"
              c-basic-offset 4
              css-indent-offset 2
              js-indent-level 2
              sh-basic-offset 2
              typescript-indent-level 2
              tab-width 4)
(define-key global-map (kbd "RET") 'newline-and-indent)
(electric-pair-mode -1)

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-enabled nil)
  ; (set-face-foreground 'highlight-indent-guides-character-face "lightgray")
  )

; Whitespace
(use-package whitespace
  :init
  (global-whitespace-mode t)
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs trailing))
  ; (setq whitespace-style '(face tabs lines-tail trailing))
  (defun my-delete-trailing-whitespace-hook
    ()
    (delete-trailing-whitespace))
  (add-hook 'before-save-hook 'my-delete-trailing-whitespace-hook))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.heex\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'web-mode-buffer-indent)))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package my-keybindings
  :after whitespace
  :commands make-map
  :defines space-display-keymap vspace-keymap
  :config
  ;; Allow for toggleable maximizing a single buffer.
  (defun toggle-maximize-buffer ()
    "Maximize current buffer."
    (interactive)
    (if (= 1 (length (window-list)))
        (jump-to-register '_)
      (progn
        (window-configuration-to-register '_)
        (delete-other-windows))))
  (defun hexify ()
    "Convert a number at point to hexadecimal."
    (interactive)
    (setq-local hx (format "0x%x" (string-to-number (word-at-point))))
    (kill-word 1)
    (insert hx))
  ;; Enable `hs-minor-mode` in all programming modes.
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (make-map space-display-keymap
            '(("f" 'toggle-maximize-buffer)
              ("h" 'highlight-symbol-at-point)
              ("H" 'unhighlight-regexp)
              ("i" 'highlight-indent-guides-mode)
              ("l" 'linum-mode)
              ("m" 'mark-whole-buffer)
              ("x" 'hexify)
              ("z" 'hs-minor-mode)))
  (make-map vspace-keymap
            '(("a" 'align)
              ("h" 'highlight-region)
              ("i" 'indent-region)
              ("t" 'tabify)
              ("T" 'untabify))))

(provide 'my-format)
;;; my-format.el ends here
