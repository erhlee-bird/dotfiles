;;; my-display.el --- Emacs display settings.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(setq inhibit-startup-screen 1)
(setq initial-scratch-message nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(and (boundp 'scroll-bar-mode)
     (scroll-bar-mode -1))
(add-hook 'after-make-frame-function
          #'(lambda (frame)
              (modify-frame-parameters frame
                                       '((vertical-scroll-bars . nil)
                                         (horizontal-scroll-bars . nil)))))
(show-paren-mode 1)

; Set frame background color.
(setq frame-background-mode 'dark)

; Set Line Numbers
(use-package linum
  :defines linum-format-fmt
  :preface
  (defun linum-format-func (line)
    "Help linum display numbers on line LINE."
    (concat
     (propertize (format linum-format-fmt line) 'face 'linum)
     (propertize " " 'face 'mode-line)))
  :config
  (unless (display-graphic-p)
    (add-hook
     'linum-before-numbering-hook
     (lambda ()
       (setq-local linum-format-fmt
                   (let ((w (length (number-to-string
                                     (count-lines (point-min) (point-max))))))
                     (concat " %" (number-to-string w) "d")))))
    (setq linum-format 'linum-format-func)))

(line-number-mode 1)
(column-number-mode 1)

;; Switch to new split buffers
(defadvice split-window (after move-point-to-new-window activate)
  "Move the point to the newly created window after splitting."
  (other-window 1))

;; Prefer splitting vertically.
(setq split-height-threshold 1)
(setq split-width-threshold 100)

(provide 'my-display)
;;; my-display.el ends here
