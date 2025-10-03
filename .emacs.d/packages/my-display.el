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
(global-display-line-numbers-mode 1)
(column-number-mode 1)

(define-advice split-window (:after (&rest _) split-and-switch)
  "Move the point to the newly created window after splitting."
  (other-window 1))
                                        ; Prefer splitting vertically.
(setq split-height-threshold 1)
(setq split-width-threshold 100)

                                        ; Open things only in the current window.
(use-package current-window-only
  :commands (current-window-only--delete-other-windows)
  :init
  (current-window-only-mode t)
  ;; Without this, delete-other-windows doesn't work.
  (let ((override #'current-window-only--delete-other-windows))
    (advice-remove 'delete-other-windows override)))

(provide 'my-display)
;;; my-display.el ends here
