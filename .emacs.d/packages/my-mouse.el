;;; my-mouse.el --- Initialize the mouse.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

;; Mouse
(defun new-frame-xterm-mouse-mode (&optional frame)
  "Enable xterm mouse mode for each new FRAME.
Necessary when running emacsclient in the terminal."
  (xterm-mouse-mode 1))
(add-hook 'after-make-frame-functions 'new-frame-xterm-mouse-mode)
(setq focus-follows-mouse t)
(setq mouse-autoselect-window t)
(setq mouse-wheel-follow-mouse t)

;; Set saner scrolling settings.
(pixel-scroll-precision-mode t)
(setq scroll-conservatively most-positive-fixnum)


(provide 'my-mouse)
;;; my-mouse.el ends here
