;;; my-mouse.el --- Initialize the mouse.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

;; Mouse
(defun new-frame-xterm-mouse-mode (&optional frame)
  "Enable xterm mouse mode for each new FRAME."
  (xterm-mouse-mode))
(add-hook 'after-make-frame-functions 'new-frame-xterm-mouse-mode)
(setq focus-follows-mouse t)
(setq mouse-autoselect-window t)
(setq mouse-wheel-follow-mouse t)

(provide 'my-mouse)
;;; my-mouse.el ends here
