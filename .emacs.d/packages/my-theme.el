;;; my-theme.el -- Theme configuration settings.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

;; Set our font
(set-face-attribute 'default nil :family "Inconsolata" :height 120)

;; Set our theme and set the default frame to have the proper theme.
;; Fix for daemon mode.
(defvar mytheme-theme 'base16-ocean)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

(defun daemon-theme-hook (frame)
  "When running in daemon mode, set the theme each time a FRAME is created."
  (select-frame frame)
  (load-theme mytheme-theme t))

(if (daemonp)
    (add-hook 'after-make-frame-functions 'daemon-theme-hook)
  (load-theme mytheme-theme t))

(provide 'my-theme)
;;; my-theme.el ends here
