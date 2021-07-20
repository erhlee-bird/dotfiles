;;; init-rtags.el --- Customize rtags.
;;; Commentary:

;;; Code:

(message "whos' loading this?")
;; (use-package rtags
  ;; :config
  ;; (setq rtags-autostart-diagnostics t)
  ;; (rtags-diagnostics)
  ;; (setq rtags-completions-enabled t)
  ;; (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
  ;; (add-hook 'c++-mode-common-hook 'rtags-start-process-unless-running)
  ;; )
; (use-package flycheck-rtags
;   :after flycheck
;   :commands flycheck-select-checker
;   :config
;   (add-hook 'c-mode-common-hook
;             (lambda ()
;               (flycheck-select-checker 'rtags)
;               ; RTags has better overlays.
;               (setq-local flycheck-highlighting-mode nil)
;               (setq-local flycheck-check-syntax-automatically nil))))

(provide 'init-rtags)
;;; init-rtags.el ends here
