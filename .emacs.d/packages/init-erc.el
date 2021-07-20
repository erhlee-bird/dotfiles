;;; erc-init.el --- Customize erc.
;;; Commentary:

;;; Code:
(require 'erc)

(setq erc-nick "ebird")
(setq erc-nick-uniquifier "_")
(setq erc-current-nick-highlight-type 'nick)
(setq erc-server "irc.freenode.net")
(setq erc-port 6667)

(setq erc-join-buffer 'bury)
(setq erc-prompt ">")
(setq erc-query-display 'buffer)
(setq erc-server-coding-system '(utf-8 . utf-8))

(setq erc-kill-buffer-on-part t)
(setq erc-kill-queries-on-quit t)
(setq erc-kill-server-buffer-on-quit t)

(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook
          '(lambda ()
             (save-excursion
               (walk-windows
                (lambda (w)
                  (let ((buffer (window-buffer w)))
                    (set-buffer buffer)
                    (when (eq major-mode 'erc-mode)
                      (setq erc-fill-column (- (window-width w) 2)))))))))

(setq erc-interpret-mirc-color t)

(defun x-urgency-hint (frame arg &optional source)
  "Set the x-urgency hint for the FRAME to ARG:

- If arg is nil, unset the urgency.
- If arg is any other value, set the urgency from SOURCE."
  (let* ((wm-hints (append (x-window-property
                            "WM_HINTS" frame "WM_HINTS"
                            source nil t) nil))
         (flags (car wm-hints)))
    (setcar wm-hints
            (if arg
                (logior flags #x00000100)
              (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun x-urgent (&optional arg)
  "Mark the current Emacs frame as requiring urgent attention.
With a prefix argument ARG which does not equal a boolean value
of nil, remove the urgency flag (which might or might not change
display, depending on the window manager)."
  (interactive "P")
  (let (frame (car (car (cdr (current-frame-configuration)))))
    (x-urgency-hint frame (not arg))))

(add-hook 'erc-server-PRIVMSG-functions
          (lambda (proc parsed) (x-urgent) nil))
(add-hook 'erc-text-matched-hook
          (lambda (match-type nickuserhost msg) (x-urgent) nil))


; Logging
(require 'erc-log)
(setq erc-log-channels-directory "~/.erc/logs/")
(if (not (file-exists-p erc-log-channels-directory))
    (mkdir erc-log-channels-directory t))
(setq erc-save-buffer-on-part t)
(erc-log-enable)


; Autojoin
(require 'erc-join)
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net" "#pwning" "#python" "#ocaml")))
(setq erc-autojoin-timing 'ident)


; Tracking
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))


(defun my-erc ()
  "Start an ERC session or switch to an active ERC buffer."
  (interactive)
  (let (final-list (list))
    (dolist (buf (buffer-list) final-list)
      (if (equal 'erc-mode (with-current-buffer buf major-mode))
          (setq final-list (append (list (buffer-name buf)) final-list))))
    (if final-list
        (switch-to-buffer (completing-read "Buffer: " final-list))
      (call-interactively 'erc))))

(provide 'erc-init)
;;; erc-init.el ends here
