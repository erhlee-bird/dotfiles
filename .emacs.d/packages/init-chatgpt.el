;;; init-chatgpt.el --- ChatGPT client.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(use-package org-ai
  :after evil
  :straight (:type git
             :host github
             :local-repo "org-ai"
             :repo "rksm/org-ai"
             :files ("README.md" "*.el"))
  :commands org-ai-mode
  :preface
  (defun query-chatgpt (prompt)
    "Insert an 'org-mode block for interacting with OpenAI."
    (unless (string= prompt "")
      ;; Create a new vsplit and switch to it.
      (unless (string= (buffer-name) my-org-ai-buffer-name)
        ;; NB: `evil-window-split` creates a file which we don't want with our
        ;;     chatgpt buffer.
        (evil-window-split 20)
        (switch-to-buffer my-org-ai-buffer-name))
      ;; Make sure that org-mode was enabled for the scratch buffer.
      (org-mode)
      (org-ai-mode)
      ;; Have the buffer wrap lines.
      (visual-line-mode)
      (goto-char (point-max))
      (insert "\n\n#+begin_ai\n" prompt "\n#+end_ai")
      (org-ai-complete-block)))
  (defun query-chatgpt--interactive ()
    (interactive)
    (let ((prompt (read-string "ChatGPT: ")))
      (query-chatgpt prompt)))
  (defun query-chatgpt--region (start end)
    (interactive "r")
    (let* ((prompt (read-string "ChatGPT: "))
           (region (buffer-substring-no-properties start end)))
      (query-chatgpt (format "%s\n\n%s" prompt region))))
  :init
  (setq my-org-ai-buffer-name "*scratch*")
  (setq org-ai-default-max-tokens 500)
  (setq org-ai-default-chat-model "gpt-3.5-turbo")
  :custom
  (org-ai-openai-api-token (getenv "OPENAI_API_KEY")))

(use-package my-keybindings
  :commands make-map
  :defines space-keymap vspace-keymap
  :config
  (make-map space-keymap
            '(("a" #'query-chatgpt--interactive)))
  (make-map vspace-keymap
            '(("a" #'query-chatgpt--region))))

(provide 'init-chatgpt)
;;; init-chatgpt.el ends here
