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
  (defun query-chatgpt ()
    "Insert an 'org-mode block for interacting with OpenAI."
    (interactive)
    (let ((text (read-string "ChatGPT: ")))
      (when (not (string= text ""))
        ;; Create a new vsplit and switch to it.
        (unless (string= (buffer-name) my-org-ai-buffer-name)
          (evil-window-split 20 my-org-ai-buffer-name))
        ;; Make sure that org-mode was enabled for the scratch buffer.
        (org-mode)
        ;; Have the buffer wrap lines.
        (visual-line-mode)
        (goto-char (point-max))
        (insert "\n\n#+begin_ai\n" text "\n#+end_ai")
        (org-ai-complete-block))))
  :init
  (setq my-org-ai-buffer-name "*my-llm*")
  (setq org-ai-default-max-tokens 500)
  (setq org-ai-default-chat-model "gpt-3.5-turbo")
  (setq org-ai-default-chat-system-prompt
        "You are a helpful assistant inside Emacs. Word-wrap all of your responses to 80 characters.")
  (add-hook 'org-mode-hook #'org-ai-mode)
  :custom
  (org-ai-openai-api-token (getenv "OPENAI_API_KEY")))

(use-package my-keybindings
  :after org-ai
  :commands make-map
  :defines space-keymap
  :config
  (make-map space-keymap
            '(("a" 'query-chatgpt))))

(provide 'init-chatgpt)
;;; init-chatgpt.el ends here
