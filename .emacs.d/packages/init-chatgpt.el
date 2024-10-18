;;; init-chatgpt.el --- ChatGPT client.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:


;; 2. Do not repeat the last word or symbol before <|suggest|>.
;; 3. Start your completion immediately after the last character before <|suggest|>.
;; 6. If the completion should start on a new line, begin with a newline character.
;; 7. If the completion ends a block or statement, add a newline at the end if appropriate.

(defvar gptel-request--response nil)
(defvar gptel-request--done nil)
(defvar llm-prompt "Complete the following code snippet. Instructions:
1. Replace <|suggest|> with appropriate code or text.
2. Return only the completion, without any surrounding context.
3. Do not use backticks or other code formatting markers when completing code.
4. Preserve indentation of the current line.
5. If the completion should require surrounding whitespace, add it appropriately to preserve indentation and style.

Context:
%s")

(use-package gptel
  :after company
  :ensure t
  :preface
  (defun company-gptel--end-of-response ()
    (setq gptel-request--done t))

  (defun company-gptel--get-buffer-content ()
    "Get the content of the current buffer as a string."
    (buffer-substring-no-properties (point-min) (point-max)))

  (defun company-gptel--insert-suggest-token (content)
    "Insert <|suggest|> token at the cursor position in CONTENT."
    (let ((position (- (point) (point-min))))
      (concat (substring content 0 position)
              "<|suggest|>"
              (substring content position))))

  (defun company-gptel--wait-for-suggestion (prompt)
    "Synchronously wait for a PROMPT completion suggestion from the LLM."
    (setq gptel-request--response "")
    (setq gptel-request--done nil)
    ;; XXX: This assumes that the gptel library works by returning single
    ;;      responses. Maybe I need to turn the stream option off.
    (gptel-request prompt :callback (lambda (response info)
                                      (setq gptel-request--done t)
                                      (setq gptel-request--response response)))

    ;; Wait for the callback to complete.
    (while (not gptel-request--done)
      (message "%s" gptel-request--response)
      (sit-for 0.5))
    ;; Post-process the response.
    gptel-request--response)

  (defun company-gptel-backend (command &optional arg &rest ignored)
    "Company backend providing completions using gptel."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-gptel-backend))
      ;; NB: Support completions in essentially any mode.
      (prefix (company-grab-word))
      ;; NB: Generate a list of possible completions at a point in a buffer.
      ;;     We want to grab the contents of the current buffer and generate a
      ;;     suggestion at the cursor location.
      (candidates
       (when arg
         (let* ((buffer-content (company-gptel--get-buffer-content))
                (query-content (company-gptel--insert-suggest-token buffer-content))
                (prompt (format llm-prompt query-content))
                (response (company-gptel--wait-for-suggestion prompt)))
           (when response
             (if (string-prefix-p arg response)
                 (list response) ; LLM already included the prefix.
               (list (concat arg response)))))))))

  :config
  (add-hook 'gtpel-post-response-functions #'company-gptel--end-of-response)

  (setq gptel-model "gpt-4o")
  (setq gptel-backend (gptel-make-azure "KAI-AI"
                        :protocol "https"
                        :host (getenv "AZURE_OPENAI_ENDPOINT")
                        :endpoint
                        (let ((base-url "/openai/deployments/")
                              (deployment (getenv "AZURE_OPENAI_DEPLOYMENT"))
                              (middle-path "/chat/completions?api-version=")
                              (api-version (getenv "AZURE_OPENAI_API_VERSION")))
                          (mapconcat 'identity (list base-url deployment middle-path api-version) ""))
                        :stream t
                        :key (getenv "AZURE_OPENAI_KEY")
                        :models '(gpt-4o))))

(use-package my-keybindings
  :after company evil gptel
  :commands make-map
  :defines evil-insert-state-map space-keymap vspace-keymap
  :preface
  (defun company-complete-gptel ()
    "Manually trigger `company-gptel-backend` for completion."
    (interactive)
    (let ((company-backends '(company-gptel-backend)))
      (company-complete)))

  (defun company-suggest-gptel ()
    "Manually trigger `company-gptel-backend` for suggestion."
    (interactive)
    (let ((company-backends '(company-gptel-backend))
          (company-minimum-prefix-length 0))
      (company-manual-begin)))
  :config
  (define-key evil-insert-state-map (kbd "M-a") #'company-suggest-gptel)
  (make-map space-keymap
            '(("a" #'company-complete-gptel)
              ("A" #'gptel-send))))

(provide 'init-chatgpt)
;;; init-chatgpt.el ends here
