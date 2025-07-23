;;; init-chatgpt.el --- ChatGPT client.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(defvar gptel-request--response nil)
(defvar gptel-request--done nil)

(defvar system-directive "
As an expert software engineer with years of experience across multiple programming
languages and paradigms, you approach code completion tasks methodically and thoughtfully.
Your responses should reflect deep knowledge of software design principles, best
practices,and efficient coding techniques.

When presented with a code completion task:

1. Analyze the existing code structure and context.
2. Identify the likely purpose or functionality of the code snippet.
3. Consider multiple potential solutions, weighing their pros and cons.
4. Choose the most appropriate solution based on factors such as:
  - Readability and maintainability
  - Performance and efficiency
  - Adherence to common design patterns and idioms
  - Consistency with the existing codebase style
5. Implement the chosen solution with clean, well-formatted code.
6. Mentally review the completed code for potential edge cases or improvements.

While your primary output should be the code completion itself, be prepared to
explain your thought process or provide alternative solutions if requested.
Your goal is to produce high-quality, production-ready code that not only solves the
immediate task but also contributes to the overall robustness and maintainability
of the software project.

Remember to adhere strictly to the given rules for output formatting and avoid
any unnecessary commentary unless explicitly asked.")
(defvar llm-prompt "
Complete the following code snippet by replacing <|suggest|> markers with appropriate code or text.

Rules:
1. Output ONLY the code completion, without any additional text or explanations unless requested.
2. Do NOT use code fences, backticks, or any other formatting markers.
3. Preserve the existing indentation of the line containing <|suggest|>.
4. If the completion requires surrounding whitespace, add it to maintain indentation and style.

Example:

Given:
def add_numbers(a, b):
  <|suggest|>

Correct output:
return a + b

Incorrect output:
```python
  return a + b
```

Now, complete this code snippet:

%s")

(use-package gptel
  :after (company evil)
  :bind
  (:map evil-insert-state-map
        ("M-a" . company-suggest-gptel)
        :map space-keymap
        ("a" . company-complete-gptel)
        ("A" . gptel-send))
  :commands (company-begin-backend
             company-grab-word
             company-set-selection
             gptel-request)
  :defines (evil-insert-state-map space-keymap)
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
    (gptel-request prompt
                   :callback (lambda (response info)
                               (setq gptel-request--done t)
                               (setq gptel-request--response response))
                   :system system-directive)
    ;; Wait for the callback to complete.
    (while (not gptel-request--done)
      (sit-for 0.1))
    ;; Post-process the response.
    gptel-request--response)

  (defun company-gptel--generate-suggestion (arg)
    "Generate a suggestion using the current buffer's content."
    (let* ((buffer-content (company-gptel--get-buffer-content))
           (query-content (company-gptel--insert-suggest-token buffer-content))
           (prompt (format llm-prompt query-content)))
      (company-gptel--wait-for-suggestion prompt)))

  (defun company-suggest-gptel-new (&optional arg)
    (interactive "p")
    (when (company-manual-begin)
      ;; Append a new suggestion to the candidates.
      (company-set-selection (+ (or arg 1)))))


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
         (let ((response (company-gptel--generate-suggestion arg)))
           (when response
             (list response)))))))

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

  ;; (when response
  ;; (if (string-prefix-p arg response)
  ;; (list response) ; LLM already included the prefix.
  ;; (list (concat arg response)))))))))

  :custom
  (gptel-model "gpt-4o")
  (gptel-backend (gptel-make-azure "KAI-AI"
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
                   :models '(gptel-model))))

(provide 'init-chatgpt)
;;; init-chatgpt.el ends here
