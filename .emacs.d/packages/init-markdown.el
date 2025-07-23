;;; init-markdown.el --- Customize markdown-mode.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :custom
  (markdown-command "pandoc")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(provide 'init-markdown)
;;; init-markdown.el ends here
