;;; init-slime.el --- Customize slime.
;; Author: Eric Lee <erhlee.bird@gmail.com>
;; Version: 0.1.0

;;; Commentary:

;;; Code:

(use-package slime
  :ensure t
  :config
  (add-to-list 'slime-contribs 'slime-fancy))

(provide 'init-slime)
;;; init-slime.el ends here
