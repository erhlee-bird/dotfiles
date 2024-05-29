;;; init.el --- erhlee.bird's emacs init file.
;;; Commentary:

;;; Code:

;; Separate customize settings.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Setup package.
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Bootstrap straight.el.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Enable unicode rendering.
(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

;; Rely on the system to open URLs.
(setq browse-url-browser-function 'browse-url-xdg-open)

;; Add Python to org-babel.
(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))

(use-package counsel :ensure t)
(use-package projectile :ensure t)
(use-package counsel-projectile :ensure t)

(use-package auto-sudoedit :ensure t)
(auto-sudoedit-mode 1)
(use-package nix-mode :ensure t)

(add-to-list 'load-path "~/.emacs.d/packages")
;; Load any my-prefixed custom packages first.
;; Enable/Disable as desired below.
(use-package my-start-server)
(use-package my-display)
(use-package my-format)

;; (use-package my-theme)
(use-package base16-theme
  :ensure t
  :config
  (setq base16-theme-256-color-source 'colors)
  (load-theme 'base16-ocean t))

(use-package my-shell)
(use-package my-clipboard)
(use-package my-backup)

;; Load init-prefixed packages that deal with third-party packages.
;; (use-package init-chatgpt)
(use-package init-company)
; (use-package init-erc)
(use-package init-evil)
(use-package init-flycheck)
(use-package init-github)
(use-package init-ivy)
(use-package init-lsp)
; (use-package init-eglot)
(use-package init-magit)
(use-package init-markdown)
(use-package init-org)
; (use-package init-rtags)

;;; init.el ends here
