;;; init.el --- erhlee.bird's emacs init file.
;;; Commentary:

;;; Code:

;; Separate customize settings.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'warnings)
(setq warning-minimum-level :error)

;; -----------------------------------------------------------------------------
;; Handle package initialization and management.

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(when (< emacs-major-version 27)
  (package-initialize))

                                        ; Bootstrap use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

                                        ; Add a check for an environment variable
                                        ; `LOAD` before setting this value.
(when (getenv "ENSURE")
  (setq use-package-always-ensure t))

(use-package diminish :ensure t)

                                        ; Automatically upgrade packages.
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-at-time "09:00")
  (auto-package-update-maybe)
  :commands (auto-package-update-at-time
             auto-package-update-maybe))

(add-to-list 'load-path "~/.emacs.d/packages")

                                        ; Call '(use-package-report) to see how
                                        ; long packages are taking at startup.
(setq use-package-compute-statistics t)

                                        ; Bootstrap straight.el.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; -----------------------------------------------------------------------------
;; Establish essential keymaps.
;; Lets us bind keys into the appropriate keymaps.
;;
;; ```
;; (use-package ...
;;   :bind (:map space-display-keymap
;;          ("m" . mark-whole-buffer))
;;   :defines space-display-keymap vspace-keymap
;;   ...)
;; ```

(use-package my-keybindings :ensure nil)

;; -----------------------------------------------------------------------------
;; Configure essential packages.

(use-package init-evil :ensure nil)

(use-package vertico
  :custom
  (vertico-count 20)
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind
  (:map vertico-map
	    ("DEL" . vertico-directory-delete-char)
	    ("/" . vertico-insert))
  :ensure nil)

(use-package marginalia
  :bind
  (:map minibuffer-local-map
        ("M-a" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))
  :custom
  (completion-category-defaults nil)
  (completion-category-overrides '((buffer (styles orderless+initialism))
                                   (command (styles orderless+initialism))
                                   (consult-multi (styles orderless+initialism))
                                   (file (styles orderless+initialism))
                                   (symbol (styles orderless+initialism))
                                   (variable (styles orderless+initialism))))
  ;; (completion-styles '(flex orderless basic))
  (completion-styles '(orderless))
  (orderless-matching-styles '(orderless-literal orderless-regexp)))

(use-package consult
  :after evil
  :bind
  (:map evil-normal-state-map
	    ("/" . consult-line)
	    :map space-keymap
	    ("/" . consult-ripgrep))
  :custom
  (consult-preview-key 'any)
  :defines (evil-ex-pattern-regex
            evil-ex-search-direction
            evil-ex-search-pattern
            evil-normal-state-map
            evil-search-module
            space-keymap)
  :preface
  (defun evil-ex-pattern-regex (pattern)
    "Return the regular expression of a search PATTERN."
    (nth 0 pattern))

  (defun consult-line-isearch-history (&rest _)
    "Add latest `consult-line' search pattern to the isearch history.

     This allows n and N to continue the search after `consult-line' exits."
    (when (and (bound-and-true-p evil-mode)
               (eq evil-search-module 'isearch)
               consult--line-history)
      (let* ((pattern (car consult--line-history))
             (regexp (if (string-prefix-p "\\_" pattern)
                         (substring pattern 2)
                       pattern)))
        (add-to-history 'regexp-search-ring regexp)
        (setq evil-ex-search-pattern (evil-ex-pattern-regex regexp))
        (setq evil-ex-search-direction 'forward))))

  ;; Now tell consult-line to run the function after a search
  (advice-add #'consult-line :after #'consult-line-isearch-history))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preview-current t)
  :init
  (global-corfu-mode))

(when (< emacs-major-version 31)
  (straight-use-package
   '(corfu-terminal
     :type git
     :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package eglot
  :bind
  (:map space-ide-keymap
        ("SPC" . eglot-code-actions)
        ("f" . eglot-format)
        ("d" . xref-find-definitions)
        ("x" . xref-go-back))
  :defines space-ide-keymap
  :hook
  (prog-mode . eglot-ensure))

;; (use-package eglot-booster
;;   :straight (eglot-booster :type git :host nil :repo "https://github.com/jdtsmith/eglot-booster")
;;   :after eglot
;;   :config (eglot-booster-mode))

(use-package eldoc
  :init
  (global-eldoc-mode))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p))

;; (use-package base16-theme
;;   :config
;;   (let ((codespaces (getenv "CODESPACES")))
;;     (if (and codespaces (string= codespaces "true"))
;; 	    (setq base16-theme-256-color-source 'base16-shell)
;;       (setq base16-theme-256-color-source 'colors)))
;;   (load-theme 'base16-ocean t))

(use-package rainbow-mode :diminish)
(use-package vterm :defer t)

;; Load any my-prefixed custom packages first.

(use-package my-backup :ensure nil)
(use-package my-clipboard :ensure nil)
(use-package my-display :ensure nil)
(use-package my-format :ensure nil)
(use-package my-mouse :ensure nil)
(use-package my-shell :ensure nil)
(use-package my-start-server :ensure nil)

;; -----------------------------------------------------------------------------
;; Miscellaneous settings.

                                        ; Rely on the system to open URLs.
(setq browse-url-browser-function 'browse-url-xdg-open)

                                        ; Enable which-key support.
(which-key-mode 1)
(setq-default which-key-idle-delay 0.5)

                                        ; Add Python to org-babel.
(org-babel-do-load-languages 'org-babel-load-languages '((python . t)))

                                        ; Ignore an annoying hardware macro that
                                        ; inputs XF86AudioPause as raw key events
(global-set-key (kbd "<ESC>[57429u") 'ignore)

                                        ; Load init-prefixed packages that deal
                                        ; with third-party packages.

(use-package init-aider :ensure nil)
(use-package init-chatgpt :ensure nil)
;; (use-package init-company :ensure nil)
;; (use-package init-erc :ensure nil)
(use-package init-flycheck :ensure nil)
(use-package init-github :ensure nil)
;; (use-package init-ivy :ensure nil)
;; (use-package init-lsp :ensure nil)
(use-package init-magit :ensure nil)
(use-package init-markdown :ensure nil)
(use-package init-org :ensure nil)
(use-package init-projectile :ensure nil)
;; (use-package init-rtags :ensure nil)

;; -----------------------------------------------------------------------------
;; Configure language-specific settings here.

;; Indent elisp buffers on save.
(add-hook 'emacs-lisp-mode-hook
	      (lambda ()
	        (add-hook 'before-save-hook
		              (lambda ()
			            (indent-region (point-min) (point-max)))
                      nil
                      :local)))

(use-package cc-mode
  :after flycheck
  :defer t
  :defines flycheck-gcc-language-standard
  :hook
  (c++-mode . (lambda ()
                (setq flycheck-gcc-language-standard "c++11"))))

;; (use-package cider
;;   :after lsp-mode
;;   :commands lsp-format-buffer
;;   :config
;;   (set-variable 'cider-lein-parameters (concat "with-profile +dev repl :headless"))
;;   :custom
;;   (cider-test-show-report-on-success t)
;;   :hook
;;   (clojure-mode . cider-mode)
;;   (clojure-mode . (lambda ()
;;                     (add-hook 'before-save-hook #'lsp-format-buffer nil :local))))

;; (use-package elixir-mode
;;   :after lsp-mode
;;   :hook
;;   (elixir-mode . lsp-deferred)
;;   (elixir-mode . (lambda ()
;;                    (add-hook 'before-save-hook 'elixir-format nil :local))))

;; (use-package lsp-haskell
;;   :after lsp-mode
;;   :commands lsp-haskell-enable
;;   :hook
;;   (haskell-mode . lsp-haskell-enable))

(use-package nix-mode :mode "\\.nix\\'")
(use-package yaml-mode)

;;; init.el ends here
