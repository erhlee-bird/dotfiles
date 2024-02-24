;; Base16 Ocean (https://github.com/chriskempson/base16)
;; Scheme: Chris Kempson (http://chriskempson.com)

;;; base16-ocean-dual-theme.el

;;; Code:

;; The following macros were used to make the conversions.
; @r yypk0
; @t /\(\(tftcl,t_type
; @y f,ea_t
; @g /\(\(tftcl,g_type
; @h f,ea_g
; @u 0@t10@y
; @j 0@g10@h
; @c @r:try|exe "norm! @u"|endtryj:try|exe "norm!"€kb @j"|endtryo

(deftheme base16-ocean-dual)

(let
    ((g_type '((type graphic)))
     (t_type '((type tty)))

     (base00_g "#2b303b")
     (base01_g "#343d46")
     (base02_g "#4f5b66")
     (base03_g "#65737e")
     (base04_g "#a7adba")
     (base05_g "#c0c5ce")
     (base06_g "#dfe1e8")
     (base07_g "#eff1f5")
     (base08_g "#bf616a")
     (base09_g "#d08770")
     (base0A_g "#ebcb8b")
     (base0B_g "#a3be8c")
     (base0C_g "#96b5b4")
     (base0D_g "#8fa1b3")
     (base0E_g "#b48ead")
     (base0F_g "#ab7967")

     (base00_t "black")
     (base01_t "color-18")
     (base02_t "color-19")
     (base03_t "brightblack")
     (base04_t "color-20")
     (base05_t "white")
     (base06_t "color-21")
     (base07_t "brightwhite")
     (base08_t "red")
     (base09_t "color-16")
     (base0A_t "yellow")
     (base0B_t "green")
     (base0C_t "cyan")
     (base0D_t "blue")
     (base0E_t "magenta")
     (base0F_t "color-17"))


  (custom-theme-set-faces
   'base16-ocean-dual

   ;; Built-in stuff (Emacs 23)
   `(border ((,t_type (:background ,base03_t))))
   `(border ((,g_type (:background ,base03_g))))

   `(border-glyph ((t (nil))))

   `(cursor ((,t_type (:background ,base08_t))))
   `(cursor ((,g_type (:background ,base08_g))))

   `(default ((,t_type (:background ,base00_t :foreground ,base05_t))))
   `(default ((,g_type (:background ,base00_g :foreground ,base05_g))))

   `(fringe ((,t_type (:background ,base02_t))))
   `(fringe ((,g_type (:background ,base02_g))))

   `(gui-element ((,t_type (:background ,base03_t :foreground ,base06_t))))
   `(gui-element ((,g_type (:background ,base03_g :foreground ,base06_g))))

   `(highlight ((,t_type (:background ,base01_t))))
   `(highlight ((,g_type (:background ,base01_g))))

   `(link ((,t_type (:foreground ,base0D_t))))
   `(link ((,g_type (:foreground ,base0D_g))))

   `(link-visited ((,t_type (:foreground ,base0E_t))))
   `(link-visited ((,g_type (:foreground ,base0E_g))))

   `(minibuffer-prompt ((,t_type (:foreground ,base0D_t))))
   `(minibuffer-prompt ((,g_type (:foreground ,base0D_g))))

   `(mode-line ((,t_type (:background ,base02_t :foreground ,base04_t :box nil))))
   `(mode-line ((,g_type (:background ,base02_g :foreground ,base04_g :box nil))))

   `(mode-line-buffer-id ((,t_type (:foreground ,base0E_t :background nil))))
   `(mode-line-buffer-id ((,g_type (:foreground ,base0E_g :background nil))))

   `(mode-line-emphasis ((,t_type (:foreground ,base06_t :slant italic))))
   `(mode-line-emphasis ((,g_type (:foreground ,base06_g :slant italic))))

   `(mode-line-highlight ((,t_type (:foreground ,base0E_t :box nil :weight bold))))
   `(mode-line-highlight ((,g_type (:foreground ,base0E_g :box nil :weight bold))))

   `(mode-line-inactive ((,t_type (:background ,base01_t :foreground ,base03_t :box nil))))
   `(mode-line-inactive ((,g_type (:background ,base01_g :foreground ,base03_g :box nil))))

   `(region ((,t_type (:background ,base02_t))))
   `(region ((,g_type (:background ,base02_g))))

   `(secondary-selection ((,t_type (:background ,base03_t))))
   `(secondary-selection ((,g_type (:background ,base03_g))))

   `(error ((,t_type (:foreground ,base08_t :weight bold))))
   `(error ((,g_type (:foreground ,base08_g :weight bold))))

   `(warning ((,t_type (:foreground ,base09_t :weight bold))))
   `(warning ((,g_type (:foreground ,base09_g :weight bold))))

   `(success ((,t_type (:foreground ,base0B_t :weight bold))))
   `(success ((,g_type (:foreground ,base0B_g :weight bold))))

   `(header-line ((,t_type (:inherit mode-line :foreground ,base0E_t :background nil))))
   `(header-line ((,g_type (:inherit mode-line :foreground ,base0E_g :background nil))))

   ;; Font-lock stuff
   `(font-lock-builtin-face ((,t_type (:foreground ,base0C_t))))
   `(font-lock-builtin-face ((,g_type (:foreground ,base0C_g))))

   `(font-lock-comment-delimiter-face ((,t_type (:foreground ,base02_t))))
   `(font-lock-comment-delimiter-face ((,g_type (:foreground ,base02_g))))

   `(font-lock-comment-face ((,t_type (:foreground ,base03_t))))
   `(font-lock-comment-face ((,g_type (:foreground ,base03_g))))

   `(font-lock-constant-face ((,t_type (:foreground ,base09_t))))
   `(font-lock-constant-face ((,g_type (:foreground ,base09_g))))

   `(font-lock-doc-face ((,t_type (:foreground ,base04_t))))
   `(font-lock-doc-face ((,g_type (:foreground ,base04_g))))

   `(font-lock-doc-string-face ((,t_type (:foreground ,base03_t))))
   `(font-lock-doc-string-face ((,g_type (:foreground ,base03_g))))

   `(font-lock-function-name-face ((,t_type (:foreground ,base0D_t))))
   `(font-lock-function-name-face ((,g_type (:foreground ,base0D_g))))

   `(font-lock-keyword-face ((,t_type (:foreground ,base0E_t))))
   `(font-lock-keyword-face ((,g_type (:foreground ,base0E_g))))

   `(font-lock-negation-char-face ((,t_type (:foreground ,base0B_t))))
   `(font-lock-negation-char-face ((,g_type (:foreground ,base0B_g))))

   `(font-lock-preprocessor-face ((,t_type (:foreground ,base0D_t))))
   `(font-lock-preprocessor-face ((,g_type (:foreground ,base0D_g))))

   `(font-lock-regexp-grouping-backslash ((,t_type (:foreground ,base0A_t))))
   `(font-lock-regexp-grouping-backslash ((,g_type (:foreground ,base0A_g))))

   `(font-lock-regexp-grouping-construct ((,t_type (:foreground ,base0E_t))))
   `(font-lock-regexp-grouping-construct ((,g_type (:foreground ,base0E_g))))

   `(font-lock-string-face ((,t_type (:foreground ,base0B_t))))
   `(font-lock-string-face ((,g_type (:foreground ,base0B_g))))

   `(font-lock-type-face ((,t_type (:foreground ,base0A_t))))
   `(font-lock-type-face ((,g_type (:foreground ,base0A_g))))

   `(font-lock-variable-name-face ((,t_type (:foreground ,base0C_t))))
   `(font-lock-variable-name-face ((,g_type (:foreground ,base0C_g))))

   `(font-lock-warning-face ((,t_type (:foreground ,base08_t))))
   `(font-lock-warning-face ((,g_type (:foreground ,base08_g))))

   ;; Search
   `(match ((,t_type (:foreground ,base0D_t :background ,base01_t :inverse-video t))))
   `(match ((,g_type (:foreground ,base0D_g :background ,base01_g :inverse-video t))))

   `(isearch ((,t_type (:foreground ,base0A_t :background ,base01_t :inverse-video t))))
   `(isearch ((,g_type (:foreground ,base0A_g :background ,base01_g :inverse-video t))))

   `(isearch-lazy-highlight-face ((,t_type (:foreground ,base0C_t :background ,base01_t :inverse-video t))))
   `(isearch-lazy-highlight-face ((,g_type (:foreground ,base0C_g :background ,base01_g :inverse-video t))))

   `(isearch-fail ((,t_type (:background ,base01_t :inherit font-lock-warning-face :inverse-video t))))
   `(isearch-fail ((,g_type (:background ,base01_g :inherit font-lock-warning-face :inverse-video t))))

   ;; Flymake
   `(flymake-warnline ((,t_type (:underline ,base09_t :background ,base01_t))))
   `(flymake-warnline ((,g_type (:underline ,base09_g :background ,base01_g))))

   `(flymake-errline ((,t_type (:underline ,base08_t :background ,base01_t))))
   `(flymake-errline ((,g_type (:underline ,base08_g :background ,base01_g))))

   ;; Clojure errors
   `(clojure-test-failure-face ((,t_type (:background nil :inherit flymake-warnline))))
   `(clojure-test-failure-face ((,g_type (:background nil :inherit flymake-warnline))))

   `(clojure-test-error-face ((,t_type (:background nil :inherit flymake-errline))))
   `(clojure-test-error-face ((,g_type (:background nil :inherit flymake-errline))))

   `(clojure-test-success-face ((,t_type (:background nil :foreground nil :underline ,base0B_t))))
   `(clojure-test-success-face ((,g_type (:background nil :foreground nil :underline ,base0B_g))))

   ;; For Brian Carper's extended clojure syntax table
   `(clojure-keyword ((,t_type (:foreground ,base0A_t))))
   `(clojure-keyword ((,g_type (:foreground ,base0A_g))))

   `(clojure-parens ((,t_type (:foreground ,base06_t))))
   `(clojure-parens ((,g_type (:foreground ,base06_g))))

   `(clojure-braces ((,t_type (:foreground ,base0B_t))))
   `(clojure-braces ((,g_type (:foreground ,base0B_g))))

   `(clojure-brackets ((,t_type (:foreground ,base0A_t))))
   `(clojure-brackets ((,g_type (:foreground ,base0A_g))))

   `(clojure-double-quote ((,t_type (:foreground ,base0C_t :background nil))))
   `(clojure-double-quote ((,g_type (:foreground ,base0C_g :background nil))))

   `(clojure-special ((,t_type (:foreground ,base0D_t))))
   `(clojure-special ((,g_type (:foreground ,base0D_g))))

   `(clojure-java-call ((,t_type (:foreground ,base0E_t))))
   `(clojure-java-call ((,g_type (:foreground ,base0E_g))))

   ;; MMM-mode
   `(mmm-code-submode-face ((,t_type (:background ,base03_t))))
   `(mmm-code-submode-face ((,g_type (:background ,base03_g))))

   `(mmm-comment-submode-face ((,t_type (:inherit font-lock-comment-face))))
   `(mmm-comment-submode-face ((,g_type (:inherit font-lock-comment-face))))

   `(mmm-output-submode-face ((,t_type (:background ,base03_t))))
   `(mmm-output-submode-face ((,g_type (:background ,base03_g))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,t_type (:foreground ,base0E_t))))
   `(rainbow-delimiters-depth-1-face ((,g_type (:foreground ,base0E_g))))

   `(rainbow-delimiters-depth-2-face ((,t_type (:foreground ,base0D_t))))
   `(rainbow-delimiters-depth-3-face ((,g_type (:foreground ,base0C_g))))

   `(rainbow-delimiters-depth-4-face ((,t_type (:foreground ,base0B_t))))
   `(rainbow-delimiters-depth-4-face ((,g_type (:foreground ,base0B_g))))

   `(rainbow-delimiters-depth-5-face ((,t_type (:foreground ,base0A_t))))
   `(rainbow-delimiters-depth-5-face ((,g_type (:foreground ,base0A_g))))

   `(rainbow-delimiters-depth-6-face ((,t_type (:foreground ,base09_t))))
   `(rainbow-delimiters-depth-6-face ((,g_type (:foreground ,base09_g))))

   `(rainbow-delimiters-depth-7-face ((,t_type (:foreground ,base08_t))))
   `(rainbow-delimiters-depth-7-face ((,g_type (:foreground ,base08_g))))

   `(rainbow-delimiters-depth-8-face ((,t_type (:foreground ,base03_t))))
   `(rainbow-delimiters-depth-8-face ((,g_type (:foreground ,base03_g))))

   `(rainbow-delimiters-depth-9-face ((,t_type (:foreground ,base05_t))))
   `(rainbow-delimiters-depth-9-face ((,g_type (:foreground ,base05_g))))

   ;; IDO
   `(ido-subdir ((,t_type (:foreground ,base04_t))))
   `(ido-subdir ((,g_type (:foreground ,base04_g))))

   `(ido-first-match ((,t_type (:foreground ,base09_t :weight bold))))
   `(ido-first-match ((,g_type (:foreground ,base09_g :weight bold))))

   `(ido-only-match ((,t_type (:foreground ,base08_t :weight bold))))
   `(ido-only-match ((,g_type (:foreground ,base08_g :weight bold))))

   `(ido-indicator ((,t_type (:foreground ,base08_t :background ,base01_t))))
   `(ido-indicator ((,g_type (:foreground ,base08_g :background ,base01_g))))

   `(ido-virtual ((,t_type (:foreground ,base04_t))))
   `(ido-virtual ((,g_type (:foreground ,base04_g))))

   ;; which-function
   `(which-func ((,t_type (:foreground ,base0D_t :background nil :weight bold))))
   `(which-func ((,g_type (:foreground ,base0D_g :background nil :weight bold))))

   `(trailing-whitespace ((,t_type (:background ,base0C_t :foreground ,base0A_t))))
   `(trailing-whitespace ((,g_type (:background ,base0C_g :foreground ,base0A_g))))

   `(whitespace-empty ((,t_type (:foreground ,base08_t :background ,base0A_t))))
   `(whitespace-empty ((,g_type (:foreground ,base08_g :background ,base0A_g))))

   `(whitespace-hspace ((,t_type (:background ,base04_t :foreground ,base04_t))))
   `(whitespace-hspace ((,g_type (:background ,base04_g :foreground ,base04_g))))

   `(whitespace-indentation ((,t_type (:background ,base0A_t :foreground ,base08_t))))
   `(whitespace-indentation ((,g_type (:background ,base0A_g :foreground ,base08_g))))

   `(whitespace-line ((,t_type (:background ,base01_t :foreground ,base0F_t))))
   `(whitespace-line ((,g_type (:background ,base01_g :foreground ,base0F_g))))

   `(whitespace-newline ((,t_type (:foreground ,base04_t))))
   `(whitespace-newline ((,g_type (:foreground ,base04_g))))

   `(whitespace-space ((,t_type (:background ,base01_t :foreground ,base04_t))))
   `(whitespace-space ((,g_type (:background ,base01_g :foreground ,base04_g))))

   `(whitespace-space-after-tab ((,t_type (:background ,base0A_t :foreground ,base08_t))))
   `(whitespace-space-after-tab ((,g_type (:background ,base0A_g :foreground ,base08_g))))

   `(whitespace-space-before-tab ((,t_type (:background ,base09_t :foreground ,base08_t))))
   `(whitespace-space-before-tab ((,g_type (:background ,base09_g :foreground ,base08_g))))

   `(whitespace-tab ((,t_type (:background ,base04_t :foreground ,base04_t))))
   `(whitespace-tab ((,g_type (:background ,base04_g :foreground ,base04_g))))

   `(whitespace-trailing ((,t_type (:background ,base08_t :foreground ,base0A_t))))
   `(whitespace-trailing ((,g_type (:background ,base08_g :foreground ,base0A_g))))

   ;; Parenthesis matching (built-in)
   `(show-paren-match ((,t_type (:background ,base0D_t :foreground ,base03_t))))
   `(show-paren-match ((,g_type (:background ,base0D_g :foreground ,base03_g))))

   `(show-paren-mismatch ((,t_type (:background ,base09_t :foreground ,base03_t))))
   `(show-paren-mismatch ((,g_type (:background ,base09_g :foreground ,base03_g))))

   ;; Parenthesis matching (mic-paren)
   `(paren-face-match ((t (:foreground nil :background nil :inherit show-paren-match))))
   `(paren-face-mismatch ((t (:foreground nil :background nil :inherit show-paren-mismatch))))
   `(paren-face-no-match ((t (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; Parenthesis dimming (parenface)
   `(paren-face ((,t_type (:foreground ,base04_t :background nil))))
   `(paren-face ((,g_type (:foreground ,base04_g :background nil))))

   `(sh-heredoc ((t (:foreground nil :inherit font-lock-string-face :weight normal))))
   `(sh-quoted-exec ((t (:foreground nil :inherit font-lock-preprocessor-face))))
   `(slime-highlight-edits-face ((t (:weight bold))))
   `(slime-repl-input-face ((t (:weight normal :underline nil))))
   `(slime-repl-prompt-face ((,t_type (:underline nil :weight bold :foreground ,base0E_t))))
   `(slime-repl-prompt-face ((,g_type (:underline nil :weight bold :foreground ,base0E_g))))

   `(slime-repl-result-face ((,t_type (:foreground ,base0B_t))))
   `(slime-repl-result-face ((,g_type (:foreground ,base0B_g))))

   `(slime-repl-output-face ((,t_type (:foreground ,base0D_t :background ,base01_t))))
   `(slime-repl-output-face ((,g_type (:foreground ,base0D_g :background ,base01_g))))

   `(csv-separator-face ((,t_type (:foreground ,base09_t))))
   `(csv-separator-face ((,g_type (:foreground ,base09_g))))

   `(diff-added ((,t_type (:foreground ,base0B_t))))
   `(diff-added ((,g_type (:foreground ,base0B_g))))

   `(diff-changed ((,t_type (:foreground ,base0A_t))))
   `(diff-changed ((,g_type (:foreground ,base0A_g))))

   `(diff-removed ((,t_type (:foreground ,base08_t))))
   `(diff-removed ((,g_type (:foreground ,base08_g))))

   `(diff-header ((,t_type (:background ,base01_t))))
   `(diff-header ((,g_type (:background ,base01_g))))

   `(diff-file-header ((,t_type (:background ,base02_t))))
   `(diff-file-header ((,g_type (:background ,base02_g))))

   `(diff-hunk-header ((,t_type (:background ,base01_t :foreground ,base0E_t))))
   `(diff-hunk-header ((,g_type (:background ,base01_g :foreground ,base0E_g))))

   `(ediff-even-diff-A ((t (:foreground nil :background nil :inverse-video t))))
   `(ediff-even-diff-B ((t (:foreground nil :background nil :inverse-video t))))
   `(ediff-odd-diff-A  ((,t_type (:foreground ,base04_t :background nil :inverse-video t))))
   `(ediff-odd-diff-A  ((,g_type (:foreground ,base04_g :background nil :inverse-video t))))

   `(ediff-odd-diff-B  ((,t_type (:foreground ,base04_t :background nil :inverse-video t))))
   `(ediff-odd-diff-B  ((,g_type (:foreground ,base04_g :background nil :inverse-video t))))

   `(eldoc-highlight-function-argument ((,t_type (:foreground ,base0B_t :weight bold))))
   `(eldoc-highlight-function-argument ((,g_type (:foreground ,base0B_g :weight bold))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face ((,t_type (:foreground ,base06_t))))
   `(undo-tree-visualizer-default-face ((,g_type (:foreground ,base06_g))))

   `(undo-tree-visualizer-current-face ((,t_type (:foreground ,base0B_t :weight bold))))
   `(undo-tree-visualizer-current-face ((,g_type (:foreground ,base0B_g :weight bold))))

   `(undo-tree-visualizer-active-branch-face ((,t_type (:foreground ,base08_t))))
   `(undo-tree-visualizer-active-branch-face ((,g_type (:foreground ,base08_g))))

   `(undo-tree-visualizer-register-face ((,t_type (:foreground ,base0A_t))))
   `(undo-tree-visualizer-register-face ((,g_type (:foreground ,base0A_g))))

   ;; auctex
   `(font-latex-bold-face ((,t_type (:foreground ,base0B_t))))
   `(font-latex-bold-face ((,g_type (:foreground ,base0B_g))))

   `(font-latex-doctex-documentation-face ((,t_type (:background ,base03_t))))
   `(font-latex-doctex-documentation-face ((,g_type (:background ,base03_g))))

   `(font-latex-italic-face ((,t_type (:foreground ,base0B_t))))
   `(font-latex-italic-face ((,g_type (:foreground ,base0B_g))))

   `(font-latex-math-face ((,t_type (:foreground ,base09_t))))
   `(font-latex-math-face ((,g_type (:foreground ,base09_g))))

   `(font-latex-sectioning-0-face ((,t_type (:foreground ,base0A_t))))
   `(font-latex-sectioning-0-face ((,g_type (:foreground ,base0A_g))))

   `(font-latex-sectioning-1-face ((,t_type (:foreground ,base0A_t))))
   `(font-latex-sectioning-1-face ((,g_type (:foreground ,base0A_g))))

   `(font-latex-sectioning-2-face ((,t_type (:foreground ,base0A_t))))
   `(font-latex-sectioning-2-face ((,g_type (:foreground ,base0A_g))))

   `(font-latex-sectioning-3-face ((,t_type (:foreground ,base0A_t))))
   `(font-latex-sectioning-3-face ((,g_type (:foreground ,base0A_g))))

   `(font-latex-sectioning-4-face ((,t_type (:foreground ,base0A_t))))
   `(font-latex-sectioning-4-face ((,g_type (:foreground ,base0A_g))))

   `(font-latex-sectioning-5-face ((,t_type (:foreground ,base0A_t))))
   `(font-latex-sectioning-5-face ((,g_type (:foreground ,base0A_g))))

   `(font-latex-sedate-face ((,t_type (:foreground ,base0C_t))))
   `(font-latex-sedate-face ((,g_type (:foreground ,base0C_g))))

   `(font-latex-string-face ((,t_type (:foreground ,base0A_t))))
   `(font-latex-string-face ((,g_type (:foreground ,base0A_g))))

   `(font-latex-verbatim-face ((,t_type (:foreground ,base09_t))))
   `(font-latex-verbatim-face ((,g_type (:foreground ,base09_g))))

   `(font-latex-warning-face ((,t_type (:foreground ,base08_t))))
   `(font-latex-warning-face ((,g_type (:foreground ,base08_g))))


   ;; dired+
   `(diredp-compressed-file-suffix ((,t_type (:foreground ,base0D_t))))
   `(diredp-compressed-file-suffix ((,g_type (:foreground ,base0D_g))))

   `(diredp-dir-heading ((t (:foreground nil :background nil :inherit heading))))
   `(diredp-dir-priv ((,t_type (:foreground ,base0C_t :background nil))))
   `(diredp-dir-priv ((,g_type (:foreground ,base0C_g :background nil))))

   `(diredp-exec-priv ((,t_type (:foreground ,base0D_t :background nil))))
   `(diredp-exec-priv ((,g_type (:foreground ,base0D_g :background nil))))

   `(diredp-executable-tag ((,t_type (:foreground ,base08_t :background nil))))
   `(diredp-executable-tag ((,g_type (:foreground ,base08_g :background nil))))

   `(diredp-file-name ((,t_type (:foreground ,base0A_t))))
   `(diredp-file-name ((,g_type (:foreground ,base0A_g))))

   `(diredp-file-suffix ((,t_type (:foreground ,base0B_t))))
   `(diredp-file-suffix ((,g_type (:foreground ,base0B_g))))

   `(diredp-flag-mark-line ((t (:background nil :inherit highlight))))

   `(diredp-ignored-file-name ((,t_type (:foreground ,base04_t))))
   `(diredp-ignored-file-name ((,g_type (:foreground ,base04_g))))

   `(diredp-link-priv ((,t_type (:background nil :foreground ,base0E_t))))
   `(diredp-link-priv ((,g_type (:background nil :foreground ,base0E_g))))

   `(diredp-mode-line-flagged ((,t_type (:foreground ,base08_t))))
   `(diredp-mode-line-flagged ((,g_type (:foreground ,base08_g))))

   `(diredp-mode-line-marked ((,t_type (:foreground ,base0B_t))))
   `(diredp-mode-line-marked ((,g_type (:foreground ,base0B_g))))

   `(diredp-no-priv ((t (:background nil))))

   `(diredp-number ((,t_type (:foreground ,base0A_t))))
   `(diredp-number ((,g_type (:foreground ,base0A_g))))

   `(diredp-other-priv ((,t_type (:background nil :foreground ,base0E_t))))
   `(diredp-other-priv ((,g_type (:background nil :foreground ,base0E_g))))

   `(diredp-rare-priv ((,t_type (:foreground ,base08_t :background nil))))
   `(diredp-rare-priv ((,g_type (:foreground ,base08_g :background nil))))

   `(diredp-read-priv ((,t_type (:foreground ,base0B_t :background nil))))
   `(diredp-read-priv ((,g_type (:foreground ,base0B_g :background nil))))

   `(diredp-symlink ((,t_type (:foreground ,base0E_t))))
   `(diredp-symlink ((,g_type (:foreground ,base0E_g))))

   `(diredp-write-priv ((,t_type (:foreground ,base0A_t :background nil))))
   `(diredp-write-priv ((,g_type (:foreground ,base0A_g :background nil))))

   ;; term and ansi-term
   `(term-color-black ((,t_type (:foreground ,base02_t :background ,base00_t))))
   `(term-color-black ((,g_type (:foreground ,base02_g :background ,base00_g))))

   `(term-color-white ((,t_type (:foreground ,base05_t :background ,base07_t))))
   `(term-color-white ((,g_type (:foreground ,base05_g :background ,base07_g))))

   `(term-color-red ((,t_type (:foreground ,base08_t :background ,base08_t))))
   `(term-color-red ((,g_type (:foreground ,base08_g :background ,base08_g))))

   `(term-color-yellow ((,t_type (:foreground ,base0A_t :background ,base0A_t))))
   `(term-color-yellow ((,g_type (:foreground ,base0A_g :background ,base0A_g))))

   `(term-color-green ((,t_type (:foreground ,base0B_t :background ,base0B_t))))
   `(term-color-green ((,g_type (:foreground ,base0B_g :background ,base0B_g))))

   `(term-color-cyan ((,t_type (:foreground ,base0C_t :background ,base0C_t))))
   `(term-color-cyan ((,g_type (:foreground ,base0C_g :background ,base0C_g))))

   `(term-color-blue ((,t_type (:foreground ,base0D_t :background ,base0D_t))))
   `(term-color-blue ((,g_type (:foreground ,base0D_g :background ,base0D_g))))

   `(term-color-magenta ((,t_type (:foreground ,base0E_t :background ,base0E_t))))
   `(term-color-magenta ((,g_type (:foreground ,base0E_g :background ,base0E_g))))


   ;; Magit (a patch is pending in magit to make these standard upstream)
   `(magit-branch ((,t_type (:foreground ,base0B_t))))
   `(magit-branch ((,g_type (:foreground ,base0B_g))))

   `(magit-header ((t (:inherit nil :weight bold))))

   `(magit-item-highlight ((t (:inherit highlight :background nil))))

   `(magit-log-graph ((,t_type (:foreground ,base04_t))))
   `(magit-log-graph ((,g_type (:foreground ,base04_g))))

   `(magit-log-sha1 ((,t_type (:foreground ,base0E_t))))
   `(magit-log-sha1 ((,g_type (:foreground ,base0E_g))))

   `(magit-log-head-label-bisect-bad ((,t_type (:foreground ,base08_t))))
   `(magit-log-head-label-bisect-bad ((,g_type (:foreground ,base08_g))))

   `(magit-log-head-label-bisect-good ((,t_type (:foreground ,base0B_t))))
   `(magit-log-head-label-bisect-good ((,g_type (:foreground ,base0B_g))))

   `(magit-log-head-label-default ((,t_type (:foreground ,base0A_t :box nil :weight bold))))
   `(magit-log-head-label-default ((,g_type (:foreground ,base0A_g :box nil :weight bold))))

   `(magit-log-head-label-local ((,t_type (:foreground ,base0D_t))))
   `(magit-log-head-label-local ((,g_type (:foreground ,base0D_g))))

   `(magit-log-head-label-remote ((,t_type (:foreground ,base0B_t))))
   `(magit-log-head-label-remote ((,g_type (:foreground ,base0B_g))))

   `(magit-log-head-label-tags ((,t_type (:foreground ,base0C_t :box nil :weight bold))))
   `(magit-log-head-label-tags ((,g_type (:foreground ,base0C_g :box nil :weight bold))))

   `(magit-section-title ((t (:inherit diff-hunk-header))))

   `(link ((t (:foreground nil :underline t))))

   `(widget-button ((t (:underline t))))

   `(widget-field ((,t_type (:background ,base03_t :box (:line-width 1 :color ,base06_t)))))
   `(widget-field ((,g_type (:background ,base03_g :box (:line-width 1 :color ,base06_g)))))

   ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
   `(compilation-column-number ((,t_type (:foreground ,base0A_t))))
   `(compilation-column-number ((,g_type (:foreground ,base0A_g))))

   `(compilation-line-number ((,t_type (:foreground ,base0A_t))))
   `(compilation-line-number ((,g_type (:foreground ,base0A_g))))

   `(compilation-message-face ((,t_type (:foreground ,base0D_t))))
   `(compilation-message-face ((,g_type (:foreground ,base0D_g))))

   `(compilation-mode-line-exit ((,t_type (:foreground ,base0B_t))))
   `(compilation-mode-line-exit ((,g_type (:foreground ,base0B_g))))

   `(compilation-mode-line-fail ((,t_type (:foreground ,base08_t))))
   `(compilation-mode-line-fail ((,g_type (:foreground ,base08_g))))

   `(compilation-mode-line-run ((,t_type (:foreground ,base0D_t))))
   `(compilation-mode-line-run ((,g_type (:foreground ,base0D_g))))

   ;; Grep
   `(grep-context-face ((,t_type (:foreground ,base04_t))))
   `(grep-context-face ((,g_type (:foreground ,base04_g))))

   `(grep-error-face ((,t_type (:foreground ,base08_t :weight bold :underline t))))
   `(grep-error-face ((,g_type (:foreground ,base08_g :weight bold :underline t))))

   `(grep-hit-face ((,t_type (:foreground ,base0D_t))))
   `(grep-hit-face ((,g_type (:foreground ,base0D_g))))

   `(grep-match-face ((t (:foreground nil :background nil :inherit match))))

   `(regex-tool-matched-face ((t (:foreground nil :background nil :inherit match))))

   ;; mark-multiple
   `(mm/master-face ((t (:inherit region :foreground nil :background nil))))
   `(mm/mirror-face ((t (:inherit region :foreground nil :background nil))))

   ;; org-mode
   `(org-agenda-structure ((,t_type (:foreground ,base0E_t))))
   `(org-agenda-structure ((,g_type (:foreground ,base0E_g))))

   `(org-agenda-date ((,t_type (:foreground ,base0D_t :underline nil))))
   `(org-agenda-date ((,g_type (:foreground ,base0D_g :underline nil))))

   `(org-agenda-done ((,t_type (:foreground ,base0B_t))))
   `(org-agenda-done ((,g_type (:foreground ,base0B_g))))

   `(org-agenda-dimmed-todo-face ((,t_type (:foreground ,base04_t))))
   `(org-agenda-dimmed-todo-face ((,g_type (:foreground ,base04_g))))

   `(org-block ((,t_type (:foreground ,base09_t))))
   `(org-block ((,g_type (:foreground ,base09_g))))

   `(org-code ((,t_type (:foreground ,base0A_t))))
   `(org-code ((,g_type (:foreground ,base0A_g))))

   `(org-column ((,t_type (:background ,base03_t))))
   `(org-column ((,g_type (:background ,base03_g))))

   `(org-column-title ((t (:inherit org-column :weight bold :underline t))))

   `(org-date ((,t_type (:foreground ,base0E_t :underline t))))
   `(org-date ((,g_type (:foreground ,base0E_g :underline t))))

   `(org-document-info ((,t_type (:foreground ,base0C_t))))
   `(org-document-info ((,g_type (:foreground ,base0C_g))))

   `(org-document-info-keyword ((,t_type (:foreground ,base0B_t))))
   `(org-document-info-keyword ((,g_type (:foreground ,base0B_g))))

   `(org-document-title ((,t_type (:weight bold :foreground ,base09_t :height 1.44))))
   `(org-document-title ((,g_type (:weight bold :foreground ,base09_g :height 1.44))))

   `(org-done ((,t_type (:foreground ,base0B_t))))
   `(org-done ((,g_type (:foreground ,base0B_g))))

   `(org-ellipsis ((,t_type (:foreground ,base04_t))))
   `(org-ellipsis ((,g_type (:foreground ,base04_g))))

   `(org-footnote ((,t_type (:foreground ,base0C_t))))
   `(org-footnote ((,g_type (:foreground ,base0C_g))))

   `(org-formula ((,t_type (:foreground ,base08_t))))
   `(org-formula ((,g_type (:foreground ,base08_g))))

   `(org-hide ((,t_type (:foreground ,base03_t))))
   `(org-hide ((,g_type (:foreground ,base03_g))))

   `(org-link ((,t_type (:foreground ,base0D_t))))
   `(org-link ((,g_type (:foreground ,base0D_g))))

   `(org-scheduled ((,t_type (:foreground ,base0B_t))))
   `(org-scheduled ((,g_type (:foreground ,base0B_g))))

   `(org-scheduled-previously ((,t_type (:foreground ,base09_t))))
   `(org-scheduled-previously ((,g_type (:foreground ,base09_g))))

   `(org-scheduled-today ((,t_type (:foreground ,base0B_t))))
   `(org-scheduled-today ((,g_type (:foreground ,base0B_g))))

   `(org-special-keyword ((,t_type (:foreground ,base09_t))))
   `(org-special-keyword ((,g_type (:foreground ,base09_g))))

   `(org-table ((,t_type (:foreground ,base0E_t))))
   `(org-table ((,g_type (:foreground ,base0E_g))))

   `(org-todo ((,t_type (:foreground ,base08_t))))
   `(org-todo ((,g_type (:foreground ,base08_g))))

   `(org-upcoming-deadline ((,t_type (:foreground ,base09_t))))
   `(org-upcoming-deadline ((,g_type (:foreground ,base09_g))))

   `(org-warning ((,t_type (:weight bold :foreground ,base08_t))))
   `(org-warning ((,g_type (:weight bold :foreground ,base08_g))))

   `(markdown-url-face ((t (:inherit link))))

   `(markdown-link-face ((,t_type (:foreground ,base0D_t :underline t))))
   `(markdown-link-face ((,g_type (:foreground ,base0D_g :underline t))))

   `(hl-sexp-face ((,t_type (:background ,base03_t))))
   `(hl-sexp-face ((,g_type (:background ,base03_g))))

   `(highlight-80+ ((,t_type (:background ,base03_t))))
   `(highlight-80+ ((,g_type (:background ,base03_g))))

   ;; Python-specific overrides
   `(py-builtins-face ((,t_type (:foreground ,base09_t :weight normal))))
   `(py-builtins-face ((,g_type (:foreground ,base09_g :weight normal))))

   ;; js2-mode
   `(js2-warning-face ((,t_type (:underline ,base09_t))))
   `(js2-warning-face ((,g_type (:underline ,base09_g))))

   `(js2-error-face ((,t_type (:foreground nil :underline ,base08_t))))
   `(js2-error-face ((,g_type (:foreground nil :underline ,base08_g))))

   `(js2-external-variable-face ((,t_type (:foreground ,base0E_t))))
   `(js2-external-variable-face ((,g_type (:foreground ,base0E_g))))

   `(js2-function-param-face ((,t_type (:foreground ,base0D_t))))
   `(js2-function-param-face ((,g_type (:foreground ,base0D_g))))

   `(js2-instance-member-face ((,t_type (:foreground ,base0D_t))))
   `(js2-instance-member-face ((,g_type (:foreground ,base0D_g))))

   `(js2-private-function-call-face ((,t_type (:foreground ,base08_t))))
   `(js2-private-function-call-face ((,g_type (:foreground ,base08_g))))


   ;; js3-mode
   `(js3-warning-face ((,t_type (:underline ,base09_t))))
   `(js3-warning-face ((,g_type (:underline ,base09_g))))

   `(js3-error-face ((,t_type (:foreground nil :underline ,base08_t))))
   `(js3-error-face ((,g_type (:foreground nil :underline ,base08_g))))

   `(js3-external-variable-face ((,t_type (:foreground ,base0E_t))))
   `(js3-external-variable-face ((,g_type (:foreground ,base0E_g))))

   `(js3-function-param-face ((,t_type (:foreground ,base0D_t))))
   `(js3-function-param-face ((,g_type (:foreground ,base0D_g))))

   `(js3-jsdoc-tag-face ((,t_type (:foreground ,base09_t))))
   `(js3-jsdoc-tag-face ((,g_type (:foreground ,base09_g))))

   `(js3-jsdoc-type-face ((,t_type (:foreground ,base0C_t))))
   `(js3-jsdoc-type-face ((,g_type (:foreground ,base0C_g))))

   `(js3-jsdoc-value-face ((,t_type (:foreground ,base0A_t))))
   `(js3-jsdoc-value-face ((,g_type (:foreground ,base0A_g))))

   `(js3-jsdoc-html-tag-name-face ((,t_type (:foreground ,base0D_t))))
   `(js3-jsdoc-html-tag-name-face ((,g_type (:foreground ,base0D_g))))

   `(js3-jsdoc-html-tag-delimiter-face ((,t_type (:foreground ,base0B_t))))
   `(js3-jsdoc-html-tag-delimiter-face ((,g_type (:foreground ,base0B_g))))

   `(js3-instance-member-face ((,t_type (:foreground ,base0D_t))))
   `(js3-instance-member-face ((,g_type (:foreground ,base0D_g))))

   `(js3-private-function-call-face ((,t_type (:foreground ,base08_t))))
   `(js3-private-function-call-face ((,g_type (:foreground ,base08_g))))

   ;; nxml
   `(nxml-name-face ((t (:foreground unspecified :inherit font-lock-constant-face))))
   `(nxml-attribute-local-name-face ((t (:foreground unspecified :inherit font-lock-variable-name-face))))
   `(nxml-ref-face ((t (:foreground unspecified :inherit font-lock-preprocessor-face))))
   `(nxml-delimiter-face ((t (:foreground unspecified :inherit font-lock-keyword-face))))
   `(nxml-delimited-data-face ((t (:foreground unspecified :inherit font-lock-string-face))))

   `(rng-error-face ((,t_type (:underline ,base08_t))))
   `(rng-error-face ((,g_type (:underline ,base08_g))))


   ;; RHTML
   `(erb-delim-face ((,t_type (:background ,base03_t))))
   `(erb-delim-face ((,g_type (:background ,base03_g))))

   `(erb-exec-face ((,t_type (:background ,base03_t :weight bold))))
   `(erb-exec-face ((,g_type (:background ,base03_g :weight bold))))

   `(erb-exec-delim-face ((,t_type (:background ,base03_t))))
   `(erb-exec-delim-face ((,g_type (:background ,base03_g))))

   `(erb-out-face ((,t_type (:background ,base03_t :weight bold))))
   `(erb-out-face ((,g_type (:background ,base03_g :weight bold))))

   `(erb-out-delim-face ((,t_type (:background ,base03_t))))
   `(erb-out-delim-face ((,g_type (:background ,base03_g))))

   `(erb-comment-face ((,t_type (:background ,base03_t :weight bold :slant italic))))
   `(erb-comment-face ((,g_type (:background ,base03_g :weight bold :slant italic))))

   `(erb-comment-delim-face ((,t_type (:background ,base03_t))))
   `(erb-comment-delim-face ((,g_type (:background ,base03_g))))

   ;; Message-mode
   `(message-header-other ((t (:foreground nil :background nil :weight normal))))

   `(message-header-subject ((,t_type (:inherit message-header-other :weight bold :foreground ,base0A_t))))
   `(message-header-subject ((,g_type (:inherit message-header-other :weight bold :foreground ,base0A_g))))

   `(message-header-to ((,t_type (:inherit message-header-other :weight bold :foreground ,base09_t))))
   `(message-header-to ((,g_type (:inherit message-header-other :weight bold :foreground ,base09_g))))

   `(message-header-cc ((t (:inherit message-header-to :foreground nil))))

   `(message-header-name ((,t_type (:foreground ,base0D_t :background nil))))
   `(message-header-name ((,g_type (:foreground ,base0D_g :background nil))))

   `(message-header-newsgroups ((,t_type (:foreground ,base0C_t :background nil :slant normal))))
   `(message-header-newsgroups ((,g_type (:foreground ,base0C_g :background nil :slant normal))))

   `(message-separator ((,t_type (:foreground ,base0E_t))))
   `(message-separator ((,g_type (:foreground ,base0E_g))))

   ;; Jabber
   `(jabber-chat-prompt-local ((,t_type (:foreground ,base0A_t))))
   `(jabber-chat-prompt-local ((,g_type (:foreground ,base0A_g))))

   `(jabber-chat-prompt-foreign ((,t_type (:foreground ,base09_t))))
   `(jabber-chat-prompt-foreign ((,g_type (:foreground ,base09_g))))

   `(jabber-chat-prompt-system ((,t_type (:foreground ,base0A_t :weight bold))))
   `(jabber-chat-prompt-system ((,g_type (:foreground ,base0A_g :weight bold))))

   `(jabber-chat-text-local ((,t_type (:foreground ,base0A_t))))
   `(jabber-chat-text-local ((,g_type (:foreground ,base0A_g))))

   `(jabber-chat-text-foreign ((,t_type (:foreground ,base09_t))))
   `(jabber-chat-text-foreign ((,g_type (:foreground ,base09_g))))

   `(jabber-chat-text-error ((,t_type (:foreground ,base08_t))))
   `(jabber-chat-text-error ((,g_type (:foreground ,base08_g))))

   `(jabber-roster-user-online ((,t_type (:foreground ,base0B_t))))
   `(jabber-roster-user-online ((,g_type (:foreground ,base0B_g))))

   `(jabber-roster-user-xa ((,t_type :foreground ,base04_t)))
   `(jabber-roster-user-xa ((,g_type :foreground ,base04_g)))

   `(jabber-roster-user-dnd ((,t_type :foreground ,base0A_t)))
   `(jabber-roster-user-dnd ((,g_type :foreground ,base0A_g)))

   `(jabber-roster-user-away ((,t_type (:foreground ,base09_t))))
   `(jabber-roster-user-away ((,g_type (:foreground ,base09_g))))

   `(jabber-roster-user-chatty ((,t_type (:foreground ,base0E_t))))
   `(jabber-roster-user-chatty ((,g_type (:foreground ,base0E_g))))

   `(jabber-roster-user-error ((,t_type (:foreground ,base08_t))))
   `(jabber-roster-user-error ((,g_type (:foreground ,base08_g))))

   `(jabber-roster-user-offline ((,t_type (:foreground ,base04_t))))
   `(jabber-roster-user-offline ((,g_type (:foreground ,base04_g))))

   `(jabber-rare-time-face ((,t_type (:foreground ,base04_t))))
   `(jabber-rare-time-face ((,g_type (:foreground ,base04_g))))

   `(jabber-activity-face ((,t_type (:foreground ,base0E_t))))
   `(jabber-activity-face ((,g_type (:foreground ,base0E_g))))

   `(jabber-activity-personal-face ((,t_type (:foreground ,base0C_t))))
   `(jabber-activity-personal-face ((,g_type (:foreground ,base0C_g))))

   ;; Gnus
   `(gnus-cite-1 ((t (:inherit outline-1 :foreground nil))))
   `(gnus-cite-2 ((t (:inherit outline-2 :foreground nil))))
   `(gnus-cite-3 ((t (:inherit outline-3 :foreground nil))))
   `(gnus-cite-4 ((t (:inherit outline-4 :foreground nil))))
   `(gnus-cite-5 ((t (:inherit outline-5 :foreground nil))))
   `(gnus-cite-6 ((t (:inherit outline-6 :foreground nil))))
   `(gnus-cite-7 ((t (:inherit outline-7 :foreground nil))))
   `(gnus-cite-8 ((t (:inherit outline-8 :foreground nil))))
   ;; there are several more -cite- faces...
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))

   `(gnus-header-from ((,t_type (:inherit message-header-other-face :weight bold :foreground ,base09_t))))
   `(gnus-header-from ((,g_type (:inherit message-header-other-face :weight bold :foreground ,base09_g))))

   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-button ((t (:inherit link :foreground nil))))
   `(gnus-signature ((t (:inherit font-lock-comment-face))))

   `(gnus-summary-normal-unread ((,t_type (:foreground ,base0D_t :weight normal))))
   `(gnus-summary-normal-unread ((,g_type (:foreground ,base0D_g :weight normal))))

   `(gnus-summary-normal-read ((,t_type (:foreground ,base06_t :weight normal))))
   `(gnus-summary-normal-read ((,g_type (:foreground ,base06_g :weight normal))))

   `(gnus-summary-normal-ancient ((,t_type (:foreground ,base0C_t :weight normal))))
   `(gnus-summary-normal-ancient ((,g_type (:foreground ,base0C_g :weight normal))))

   `(gnus-summary-normal-ticked ((,t_type (:foreground ,base09_t :weight normal))))
   `(gnus-summary-normal-ticked ((,g_type (:foreground ,base09_g :weight normal))))

   `(gnus-summary-low-unread ((,t_type (:foreground ,base04_t :weight normal))))
   `(gnus-summary-low-unread ((,g_type (:foreground ,base04_g :weight normal))))

   `(gnus-summary-low-read ((,t_type (:foreground ,base04_t :weight normal))))
   `(gnus-summary-low-read ((,g_type (:foreground ,base04_g :weight normal))))

   `(gnus-summary-low-ancient ((,t_type (:foreground ,base04_t :weight normal))))
   `(gnus-summary-low-ancient ((,g_type (:foreground ,base04_g :weight normal))))

   `(gnus-summary-high-unread ((,t_type (:foreground ,base0A_t :weight normal))))
   `(gnus-summary-high-unread ((,g_type (:foreground ,base0A_g :weight normal))))

   `(gnus-summary-high-read ((,t_type (:foreground ,base0B_t :weight normal))))
   `(gnus-summary-high-read ((,g_type (:foreground ,base0B_g :weight normal))))

   `(gnus-summary-high-ancient ((,t_type (:foreground ,base0B_t :weight normal))))
   `(gnus-summary-high-ancient ((,g_type (:foreground ,base0B_g :weight normal))))

   `(gnus-summary-high-ticked ((,t_type (:foreground ,base09_t :weight normal))))
   `(gnus-summary-high-ticked ((,g_type (:foreground ,base09_g :weight normal))))

   `(gnus-summary-cancelled ((,t_type (:foreground ,base08_t :background nil :weight normal))))
   `(gnus-summary-cancelled ((,g_type (:foreground ,base08_g :background nil :weight normal))))

   `(gnus-group-mail-low ((,t_type (:foreground ,base04_t))))
   `(gnus-group-mail-low ((,g_type (:foreground ,base04_g))))

   `(gnus-group-mail-low-empty ((,t_type (:foreground ,base04_t))))
   `(gnus-group-mail-low-empty ((,g_type (:foreground ,base04_g))))

   `(gnus-group-mail-1 ((t (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-mail-2 ((t (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-mail-3 ((t (:foreground nil :weight normal :inherit outline-3))))
   `(gnus-group-mail-4 ((t (:foreground nil :weight normal :inherit outline-4))))
   `(gnus-group-mail-5 ((t (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-mail-6 ((t (:foreground nil :weight normal :inherit outline-6))))

   `(gnus-group-mail-1-empty ((,t_type (:inherit gnus-group-mail-1 :foreground ,base04_t))))
   `(gnus-group-mail-1-empty ((,g_type (:inherit gnus-group-mail-1 :foreground ,base04_g))))

   `(gnus-group-mail-2-empty ((,t_type (:inherit gnus-group-mail-2 :foreground ,base04_t))))
   `(gnus-group-mail-2-empty ((,g_type (:inherit gnus-group-mail-2 :foreground ,base04_g))))

   `(gnus-group-mail-3-empty ((,t_type (:inherit gnus-group-mail-3 :foreground ,base04_t))))
   `(gnus-group-mail-3-empty ((,g_type (:inherit gnus-group-mail-3 :foreground ,base04_g))))

   `(gnus-group-mail-4-empty ((,t_type (:inherit gnus-group-mail-4 :foreground ,base04_t))))
   `(gnus-group-mail-4-empty ((,g_type (:inherit gnus-group-mail-4 :foreground ,base04_g))))

   `(gnus-group-mail-5-empty ((,t_type (:inherit gnus-group-mail-5 :foreground ,base04_t))))
   `(gnus-group-mail-5-empty ((,g_type (:inherit gnus-group-mail-5 :foreground ,base04_g))))

   `(gnus-group-mail-6-empty ((,t_type (:inherit gnus-group-mail-6 :foreground ,base04_t))))
   `(gnus-group-mail-6-empty ((,g_type (:inherit gnus-group-mail-6 :foreground ,base04_g))))

   `(gnus-group-news-1 ((t (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-news-2 ((t (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-news-3 ((t (:foreground nil :weight normal :inherit outline-7))))
   `(gnus-group-news-4 ((t (:foreground nil :weight normal :inherit outline-8))))
   `(gnus-group-news-5 ((t (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-news-6 ((t (:foreground nil :weight normal :inherit outline-2))))

   `(gnus-group-news-1-empty ((,t_type (:inherit gnus-group-news-1 :foreground ,base04_t))))
   `(gnus-group-news-1-empty ((,g_type (:inherit gnus-group-news-1 :foreground ,base04_g))))

   `(gnus-group-news-2-empty ((,t_type (:inherit gnus-group-news-2 :foreground ,base04_t))))
   `(gnus-group-news-2-empty ((,g_type (:inherit gnus-group-news-2 :foreground ,base04_g))))

   `(gnus-group-news-3-empty ((,t_type (:inherit gnus-group-news-3 :foreground ,base04_t))))
   `(gnus-group-news-3-empty ((,g_type (:inherit gnus-group-news-3 :foreground ,base04_g))))

   `(gnus-group-news-4-empty ((,t_type (:inherit gnus-group-news-4 :foreground ,base04_t))))
   `(gnus-group-news-4-empty ((,g_type (:inherit gnus-group-news-4 :foreground ,base04_g))))

   `(gnus-group-news-5-empty ((,t_type (:inherit gnus-group-news-5 :foreground ,base04_t))))
   `(gnus-group-news-5-empty ((,g_type (:inherit gnus-group-news-5 :foreground ,base04_g))))

   `(gnus-group-news-6-empty ((,t_type (:inherit gnus-group-news-6 :foreground ,base04_t))))
   `(gnus-group-news-6-empty ((,g_type (:inherit gnus-group-news-6 :foreground ,base04_g))))

   `(erc-direct-msg-face ((,t_type (:foreground ,base09_t))))
   `(erc-direct-msg-face ((,g_type (:foreground ,base09_g))))

   `(erc-error-face ((,t_type (:foreground ,base08_t))))
   `(erc-error-face ((,g_type (:foreground ,base08_g))))

   `(erc-header-face ((,t_type (:foreground ,base06_t :background ,base04_t))))
   `(erc-header-face ((,g_type (:foreground ,base06_g :background ,base04_g))))

   `(erc-input-face ((,t_type (:foreground ,base0B_t))))
   `(erc-input-face ((,g_type (:foreground ,base0B_g))))

   `(erc-keyword-face ((,t_type (:foreground ,base0A_t))))
   `(erc-keyword-face ((,g_type (:foreground ,base0A_g))))

   `(erc-current-nick-face ((,t_type (:foreground ,base0B_t))))
   `(erc-current-nick-face ((,g_type (:foreground ,base0B_g))))

   `(erc-my-nick-face ((,t_type (:foreground ,base0B_t))))
   `(erc-my-nick-face ((,g_type (:foreground ,base0B_g))))

   `(erc-nick-default-face ((,t_type (:weight normal :foreground ,base0E_t))))
   `(erc-nick-default-face ((,g_type (:weight normal :foreground ,base0E_g))))

   `(erc-nick-msg-face ((,t_type (:weight normal :foreground ,base0A_t))))
   `(erc-nick-msg-face ((,g_type (:weight normal :foreground ,base0A_g))))

   `(erc-notice-face ((,t_type (:foreground ,base04_t))))
   `(erc-notice-face ((,g_type (:foreground ,base04_g))))

   `(erc-pal-face ((,t_type (:foreground ,base09_t))))
   `(erc-pal-face ((,g_type (:foreground ,base09_g))))

   `(erc-prompt-face ((,t_type (:foreground ,base0D_t))))
   `(erc-prompt-face ((,g_type (:foreground ,base0D_g))))

   `(erc-timestamp-face ((,t_type (:foreground ,base0C_t))))
   `(erc-timestamp-face ((,g_type (:foreground ,base0C_g))))

   `(custom-variable-tag ((,t_type (:foreground ,base0D_t))))
   `(custom-variable-tag ((,g_type (:foreground ,base0D_g))))

   `(custom-group-tag ((,t_type (:foreground ,base0D_t))))
   `(custom-group-tag ((,g_type (:foreground ,base0D_g))))

   `(custom-state ((,t_type (:foreground ,base0B_t))))
   `(custom-state ((,g_type (:foreground ,base0B_g)))))

  (custom-theme-set-variables
   'base16-ocean-dual

   `(ansi-color-names-vector
     ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
     [,base00_t ,base08_t ,base0B_t ,base0A_t ,base0D_t ,base0E_t ,base0D_t ,base05_t])
   `(when (not (facep (aref ansi-term-color-vector 0)))
      (ansi-term-color-vector
       ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
       [base00_t ,base00_t ,base08_t ,base0B_t ,base0A_t ,base0D_t ,base0E_t ,base0D_t ,base05_t]))))

(provide-theme 'base16-ocean-dual)

;;; base16-ocean-dual-theme.el ends here
