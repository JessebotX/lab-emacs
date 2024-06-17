;;; init.el -*- lexical-binding: t; -*-

;;; Preface
(dolist (path '("lisp/"))
  (add-to-list 'load-path (expand-file-name path user-emacs-directory)))

;;; Straight.el Package Manager
(require 'my-config-straight-bootstrap)

;;; No Littering
(use-package no-littering
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-etc-directory)))
  (no-littering-theme-backups))

;;; Initial Settings
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(setq auto-window-vscroll nil)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default compile-command "make")

(setopt ad-redefinition-action 'accept)
(setopt backward-delete-char-untabify-method 'hungry)
(setopt create-lockfiles nil)
(setopt custom-file (make-temp-file "emacs-custom-"))
(setopt display-line-numbers-type 'relative)
(setopt display-line-numbers-width 3)
(setopt fast-but-imprecise-scrolling t)
(setopt global-auto-revert-non-file-buffers t)
(setopt isearch-lazy-count t)
(setopt lazy-count-prefix-format nil)
(setopt lazy-count-suffix-format "   (%s/%s)")
(setopt isearch-lazy-count t)
(setopt require-final-newline t)
(setopt scroll-conservatively 101)
(setopt scroll-preserve-screen-position t)
(setopt uniquify-buffer-name-style 'forward)
(setopt uniquify-ignore-buffers-re "^\\*")
(setopt uniquify-separator "/")
(setopt use-short-answers t)
(setopt visible-bell nil)
(setopt whitespace-style '(face tabs tab-mark trailing))
(setopt whitespace-display-mappings '((tab-mark 9 [#x21e5 9] [92 9])))
(setopt word-wrap nil)

(blink-cursor-mode -1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(global-so-long-mode 1)
(save-place-mode 1)
(winner-mode 1)
(recentf-mode 1)

(add-hook 'text-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (whitespace-mode 1)
            (display-line-numbers-mode 1)))

;;; Keybindings
;;;; Base keybind config
(global-set-key [remap list-buffers] 'ibuffer)
(keymap-global-set "<escape>" 'keyboard-escape-quit)
(keymap-global-set "C--" 'text-scale-decrease)
(keymap-global-set "C-=" 'text-scale-increase)
(keymap-global-set "C-+" 'text-scale-increase)
(keymap-global-set "M-]" 'forward-paragraph)
(keymap-global-set "M-[" 'backward-paragraph)

(advice-add #'split-window-below :after (lambda (&rest _) (other-window 1))) ; Jump to new window on split
(advice-add #'split-window-right :after (lambda (&rest _) (other-window 1))) ; Jump to new window on split

;;;; hydra
(use-package hydra
  :defer t)

;;;;; Evil mode
(use-package evil
  :init
  (setopt evil-want-integration t)
  (setopt evil-want-keybinding nil)
  (setopt evil-want-C-i-jump t)
  (setopt evil-respect-visual-line-mode t)
  (setopt evil-want-C-h-delete t)
  (setopt evil-want-C-u-scroll t)
  (setopt evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)

  (evil-select-search-module 'evil-search-module 'evil-search)
  (keymap-set evil-insert-state-map "C-g" 'evil-normal-state)
  ;; Rebind `universal-argument' since 'C-u' now scrolls the buffer
  (keymap-global-set "C-M-u" 'universal-argument)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Make sure some modes start in Emacs state
  (dolist (mode '(custom-mode
                  eshell-mode
                  term-mode
                  eat-mode
                  vterm-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; evil mode visual indentation
  (defun gemacs/evil-shift-right ()
    (interactive)
    (evil-shift-right evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))

  (defun gemacs/evil-shift-left ()
    (interactive)
    (evil-shift-left evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))
  (evil-define-key 'visual global-map (kbd ">") 'gemacs/evil-shift-right)
  (evil-define-key 'visual global-map (kbd "<") 'gemacs/evil-shift-left))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :config
  (keymap-global-set "M-;" 'evilnc-comment-or-uncomment-lines))

;;;; General
(use-package general
  :after evil
  :config
  (general-create-definer gemacs/define-leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (keymap-global-set "C-," 'set-mark-command)

  (gemacs/define-leader-key
    "h" '(nil :which-key "Help")
    "k" '(nil :which-key "Buffer/Window killing")
    "l" '(nil :which-key "Transform Text")
    "o" '(nil :which-key "Applications + ace-window")
    "t" '(nil :which-key "Modes and Themes")
    "w" '(nil :which-key "Window"))

  (gemacs/define-leader-key
    "." 'find-file
    "qe" 'eval-last-sexp
    "SPC" 'cycle-spacing
    "C-," 'set-mark-command
    "=" 'count-words
    "-" 'kill-this-buffer
    "fj" 'next-error
    "fk" 'previous-error
    "ff" 'gemacs/next-previous-error-hydra/body
    "lf" 'downcase-dwim
    "lj" 'capitalize-dwim
    "cc" 'compile
    "cr" 'recompile
    "cp" 'project-compile
    "do" 'dictionary-lookup-definition
    "ho" 'describe-symbol
    "hf" 'describe-face
    "hk" 'describe-key
    "j" 'execute-extended-command
    "oa" 'org-agenda
    "nc" 'org-capture
    "kjj" 'kill-buffer-and-window
    "k SPC" 'kill-buffer
    "kl" 'delete-other-windows
    "kk" 'delete-window
    "mf" 'make-frame
    "qe" 'eval-last-sexp
    "tn" 'display-line-numbers-mode
    "twv" 'variable-pitch-mode
    "us" 'scratch-buffer
    "wj" 'winner-undo
    "wk" 'winner-redo)

  ;; unbind SPC in help-mode
  (evil-define-key '(normal emacs) help-mode-map (kbd "SPC") nil))

;;;; ace-window
(use-package ace-window
  :bind ("C-x o" . ace-window)
  :init
  (gemacs/define-leader-key
    "oo" 'ace-window
    "wo" 'ace-window))

;;;;; Pulse
(use-package pulse
  :straight nil
  :preface
  (defun gemacs--pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  :config
  ;; Default
  (dolist (command '(scroll-up-command
                     scroll-down-command
                     windmove-left
                     windmove-right
                     windmove-down
                     windmove-up
                     forward-paragraph
                     backward-paragraph
                     move-to-window-line-top-bottom
                     recenter-top-bottom
                     other-window))
    (advice-add command :after #'gemacs--pulse-line))

  (with-eval-after-load 'org
    (dolist (command '(org-forward-element
                       org-backward-element
                       org-forward-paragraph
                       org-backward-paragraph
                       org-forward-sentence
                       org-backward-sentence
                       org-forward-heading-same-level
                       org-backward-heading-same-level))
      (advice-add command :after #'gemacs--pulse-line)))

  (with-eval-after-load 'markdown-mode
    (dolist (command '(markdown-forward-block
                       markdown-backward-block
                       markdown-forward-paragraph
                       markdown-backward-paragraph
                       markdown-backward-page
                       markdown-forward-page))
      (advice-add command :after #'gemacs--pulse-line)))

  (with-eval-after-load 'evil
    (dolist (command '(evil-forward-paragraph
                       evil-backward-paragraph
                       evil-scroll-up
                       evil-scroll-down
                       evil-ex-search-next
                       evil-ex-search-previous
                       evil-goto-line))
      (advice-add command :after #'gemacs--pulse-line)))

  (with-eval-after-load 'ace-window
    (dolist (command '(ace-window))
      (advice-add command :after #'gemacs--pulse-line))))

;;; User Interface
;;;; Theme
(defun gemacs/set-theme (theme)
  "Set emacs current color theme to THEME.

THEME is a string matching its symbol name.
e.g. \"tango-dark\" => 'tango-dark"
  (interactive (list (completing-read "Set theme: " (custom-available-themes))))
  (mapc 'disable-theme custom-enabled-themes)
  (load-theme (intern theme) t)
  (enable-theme (intern theme)))

(use-package modus-themes
  :init
  (keymap-global-set "<f5>" 'modus-themes-toggle)
  :custom
  (modus-themes-to-toggle '(modus-operandi modus-vivendi))
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-common-palette-overrides
   '((fg-line-number-inactive "gray50")
     (fg-line-number-active red-cooler)
     (bg-line-number-inactive unspecified)
     (bg-line-number-active unspecified)
     (border-mode-line-active bg-dim)
     (bg-mode-line-active bg)
     (bg-mode-line-inactive bg))))

(use-package adwaita-dark-theme)

(use-package ef-themes
  :custom
  (ef-themes-to-toggle '(ef-bio ef-frost)))

(use-package catppuccin-theme
  :custom
  (catppuccin-flavor 'mocha))

(use-package doom-themes)

(gemacs/set-theme "modus-operandi-tinted")

;;;; Fonts
(defcustom gemacs/default-font-size 120
  "Default font size (face height)")

;;;; Icons
(use-package nerd-icons)

;;;; Fontaine (Font settings)
(when (display-graphic-p)
  (use-package fontaine
    :init
    (gemacs/define-leader-key
      "tf" 'fontaine-set-preset)
    (add-hook 'fontaine-set-preset-hook
              (lambda ()
                (set-fontset-font
                 t
                 (if (version< emacs-version "28.1")
                     '(#x1f300 . #x1fad0)
                   'emoji)
                 (cond
                  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
                  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
                  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
                  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
                  ((member "Symbola" (font-family-list)) "Symbola")))
                ;; Cascadia Code symbols: ●
                (set-fontset-font t '(#x25e6 . #x25e6) "Cascadia Code")
                (set-fontset-font t '(#x25ce . #x25ce) "Cascadia Code")
                (set-fontset-font t '(#x25cf . #x25cf) "Cascadia Code")
                (set-fontset-font t '(#x25c9 . #x25c9) "Cascadia Code")
                (set-fontset-font t '(#x25cb . #x25cb) "Cascadia Code")
                (set-fontset-font t '(#x25cf . #x25cf) "Cascadia Code")))
    (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
    :custom
    (fontaine-presets
     `((regular)
       (cascadia-code
        :default-family "Cascadia Code"
        :fixed-pitch-family "Cascadia Code")
       (poppins-variable-pitch
        :variable-pitch-family "Poppins")
       (urbanist-variable-pitch
        :variable-pitch-family "Urbanist")
       (outfit-variable-pitch
        :variable-pitch-family "Outfit")
       (tengwar-cursive-variable-pitch
        :variable-pitch-family "Tengwar Cursive")
       (julia-mono
        :default-family "JuliaMono")
       (jetbrains-mono
        :default-family "JetBrains Mono")
       (ubuntu-mono
        :default-family "Ubuntu Mono"
        :default-height ,(+ gemacs/default-font-size 20)
        :variable-pitch-family "Ubuntu")
       (t
        :default-family "Maple Mono"
        :default-weight regular
        :default-height ,gemacs/default-font-size
        :fixed-pitch-family "JetBrains Mono"
        :fixed-pitch-weight regular
        :variable-pitch-height ,(+ gemacs/default-font-size 30)
        :variable-pitch-family "Spectral")))
    :config
    (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)))

(defun gemacs/font-reset-size ()
  "Reset the default font size (face height) to `gemacs/default-font-size'"
  (interactive)
  (set-face-attribute 'default nil :height gemacs/default-font-size))

(defun gemacs/font-increase-size (&optional increment)
  "Increase default font size (face height) by INCREMENT. If no
INCREMENT is provided, increase by 10."
  (interactive)
  (if increment
      (set-face-attribute 'default nil :height (+ (face-attribute 'default :height) increment))
    (set-face-attribute 'default nil :height (+ (face-attribute 'default :height) 10))))

(defun gemacs/font-decrease-size (&optional decrement)
  "Decrease default font size (face height) by DECREMENT. If no
DECREMENT is provided, increase by 10."
  (interactive)
  (if decrement
      (set-face-attribute 'default nil :height (- (face-attribute 'default :height) decrement))
    (set-face-attribute 'default nil :height (- (face-attribute 'default :height) 10))))

(defhydra gemacs/font-size-hydra (:timeout 4)
  "Increase/decrease/reset current font size."
  ("=" gemacs/font-increase-size "increase")
  ("+" gemacs/font-increase-size "increase")
  ("-" gemacs/font-decrease-size "decrease")
  ("0" gemacs/font-reset-size "reset"))

;;; Mode-line (Modeline)
(setopt display-time-default-load-average nil)
(display-time-mode 1)
(column-number-mode)

;;;; Battery
;; Only display when on a laptop or something
(require 'battery)
(when
    (and battery-status-function
         (not (string-match-p "N/A"  (battery-format "%B" (funcall battery-status-function)))))
  (display-battery-mode 1))

;;;; Doom Modeline (fancy modeline look)
(use-package doom-modeline
  :hook (emacs-startup . doom-modeline-mode)
  :custom
  (doom-modeline-height 35)
  (doom-modeline-modal nil)
  (doom-modeline-enable-word-count t))

;;; Minibuffer / Completion
(defun gemacs--minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name, delete up to parent
folder, otherwise delete a word."
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (kill-word (- arg))))

(let ((map minibuffer-local-map))
  (define-key map (kbd "C-<backspace>") #'gemacs--minibuffer-backward-kill)
  (define-key map (kbd "M-<backspace>") #'gemacs--minibuffer-backward-kill))

(use-package vertico
  :hook (emacs-startup . vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-M-j" . vertico-next-group)
              ("C-M-k" . vertico-previous-group))
  :custom
  (vertico-cycle t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :after vertico
  :config
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (gemacs/define-leader-key
    "b"  'consult-buffer
    "p"  'consult-yank-from-kill-ring
    "tt" 'consult-theme
    "r"  '(nil :which-key "Consult")
    "rs" 'consult-line
    "rr" 'consult-recent-file
    "rb" 'consult-bookmark
    "rm" 'consult-bookmark))

(use-package marginalia
  :hook (vertico-mode . marginalia-mode))

(use-package corfu
  :hook (prog-mode . corfu-mode)
  :bind
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :custom
  (corfu-popupinfo-delay nil)
  (corfu-auto t)
  (corfu-quit-no-match 'separator))

;;; Applications
(use-package magit
  :commands (magit magit-status)
  :init
  (gemacs/define-leader-key
    "og" 'magit))

;;; Utils
;;;; Preface
(defcustom gemacs/terminal "alacritty"
  "Terminal emulator")

(defcustom gemacs/terminal-windows "wt -d"
  "Terminal emulator on Windows systems")

;;;;; Increment/decrement numbers
(defun gemacs/increment-number-at-point ()
  "Increment integer under the cursor."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun gemacs/decrement-number-at-point ()
  "Decrement integer under the cursor."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(keymap-global-set "C-c =" #'gemacs/increment-number-at-point)
(keymap-global-set "C-c -" #'gemacs/decrement-number-at-point)

;;;;; Copying/linking buffers
(defun gemacs/buffer-copy-base-file-name ()
  "Copy the current buffer's file name"
  (interactive)
  (if (buffer-file-name)
      (kill-new
       (concat
        (file-name-base buffer-file-name)
        "."
        (file-name-extension buffer-file-name)))
    (message "Error: (buffer-file-name) returned nil")))

(defun gemacs/buffer-insert-relative-link-to-file (filepath &optional useless)
  "Get a relative link from the file in the current buffer to FILEPATH.

USELESS is not used."
  (interactive (find-file-read-args "Link to file: " (confirm-nonexistent-file-or-buffer)))
  (if (buffer-file-name)
      (insert
       (file-relative-name filepath (file-name-directory buffer-file-name)))
    (message "Error: (buffer-file-name) returned nil")))

;;;;; Open buffer contents in external applications
(defun gemacs/open-in-terminal ()
  "Open the current directory in a new terminal window

Credit: xahlee.info"
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (shell-command (format "%s \"%s\"" gemacs/terminal-windows (expand-file-name default-directory))))
   ((eq system-type 'darwin)
    (shell-command
     (concat "open -a terminal " (shell-quote-argument (expand-file-name default-directory)))))
   (t (call-process shell-file-name nil 0 nil
                    shell-command-switch
                    (format "%s --working-directory '%s'" gemacs/terminal (expand-file-name default-directory))))))

(defun gemacs/open-file ()
  "Open file.

Credit: xahlee.info"
  (interactive)
  (let ((path (if (eq major-mode 'dired-mode)
                  (if (eq nil (dired-get-marked-files))
                      default-directory
                    (car (dired-get-marked-files)))
                (if buffer-file-name
                    buffer-file-name
                  default-directory))))
    (cond
     ((eq system-type 'windows-nt)
      (shell-command
       (format "PowerShell -Command invoke-item '%s'" (expand-file-name path))))
     ((eq system-type 'darwin)
      (shell-command (concat "open -R " (shell-quote-argument path))))
     (t
      (call-process shell-file-name nil 0 nil
                    shell-command-switch
                    (format "xdg-open '%s'" (expand-file-name path)))))))

(defun gemacs/open-current-directory ()
  "Open the current directory"
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (shell-command
     (format "PowerShell -Command invoke-item '%s'" (expand-file-name default-directory))))
   ((eq system-type 'darwin)
    (shell-command
     (concat "open -R " (shell-quote-argument (expand-file-name default-directory)))))
   (t
    (call-process shell-file-name nil 0 nil
                  shell-command-switch
                  (format "xdg-open '%s'" (expand-file-name default-directory))))))

(defun gemacs/kill-ring-clear ()
  "Clear all saved text in the kill ring."
  (interactive)
  (setq kill-ring nil)
  (garbage-collect))

(defun gemacs/directory-consult-grep ()
  (interactive)
  (require 'consult)
  (consult-grep (expand-file-name default-directory)))

(defun gemacs/directory-consult-ripgrep ()
  (interactive)
  (require 'consult)
  (consult-ripgrep (expand-file-name default-directory)))

;;;; Utils keybindings
(gemacs/define-leader-key
  "et" 'gemacs/open-in-terminal
  "of" 'gemacs/open-file
  "od" 'gemacs/open-current-directory)

;;; Programming
;;;; Helpful packages
(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle)
              ([S-tab] . bicycle-cycle-global)))

(use-package outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    #'outline-minor-faces-mode))

(use-package backline
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update))

;; Hungry delete (better backspace deletion)
(use-package hungry-delete
  :hook (emacs-startup . global-hungry-delete-mode)
  :custom
  ;; delete the following on backspace: SPC, TAB, ^M, ^L, ^K
  (hungry-delete-chars-to-skip " 	"))

;; Helpful colors
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Devdocs (offline programming language documentation)
(use-package devdocs
  :commands (devdocs-lookup)
  :bind (("C-h d" . devdocs-lookup))
  :config
  (gemacs/define-leader-key
    "hd" 'devdocs-lookup)

  (evil-define-key '(normal emacs) devdocs-mode-map
    (kbd "SPC") nil))

;; Dumb jump (navigate to code definitions)
(use-package dumb-jump
  :config
  (setopt xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Treemacs file tree explorer
(use-package treemacs
  :commands
  (treemacs
   treemacs-add-and-display-current-project-exclusively
   treemacs-add-and-display-current-project)
  :custom
  (treemacs-width 30)
  :config
  (evil-define-key '(normal emacs) treemacs-mode-map
    "d" 'treemacs-delete-file
    "r" 'treemacs-rename-file
    "o" 'treemacs-create-file
    "gr" 'treemacs-refresh))

;;;; CSS
(defun gemacs--css-mode-setup ()
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)
  (setq-local evil-shift-width 2))

(setopt css-indent-offset 2)

(add-hook 'css-mode-hook #'gemacs--css-mode-setup)

;;;; Emacs lisp
(defun gemacs--lisp-mode-setup ()
  (outline-minor-mode 1)
  (setq-local tab-width 8)
  (setq-local indent-tabs-mode nil)
  (setq-local evil-shift-width 8))

(add-hook 'lisp-mode-hook #'gemacs--lisp-mode-setup)
(add-hook 'emacs-lisp-mode-hook #'gemacs--lisp-mode-setup)

;;;; Git modes (gitignore mode, etc.)
(use-package git-modes
  :defer t)

;;;; Go
(use-package go-mode
  :preface
  (defun gemacs--go-mode-setup ()
    (setq-local compile-command "go build")
    (setq-local tab-width 4)
    (setq-local indent-tabs-mode t)
    (setq-local evil-shift-width 4))
  :mode "\\.go\\'"
  :hook (go-mode . gemacs--go-mode-setup))

;;;; HTML
(setopt sgml-basic-offset 2)

(defun gemacs--html-mode-setup ()
  (display-line-numbers-mode 1)
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)
  (setq-local evil-shift-width 2))
(add-hook 'html-mode-hook #'gemacs--html-mode-setup)

;;;; Python
(defun gemacs--python-mode-setup ()
  (setq-local compile-command "python")
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)
  (setq-local evil-shift-width 4))

(add-hook 'python-mode-hook #'gemacs--python-mode-setup)

;;;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :preface
  (defun gemacs--yaml-mode-setup ()
    (setq-local tab-width 4)
    (setq-local indent-tabs-mode nil)
    (setq-local evil-shift-width 4))
  :config
  (add-hook 'yaml-mode-hook #'gemacs--yaml-mode-setup))

;;; Writing
;;;; Large headings mode
(use-package gemacs-large-headings-mode
  :straight nil
  :init
  :commands (gemacs/large-headings-mode))

;;;; Spell-checker
(when (not (eq system-type 'windows-nt))
  (use-package jinx
    :defer t))

;;;; Dictionary
(use-package dictionary
  :straight nil
  :init
  (gemacs/define-leader-key
    "do"   #'dictionary-lookup-definition)
  :commands (dictionary-lookup-definition)
  :custom
  (dictionary-server "dict.org")
  :config
  (evil-define-key '(normal emacs) dictionary-mode-map
    (kbd "SPC") nil))

;;;; Imenu-list
(use-package imenu-list
  :commands (imenu-list-smart-toggle)
  :bind ("C-'" . imenu-list-smart-toggle)
  :init
  (setq imenu-list-focus-after-activation t))

;;;; Writegood
(use-package writegood-mode
  :defer t)

;;;; Writeroom
(use-package writeroom-mode
  :init
  (gemacs/define-leader-key
    "twbw" 'writeroom-mode)
  :commands (writeroom-mode global-writeroom-mode)
  :custom
  (writeroom-width 80))

;;;; Olivetti (writeroom/zen mode)
(use-package olivetti
  :init
  (gemacs/define-leader-key
    "twbb" 'olivetti-mode)
  :commands (olivetti-mode)
  :hook ((markdown-mode   . olivetti-mode)
         (org-mode        . olivetti-mode)
         (org-agenda-mode . olivetti-mode))
  :custom
  (olivetti-body-width 80))

;;;; Markdown
(use-package edit-indirect
  :defer t)

(use-package markdown-mode
  :preface
  (defun gemacs/markdown--setup ()
    "Settings for `markdown-mode'"
    (setq-local tab-width 2
                indent-tabs-mode nil
                evil-shift-width 2)
    (display-line-numbers-mode -1)
    (visual-line-mode 1))
  :mode "\\.\\(?:md\\|txt\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'"
  :custom
  (markdown-italic-underscore t)
  (markdown-list-indent-width 2)
  (markdown-fontify-code-blocks-natively t)
  (markdown-max-image-size '(512 . 512))
  :config
  (add-hook 'markdown-mode-hook #'gemacs/markdown--setup)
  (add-to-list 'markdown-code-lang-modes '("c" . c-mode))
  (add-to-list 'markdown-code-lang-modes '("yml" . yaml-mode))
  (add-to-list 'markdown-code-lang-modes '("yaml" . yaml-mode))
  (add-to-list 'markdown-code-lang-modes '("toml" . conf-toml-mode))
  (add-to-list 'markdown-code-lang-modes '("go" . go-mode)))

;;;; Org mode
(use-package org
  :straight nil
  :preface
  (defun gemacs--org-mode-setup ()
    "Settings for `org-mode-hook'"
    (setq-local tab-width 8)
    (setq-local evil-shift-width 8)
    (setq-local indent-tabs-mode nil)
    (setq paragraph-start "\\|[    ]*$")
    (setq paragraph-separate "[     ]*$")
    (visual-line-mode 1)
    (display-line-numbers-mode -1)
    (org-indent-mode 1))
  :hook (org-mode . gemacs--org-mode-setup)
  :custom
  (org-id-link-to-org-use-id 'use-existing)
  (org-ellipsis " +")
  (org-auto-align-tags nil)
  (org-src-preserve-indentation t)
  (org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                          (vm-imap . vm-visit-imap-folder-other-frame)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file)
                          (wl . wl-other-frame)))
  (org-link-file-path-type 'relative)
  (org-hide-emphasis-markers t)
  (org-startup-folded 'showall)
  (org-todo-keywords '((sequence
                        "TODO(t)"
                        "WIP(w)"
                        "HOLD(h)"
                        "EDIT(e)"
                        "REDO(r)"
                        "APPOINTMENT(a)"
                        "|" ; separate active and inactive states
                        "ARCHIVED(s)"
                        "DONE(d)"
                        "CANCELLED(w)")))
  (org-agenda-span 5)
  (org-agenda-start-day "+0d")
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-timestamp-if-deadline-is-shown t)
  (org-startup-with-inline-images t)
  (org-tags-column 0)
  :config
  (add-to-list 'org-export-backends 'md)

  ;; let j,k move around `org-agenda-mode'
  (evil-define-key '(normal emacs) org-agenda-mode-map
    (kbd "j") 'org-agenda-next-item
    (kbd "k") 'org-agenda-previous-item
    (kbd "N") 'org-agenda-goto-date
    (kbd "P") 'org-agenda-capture)

  ;; Commands/functions/macros
  (defun gemacs/org-toggle-markup ()
    "Toggle hiding/showing `org-mode' emphasis markers."
    (interactive)
    (if org-hide-emphasis-markers
        (setopt org-hide-emphasis-markers nil)
      (setopt org-hide-emphasis-markers t))
    (org-mode-restart))

  ;; Binds
  (let ((map org-mode-map))
    (keymap-set map "C-'" nil) ; remove org agenda cycle list
    (keymap-set map "C-," nil) ; remove org agenda cycle list
    (keymap-set map "C-c m i" 'org-id-store-link)
    (keymap-set map "C-c m l" 'org-store-link)
    (keymap-set map "C-c C-x RET" #'gemacs/org-toggle-markup)))

(use-package org-modern
  :hook ((org-mode . global-org-modern-mode)
         (org-agenda-mode . global-org-modern-mode))
  :custom
  (org-modern-keyword "» ")
  (org-modern-star '("◉" "●" "○" "◈" "◇"))
  (org-modern-block-fringe nil)
  (org-modern-hide-stars nil)
  :config
  (defun gemacs/org-modern-hide-star-headings ()
    "Remove all stars from a heading, increase heading sizes and disable `org-indent-mode'."
    (interactive)
    (setopt org-modern-hide-stars t)
    (setopt org-hide-leading-stars t)
    (org-mode-restart)
    (gemacs/large-headings-mode 1)
    (org-indent-mode -1))

  (defun gemacs/org-modern-star-headings ()
    "Enable stars and `org-indent-mode'."
    (interactive)
    (setopt org-modern-hide-stars nil)
    (setopt org-hide-leading-stars nil)
    (org-mode-restart)
    (gemacs/large-headings-mode -1)
    (org-indent-mode 1))

  (gemacs/define-leader-key
    "twhj" 'gemacs/org-modern-hide-star-headings
    "twhk" 'gemacs/org-modern-star-headings))

(use-package org-wc
  :commands org-wc-display)

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package ox-epub
  :commands (org-export-dispatch))

;;; End of init.el
(load (locate-user-emacs-file "machine-init.el") :noerror :nomessage) ; ignore in version control
