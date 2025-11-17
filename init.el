;;; init.el -*- lexical-binding: t; -*-

;;; PREFACE

;;;; Directories

(defconst my/var-directory (locate-user-emacs-file "var")
  "Directory to store emacs' variable data.")

(defconst my/etc-directory (locate-user-emacs-file "etc")
  "Directory to store other emacs files such as extra configuration.")

(defconst my/packages-directory (locate-user-emacs-file "lisp")
  "Directory to store external emacs packages that are locally installed.")

(defconst my/auto-save-files-directory (expand-file-name "auto-saves" my/var-directory)
  "Directory to store temporary auto-save files.")

(defun my/get-var-file (path)
  "Return absolute file path of PATH relative to `my/var-directory'."
  (expand-file-name (convert-standard-filename path) my/var-directory))

(defun my/get-etc-file (path)
  "Return absolute file path of PATH relative to `my/etc-directory'."
  (expand-file-name (convert-standard-filename path) my/etc-directory))

(defun my/get-packages-file (path)
  "Return absolute file path of PATH relative to `my/packages-directory'."
  (expand-file-name (convert-standard-filename path) my/packages-directory))

;;;; Customization

(defcustom my/theme 'modus-operandi
  "Default Emacs theme.")

(defcustom my/theme-toggle-options '(modus-operandi modus-vivendi)
  "Two Emacs themes to toggle between that are available for
loading (`custom-available-themes').")

(defcustom my/font-family-default "Maple Mono"
  "Default font family.")

(defcustom my/font-size-default 120
  "Default font size.")

(defcustom my/indent-size-default 3
  "Size of indentation, in columns. Wrapper around `tab-width'."
  :group 'indent
  :type '(natnum))

(defcustom my/indent-use-tabs-default t
  "If non-nil, indentation will use tabs instead of spaces. Wrapper around
`indent-tabs-mode'."
  :group 'indent
  :type '(boolean))

(defcustom my/lang-indent-settings
  '((cc         :size 3 :use-tabs t)
    (cmake      :size 3 :use-tabs t)
    (css        :size 3 :use-tabs t)
    (go         :size 3 :use-tabs t)
    (html       :size 3 :use-tabs t)
    (java       :size 3 :use-tabs t)
    (javascript :size 3 :use-tabs t)
    (json       :size 3 :use-tabs t)
    (lisp       :size 8 :use-tabs nil)
    (markdown   :size 2 :use-tabs t)
    (rust       :size 3 :use-tabs t)
    (rst        :size 2 :use-tabs nil)
    (org        :size 8 :use-tabs nil)
    (tex        :size 3 :use-tabs t)
    (web        :size 3 :use-tabs t)
    (xml        :size 3 :use-tabs t)
    (yaml       :size 2 :use-tabs nil))
  "List of language-specific indentation settings. Access values using the
functions`my/lang-indent-size' and `my/lang-indent-use-tabs'.

Elements of this alist are of the form:

  (LANG-SYMBOL [:size SIZE] [:use-tabs USE-TABS])

where LANG-SYMBOL is a unique key name that represents a language, SIZE
is the width of each indent in columns, and USE-TABS is a boolean where
if non-nil, indentation will use tabs instead of spaces."
  :group 'indent)

;;;; General Lisp Load Path

(add-to-list 'load-path my/packages-directory)

;;; BASE CUSTOMIZATION

;; NOTE: using setq because it seems to be faster than setopt

(setq-default fill-column 72)

(setq backward-delete-char-untabify-method 'hungry)
(setq bookmark-default-file (my/get-etc-file "bookmarks"))
(setq completion-ignore-case t)
(setq custom-file (my/get-etc-file "custom.el"))
(setq delete-by-moving-to-trash t)
(setq enable-recursive-minibuffers t)
(setq find-file-visit-truename t)
(setq jit-lock-defer-time 0)
(setq kill-do-not-save-duplicates t)

;;;; Autosaves, Backups and Lockfiles
(setq auto-save-list-file-prefix (expand-file-name "sessions" my/auto-save-files-directory))
(setq auto-save-file-name-transforms `((".*" ,my/auto-save-files-directory t)))
(setq create-lockfiles nil)
(setq make-backup-files nil)

(setq kill-buffer-delete-auto-save-files t)

;;;; Auto-revert buffers
(setq global-auto-revert-non-file-buffers t)

;;;; Compile
(setq compilation-always-kill t)
(setq compilation-scroll-output 'first-error)

;;;; History lists
(setq history-length 300)
(setq transient-history-file (my/get-var-file "transient-history.el"))

;;;; Default buffer indentation
(setq-default tab-width my/indent-size-default)
(setq-default indent-tabs-mode my/indent-use-tabs-default)

;;;; Scroll
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling t)
(setq scroll-conservatively 101)
(setq scroll-perserve-screen-position t)

;; Can reduce lag in large buffers and speed up scrolling
(setq redisplay-skip-fontification-on-input t)

;;;; Better Buffer Names
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "^\\*")
(setq uniquify-separator "/")

;;;; Incremental search
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")

;;;; Line Numbers
(setq display-line-numbers-width 3)
(setq display-line-numbers-widen t)
(setq display-line-numbers-type 'relative)

;;;; Short Answers
(setq use-short-answers t)
(setq read-answer-short t)

;;;; Show parentheses
(setq show-paren-delay 0.1)
(setq show-paren-highlight-openparen t)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)

;;;; Undo
(setq undo-limit (* 13 160000))
(setq undo-strong-limit (* 13 240000))
(setq undo-outer-limit (* 13 24000000))

;;;; Visible/Audible Bell
(setq ring-bell-function #'ignore)
(setq visible-bell nil)

;;;; Whitespace

;; #x7C   = | , #x2502 = │
(setq whitespace-display-mappings '((tab-mark 9 [#x2502 9] [92 9])))
(setq whitespace-style '(face tabs tab-mark trailing))
(setq whitespace-line-column nil)

;;;; Project

(with-eval-after-load 'project
  (setq project-list-file (my/get-var-file "projects.el")))

;;;; Which-key Keybind Popup Helper

(with-eval-after-load 'which-key
  (setq which-key-idle-delay 0.1))

;;; FONTS

(defun my/font-size-set (value)
  "Set the base font size to VALUE (integer)."
  (interactive "nNew font size: ")
  (set-face-attribute 'default nil :height value))

(defun my/font-size-decrement ()
  "Decrement base font size by approximately 10."
  (interactive)
  (let* ((font-size (face-attribute 'default :height))
         (new-size  (- font-size 10)))
    (set-face-attribute 'default nil :height new-size)
    (message "New font size %d" (face-attribute 'default :height))))

(defun my/font-size-increment ()
  "Increment base font size by approximately 10."
  (interactive)
  (let* ((font-size (face-attribute 'default :height))
         (new-size  (+ font-size 10)))
    (set-face-attribute 'default nil :height new-size)
    (message "New font size %d" (face-attribute 'default :height))))

(defun my/font-family-set (font)
  "Set emacs `default' face's font family."
  (interactive (list (completing-read "Font: " (font-family-list))))
  (set-face-attribute 'default nil :family font))

(defun my/font-family-variable-pitch-set (font)
  "Set emacs `variable-pitch' face's font family."
  (interactive (list (completing-read "Font: " (font-family-list))))
  (set-face-attribute 'variable-pitch nil :family font))

;; 👋 Display emojis 🖥️⌨️🖱️
(defun my/fonts-enable-emojis ()
  (set-fontset-font
   t 'symbol
   (cond
    ((member "Adwaita Sans Text" (font-family-list)) "Adwaita Sans Text")
    ((member "Adwaita Sans" (font-family-list)) "Adwaita Sans")
    ((member "JuliaMono" (font-family-list)) "JuliaMono")
    ((member "Cascadia Code" (font-family-list)) "Cascadia Code")
    ((member "Cascadia Mono" (font-family-list)) "Cascadia Mono")
    ((member "Consolas" (font-family-list)) "Consolas")))
  (set-fontset-font
   t 'emoji
   (cond
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Symbola" (font-family-list)) "Symbola"))))

;;; THEME

(defun my/theme-set (theme)
  "Set the current emacs theme to THEME. Disables all other themes."
  (interactive
   (list (intern (completing-read "Theme: " (custom-available-themes)))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (enable-theme theme))

;; set the theme after init
(defun my/theme-load-my-theme ()
  "Set/reset theme based on the value of `my/theme'."
  (interactive)
  (my/theme-set my/theme))

(defun my/theme-toggle ()
  "Toggle between the two themes stored in `my/theme-toggle-options'"
  (interactive)
  (if (= (length my/theme-toggle-options) 2)
      (let ((theme-1 (car my/theme-toggle-options))
            (theme-2 (car (cdr my/theme-toggle-options))))
        (if (member theme-1 custom-enabled-themes)
            (my/theme-set theme-2)
          (my/theme-set theme-1)))
    (message "Variable `my/theme-toggle-options' must have exactly 2 options.")))

;;;; Modus-themes
(setq modus-themes-italic-constructs t)
(setq modus-themes-bold-constructs t)
(setq modus-themes-common-palette-overrides
      '((fg-line-number-inactive "gray50")
        (fg-line-number-active fg-main)

        (bg-line-number-inactive unspecified)
        (bg-line-number-active unspecified)

        (bg-mode-line-active bg-main)
        (fg-mode-line-active fg-main)

        (border-mode-line-active bg-dim)
        (border-mode-line-inactive bg-mode-line-inactive)))

;;; MODE-LINE

(autoload #'my/toggle-mode-line-mode "my-toggle-mode-line-mode"
  "Minor mode for toggling the visibility of the mode-line." t)
(autoload #'my/hide-mode-line "my-toggle-mode-line-mode"
  "Minor mode for hiding the mode-line." t)
(autoload #'my/show-mode-line "my-toggle-mode-line-mode"
  "Minor mode for showing the mode-line." t)

(setq mode-line-compact t)

;;; MINIBUFFER

(let* ((name "orderless")
       (path (my/get-packages-file name))
       (exists (file-directory-p path)))
  (if exists
      (progn
        (add-to-list 'load-path path)
        (defun my/orderless-completion--init ()
          (require 'orderless)
          (setq completion-styles '(orderless basic))
          (setq completion-category-overrides '((file (styles orderless partial-completion))))
          ;; Emacs 31: partial-completion behaves like substring
          (setq completion-pcm-leading-wildcard t)))
    (progn
      (setq completion-styles '(flex initials basic))
      (setq completion-category-overrides
            '((file (styles basic partial-completion)))))))

(defun my/minibuffer--backward-kill (arg)
  "When minibuffer is completing a file name, delete up to parent
folder, otherwise delete a word."
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (kill-word (- arg))))

(defun my/icomplete--init ()
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-delay-completions-threshold 0)
  (setq icomplete-compute-delay 0)
  (setq icomplete-in-buffer t)
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-scroll t)

  (advice-add 'completion-at-point :after #'minibuffer-hide-completions)
  (keymap-set icomplete-minibuffer-map "TAB" #'icomplete-force-complete)
  (keymap-set icomplete-minibuffer-map "C-M-i" #'minibuffer-complete))
(add-hook 'icomplete-mode-hook #'my/icomplete--init)

;;; LSP

;;;; Eglot

;; Requires harper to be installed: https://github.com/Automattic/harper
(with-eval-after-load 'eglot
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs
               '(text-mode . ("harper-ls" "--stdio"))))

;;; PACKAGES

(let* ((name "adaptive-wrap")
       (path (my/get-packages-file name))
       (exists (file-directory-p path)))
  (when exists
    (add-to-list 'load-path path)
    (autoload #'adaptive-wrap-prefix-mode name
      "Minor mode that visually indents wrapped lines." t)
    (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)))

(let* ((name "olivetti")
       (path (my/get-packages-file "olivetti"))
       (exists (file-directory-p path)))
  (when exists
    (add-to-list 'load-path (my/get-packages-file "olivetti"))
    (autoload #'olivetti-mode "olivetti"
      "Minor mode for providing a nice writing environment." t)

    (defun my/olivetti--no-newline-in-fringe ()
      "Hack to prevent cursor from going into the fringe."
      (setq-local overflow-newline-into-fringe nil))
    (add-hook 'olivetti-mode-hook #'my/olivetti--no-newline-in-fringe)

    (defvar my/writeroom-mode)
    (define-minor-mode my/writeroom-mode
      "Minor mode that toggles a nice writing environment"
      :global t
      (if my/writeroom-mode
          (progn
            (whitespace-mode -1)
            (my/toggle-mode-line-mode 1) ; TODO: make this buffer-local action
            (olivetti-mode 1))
        (progn
          (whitespace-mode 1)
          (my/toggle-mode-line-mode -1)
          (olivetti-mode -1))))))

(let* ((name "rainbow-delimiters")
       (path (my/get-packages-file name))
       (exists (file-directory-p path)))
  (when exists
    (add-to-list 'load-path path)
    (autoload #'rainbow-delimiters-mode name
      "Minor mode that highlights delimiters based on their depth." t)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

;;; TEXT EDITING

(defun my/paragraph-default-movement-local ()
  (interactive)
  (setq-local paragraph-start (default-value 'paragraph-start))
  (setq-local paragraph-separate (default-value 'paragraph-separate)))

(defun my/buffer-config-indent (size use-tabs)
  "Configure buffer-local indentation settings, where SIZE is the
indentation size in columns, and USE-TABS is a boolean where if non-nil,
tabs will be used instead of spaces."
  (interactive
   (list
    (read-number "Indent size (# of columns): ")
    (y-or-n-p "Use tabs instead of spaces")))
  (setq-local tab-width size
              indent-tabs-mode use-tabs))

(defun my/lang-indent-size (lang)
  "Get the size of indentation, in columns, for LANG, where LANG is a
symbol and a key to the `my/lang-indent-settings' list.

If the key or the size property of the language does not exist, then
return the default indentation size defined in `my/indent-size-default'."
  (let ((val (cdr (assoc lang my/lang-indent-settings))))
    (if (plist-get val :size)
        (plist-get val :size)
      tab-width)))

(defun my/lang-indent-use-tabs (lang)
  "Get whether indentation will use tabs instead of spaces on
indent for LANG, where LANG is a symbol and a key to the
`my/lang-indent-settings' list.

If the key or the use-tabs property of the language does not exist
then return the default use-tabs value defined in
`my/indent-use-tabs-default'."
  (let ((val (cdr (assoc lang my/lang-indent-settings))))
    (plist-get val :use-tabs)))

(defun my/lang-indent-set-local (lang)
  "Set default emacs indent rules based on LANG in local buffer. Note: you
may still need to modify the major-mode specific indent settings."
  (setq-local tab-width (my/lang-indent-size lang))
  (setq-local indent-tabs-mode (my/lang-indent-use-tabs lang)))

;;;; Language: C & C++

(setq-default c-basic-offset (my/lang-indent-size 'cc))
(defun my/hook--cc-mode ()
  "Settings for `c-mode' and `c++-mode'"
  (setq compile-command "make ")
  (setq-local indent-line-function 'tab-to-tab-stop)
  (c-set-style "bsd")
  (my/lang-indent-set-local 'cc)
  (keymap-set c-mode-map "C-c C-c" 'compile)
  (keymap-set c++-mode-map "C-c C-c" 'compile)

  (setq-local c-basic-offset (my/lang-indent-size 'cc)))
(add-hook 'c-mode-hook #'my/hook--cc-mode)
(add-hook 'c++-mode-hook #'my/hook--cc-mode)

;;;; Language: CMake

(autoload #'cmake-mode "cmake-mode"
  "Major mode for editing CMake files." t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(setq-default cmake-tab-width (my/lang-indent-size 'cmake))
(defun my/hook--cmake-mode ()
  "Settings for `cmake-mode'"
  (setq-local cmake-tab-width (my/lang-indent-size 'cmake))
  (my/lang-indent-set-local 'cmake))

;;;; Language: Go

(let* ((name "go-mode.el")
       (path (my/get-packages-file name))
       (exists (file-directory-p path)))
  (when exists
    (add-to-list 'load-path path)
    (autoload #'go-mode name
      "Major mode for editing Go files." t)
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

    (with-eval-after-load 'go-mode
      (setq gofmt-args '("-s")))

    (defun my/hook--go-mode ()
      "Configuration for `go-mode'."
      (setq compile-command "go build ")

      (keymap-set go-mode-map "C-c g" #'gofmt)
      (my/lang-indent-set-local 'go))

    (add-hook 'go-mode-hook #'my/hook--go-mode)))

;;;; Language: HTML & html templates

(let* ((name "web-mode")
       (path (my/get-packages-file name))
       (exists (file-directory-p path)))
  (when exists
    (add-to-list 'load-path path)
    (autoload #'web-mode name
      "Major mode for editing web template files." t)
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

    (defun my/web-mode-hook--setup ()
      "Configuration for `web-mode'."
      (my/lang-indent-set-local 'web)
      (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
      (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
      (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
      (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

      ;; (setq-local indent-line-function 'tab-to-tab-stop)
      (setq-local web-mode-enable-auto-pairing nil)
      (setq-local web-mode-enable-auto-closing nil)
      (setq-local web-mode-enable-auto-opening nil)
      (setq-local web-mode-enable-auto-indentation nil)
      (setq-local web-mode-enable-auto-quoting nil)
      (setq-local web-mode-enable-auto-expanding nil)

      (setq-local web-mode-block-padding (my/lang-indent-size 'web))
      (setq-local web-mode-style-padding (my/lang-indent-size 'web))
      (setq-local web-mode-part-padding (my/lang-indent-size 'web))
      (setq-local web-mode-script-padding (my/lang-indent-size 'web))

      (setq-local web-mode-markup-indent-offset (my/lang-indent-size 'web)) ; html
      (setq-local web-mode-css-indent-offset (my/lang-indent-size 'web)) ; css
      (setq-local web-mode-code-indent-offset (my/lang-indent-size 'web)) ; js/code
      (setq-local web-mode-indent-style (my/lang-indent-size 'web)))
    (add-hook 'web-mode-hook #'my/web-mode-hook--setup)))

;;;; Language: JSON

(defun my/json-mode-hook--setup ()
  "Configuration for `js-json-mode'"
  (my/lang-indent-set-local 'json)
  (setq-local js-indent-level (my/lang-indent-size 'json)))

(add-hook 'js-json-mode-hook #'my/json-mode-hook--setup)

;;;; Language: Lisp

(defun my/hook--lisp-mode ()
  "Configuration for lisp-like languages such as `lisp-mode' and
`emacs-lisp-mode'."
  (setq-local fill-column 70)

  (my/lang-indent-set-local 'lisp)
  (electric-indent-local-mode 1)
  (electric-pair-local-mode 1))

(add-hook 'emacs-lisp-mode-hook #'my/hook--lisp-mode)
(add-hook 'lisp-mode-hook #'my/hook--lisp-mode)

;;;; Language: Markdown

(let* ((name "markdown-mode")
       (path (my/get-packages-file name))
       (exists (file-directory-p path)))
  (when exists
    (add-to-list 'load-path path)
    (autoload #'markdown-mode name
      "Major mode for editing Markdown files." t)
    (add-to-list 'auto-mode-alist '("\\.\\(?:md\\|markdown\\)\\'" . markdown-mode))

    (defun my/hook--markdown-mode ()
      "Configuration for `markdown-mode'."
      (let ((map markdown-mode-map))
        (define-key map [remap backward-paragraph] 'backward-paragraph)
        (define-key map [remap forward-paragraph] 'forward-paragraph))
      (setq-local indent-line-function 'tab-to-tab-stop)
      (keymap-set markdown-mode-map "TAB" 'indent-for-tab-command)
      (my/lang-indent-set-local 'markdown)
      (my/paragraph-default-movement-local)
      (visual-line-mode 1))

    (add-hook 'markdown-mode-hook #'my/hook--markdown-mode)))

;;;; Language: Python

(setq python-indent-guess-indent-offset-verbose nil)
(defun my/hook--python-mode ()
  "Configuration for `python-mode' buffers."
  (setq-local python-indent-offset (my/lang-indent-size 'python))
  (my/lang-indent-set-local 'python))
(add-hook 'python-mode-hook #'my/hook--python-mode)

;;;; Language: Rust

(let* ((name "rust-mode")
       (path (my/get-packages-file name))
       (exists (file-directory-p path)))
  (when exists
    (add-to-list 'load-path path)
    (autoload #'rust-mode name nil t)
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

    (defun my/rust-mode--hook-setup ()
      "Configuration for `rust-mode'."
      (my/lang-indent-set-local 'rust))

    (add-hook 'rust-mode-hook #'my/rust-mode--hook-setup)))

;;; UTILS

(defun my/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when the
minibuffer is open.  Whereas we want it to close the minibuffer, even
without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'.

Credit: ripped from
https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/#h:1e468b2a-9bee-4571-8454-e3f5462d9321
"
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

;; Found in: https://github.com/LionyxML/emacs-solo/
;; A Protesilaos life savier HACK
;; Add option "d" to whenever using C-x s or C-x C-c, allowing a quick preview
;; of the diff (if you choose `d') of what you're asked to save.

(defun my/diff-current-to-saved-file ()
  "Show diff between the current unsaved buffer/file contents and the saved
buffer/file contents."
  (interactive)
  (diff-buffer-with-file (current-buffer)))

(add-to-list 'save-some-buffers-action-alist
             (list "d"
                   (lambda (buffer)
                     (diff-buffer-with-file (buffer-file-name buffer)))
                   "show diff between the buffer and its file"))

(defun my/open-file ()
  "Open current buffer/file in external app.

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

(defun my/open-current-directory ()
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

;;; HOOKS

(defun my/hook--after-init ()
  "Basic configuration on `after-init-hook'."
  (advice-add #'split-window-below :after (lambda (&rest _) (other-window 1)))
  (advice-add #'split-window-right :after (lambda (&rest _) (other-window 1)))

  (set-face-attribute 'default nil :family my/font-family-default :height my/font-size-default)

  (my/fonts-enable-emojis)
  (my/theme-load-my-theme)

  (icomplete-vertical-mode 1)

  (my/orderless-completion--init)

  (blink-cursor-mode -1)
  (column-number-mode 1)
  (delete-selection-mode 1)
  (electric-indent-mode -1)
  (global-auto-revert-mode 1)
  (which-key-mode 1)
  (winner-mode 1))
(add-hook 'after-init-hook #'my/hook--after-init)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(add-hook 'text-mode-hook #'whitespace-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)

;;; KEYBINDINGS

(keymap-global-set "<escape>" 'keyboard-escape-quit)
(keymap-global-set "C-g" #'my/keyboard-quit-dwim)

(keymap-global-set "<f8>" 'variable-pitch-mode)
(keymap-global-set "<f5>" #'my/theme-toggle)
(keymap-global-set "C-<f5>" #'my/theme-set)

(keymap-global-set "C-=" #'my/font-size-increment)
(keymap-global-set "C-+" #'my/font-size-decrement)
(keymap-global-set "C-c C-0" #'my/font-size-set)

(keymap-global-set "M-[" 'backward-paragraph)
(keymap-global-set "M-]" 'forward-paragraph)
(keymap-global-set "M-s M-s" 'grep)

(keymap-global-set "C-z" nil)
(keymap-global-set "C-x C-k RET" nil)
(keymap-global-set "C-x C-z" nil)

(keymap-global-set "C-x C-p" 'project-find-file)

(keymap-global-set "C-c -" 'kill-buffer-and-window)
(keymap-global-set "C-c C-SPC" 'just-one-space)
(keymap-global-set "C-c j j" 'compile)
(keymap-global-set "C-c m l" 'display-line-numbers-mode)
(keymap-global-set "C-c m w" 'whitespace-mode)
(keymap-global-set "C-c m t w" #'my/writeroom-mode)

(keymap-set minibuffer-local-map "C-<backspace>" #'my/minibuffer--backward-kill)
(keymap-set minibuffer-local-map "M-<backspace>" #'my/minibuffer--backward-kill)

;;; END: load machine-init.el

(load (my/get-etc-file "machine-init.el") :no-error-if-file-is-missing :nomessage)
