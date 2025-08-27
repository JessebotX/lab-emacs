;; init.el -*- lexical-binding: t; -*-
;; ══════════════════════════════════════════════════════════════════════
;;
;;               _|    _|              _|   _|
;;               _|    _|     _|_|     _|   _|     _|_|
;;               _|_|_|_|   _|_|_|_|   _|   _|   _|    _|
;;               _|    _|   _|         _|   _|   _|    _|
;;               _|    _|     _|_|_|   _|   _|     _|_|
;;
;; ══════════════════════════════════════════════════════════════════════

;;; [PREFACE]

(defconst my/var-directory (locate-user-emacs-file "var")
  "Directory to store emacs' variable data.")

(defconst my/etc-directory (locate-user-emacs-file "etc")
  "Directory to store other emacs files such as extra configuration.")

(defconst my/auto-save-files-directory (expand-file-name "auto-saves" my/var-directory)
  "Directory to store temporary auto-save files.")

(defconst my/packages-directory (locate-user-emacs-file "lisp")
  "Directory to store external emacs packages that are locally installed.")

(defcustom my/terminal nil
  "Default terminal to open for `my/open-terminal'. If nil, use
platform-specific defaults defined in `my/open-terminal'.")

(defcustom my/terminal-args nil
  "Program arguments to pass into terminal. Used in `my/open-terminal'
only when `my/terminal' is non-nil.")

(defcustom my/theme 'modus-operandi
  "Default emacs color theme.")

(defcustom my/theme-toggle-options '(modus-operandi adwaita-dark)
  "Two Emacs themes to toggle between that are available for
loading (`custom-available-themes').")

(defcustom my/lang-indent-settings
  '((cc         :size 3 :use-tabs t)
    (css        :size 3 :use-tabs t)
    (go         :size 3 :use-tabs t)
    (html       :size 3 :use-tabs t)
    (java       :size 3 :use-tabs t)
    (javascript :size 3 :use-tabs t)
    (json       :size 3 :use-tabs t)
    (lisp       :size 8 :use-tabs nil)
    (markdown   :size 2 :use-tabs t)
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

(defcustom my/indent-size-default 3
  "Size of indentation, in columns. Wrapper around `tab-width'."
  :group 'indent
  :type '(natnum))

(defcustom my/indent-use-tabs-default t
  "If non-nil, indentation will use tabs instead of spaces. Wrapper around
`indent-tabs-mode'."
  :group 'indent
  :type '(boolean))

(defcustom my/font-size-default 120
  "Default font height for the `default' face. Divide by 10 to get a
traditional-looking font size value."
  :group 'face
  :type '(natnum))

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

(defun my/lang-indent-set-local (lang)
  "Set default emacs indent rules based on LANG in local buffer. Note: you
may still need to modify the major-mode specific indent settings."
  (setq-local tab-width (my/lang-indent-size lang))
  (setq-local indent-tabs-mode (my/lang-indent-use-tabs lang)))

(defun my/get-var-file (path)
  "Return absolute file path of PATH relative to `my/var-directory'."
  (expand-file-name (convert-standard-filename path) my/var-directory))

(defun my/get-etc-file (path)
  "Return absolute file path of PATH relative to `my/etc-directory'."
  (expand-file-name (convert-standard-filename path) my/etc-directory))

(defun my/get-packages-file (path)
  "Return absolute file path of PATH relative to `my/packages-directory'."
  (expand-file-name (convert-standard-filename path) my/packages-directory))

(defun my/goto-config-init ()
  "Jump to user's init.el `user-init-file'"
  (interactive)
  (find-file user-init-file))

(defun my/open-terminal ()
  "Open the current directory in the terminal

Credit: http://xahlee.info/emacs/emacs/emacs_open_in_terminal.html"
  (interactive)
  (cond
   (my/terminal
    (start-process
     ""
     nil
     my/terminal
     (if my/terminal-args
         (format my/terminal-args (expand-file-name default-directory)))))
   ((eq system-type 'windows-nt)
    (shell-command (format "wt -d \"%s\"" default-directory)))
   ((eq system-type 'darwin)
    (shell-command
     (format "open -a terminal %s" (shell-quote-argument (expand-file-name default-directory)))))
   (t
    (start-process "" nil "x-terminal-emulator"
                   (format "--working-directory=%s"
                           (shell-quote-argument
                            (expand-file-name default-directory)))))))

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

(defun my/kill-ring-clear ()
  "Clear all saved text in the kill ring."
  (interactive)
  (setq kill-ring nil)
  (garbage-collect))

(defun my/increment-number-at-point ()
  "Increment integer under the cursor."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun my/decrement-number-at-point ()
  "Decrement integer under the cursor."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(defun my/font-size-increment ()
  "Increment base font size by approximately 10."
  (interactive)
  (let* ((font-size (face-attribute 'default :height))
         (new-size  (+ font-size 15)) ; sometimes it doesn't add enough so I add 15 instead of 10
         (mod-size  (mod new-size 10)))
    (message "New: %d, Mod: %d" new-size mod-size)
    (if (eq mod-size 0)
        (set-face-attribute 'default nil :height new-size)
      (set-face-attribute 'default nil :height (- new-size mod-size))))
  (message "New font size %d" (face-attribute 'default :height)))

(defun my/font-size-decrement ()
  "Decrement base font size by approximately 10."
  (interactive)
  (let* ((font-size (face-attribute 'default :height))
         (new-size  (- font-size 5)) ; sometimes it decrements too much so I subtract 5 instead of 10
         (mod-size  (mod new-size 10)))
    (message "New: %d, Mod: %d" new-size mod-size)
    (if (eq mod-size 0)
        (set-face-attribute 'default nil :height new-size)
      (set-face-attribute 'default nil :height (- new-size mod-size))))
  (message "New font size %d" (face-attribute 'default :height)))

(defun my/font-size-set (value)
  "Set the base font size to VALUE (integer)."
  (interactive "nNew font size: ")
  (set-face-attribute 'default nil :height value))

(defun my/font-family-set (font)
  "Set emacs `default' face's font family."
  (interactive (list (completing-read "Font: " (font-family-list))))
  (set-face-attribute 'default nil :family font))

(defun my/font-variable-pitch-family-set (font)
  "Set emacs `variable-pitch' face's font family."
  (interactive (list (completing-read "Font: " (font-family-list))))
  (set-face-attribute 'variable-pitch nil :family font))
(keymap-global-set "<f8>" 'variable-pitch-mode)
(keymap-global-set "<f9>" 'my/font-variable-pitch-family-set)

(defun my/theme-set (theme)
  "Set the current emacs theme to THEME. Disables all other themes."
  (interactive (list (intern (completing-read "Theme: " (custom-available-themes)))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (enable-theme theme))

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

(keymap-global-set "<f5>" 'my/theme-set)
(keymap-global-set "C-<f5>" 'my/theme-toggle)

(defun my/diff-changes-to-saved-file ()
  "Show diff between the current unsaved buffer/file contents and the saved
buffer/file contents."
  (interactive)
  (diff-buffer-with-file (current-buffer)))

;;; [LISP MODULES]
(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(add-to-list 'load-path (locate-user-emacs-file "modules"))

;;; [BASE CUSTOMIZATION VARIABLES]

;; Customization variables
(setopt ad-redefinition-action 'accept) ; disable warning about advice de/activation
(setopt backward-delete-char-untabify-method 'hungry)
(setopt bookmark-default-file (my/get-etc-file "bookmarks"))
(setopt completion-ignore-case t)
(setopt delete-by-moving-to-trash t)
(setopt display-line-numbers-width 3)
(setopt display-line-numbers-widen t)
(setopt display-line-numbers-type 'relative)
(setopt enable-recursive-minibuffers t)
(setopt fast-but-imprecise-scrolling t)
(setopt grep-command "rg -nHS --no-heading --null ")
(setopt grep-find-ignored-directories
        '("SCCS"
          "RCS"
          "CVS"
          "MCVS"
          ".src"
          ".svn"
          ".jj"
          ".git"
          ".hg"
          ".bzr"
          "_MTN"
          "_darcs"
          "{arch}"
          "node_modules"
          "build"
          "dist"
          ".venv"
          "venv"))
(setopt history-length 300)
(setopt indent-tabs-mode my/indent-use-tabs-default)
(setopt isearch-lazy-count t)
(setopt jit-lock-defer-time 0)
(setopt kill-do-not-save-duplicates t)
(setopt lazy-count-prefix-format nil)
(setopt lazy-count-suffix-format "   (%s/%s)")
(setopt read-answer-short t)
(setopt ring-bell-function #'ignore) ; disable sound on invalid input
(setopt scroll-conservatively 101) ; scroll normally
(setopt scroll-perserve-screen-position t)
(setopt transient-history-file (my/get-var-file "transient-history.el"))
(setopt undo-limit (* 13 160000))
(setopt undo-strong-limit (* 13 240000))
(setopt undo-outer-limit (* 13 24000000))
(setopt uniquify-buffer-name-style 'forward)
(setopt uniquify-ignore-buffers-re "^\\*")
(setopt uniquify-separator "/")
(setopt use-short-answers t)
(setopt visible-bell nil) ; disable visual indicator of invalid input
                                        ;(setopt word-wrap nil)

;; Non-customization variables
(setq auto-window-vscroll nil)
(setq custom-file (my/get-etc-file "custom.el"))
(setq-default fill-column 72)
(setq-default tab-width my/indent-size-default)
(setq-default indent-tabs-mode my/indent-use-tabs-default)

;;; [AUTOSAVES, BACKUPS AND LOCKFILES]
(setq auto-save-list-file-prefix (expand-file-name "sessions" my/auto-save-files-directory))
(setq auto-save-file-name-transforms `((".*" ,my/auto-save-files-directory t)))
(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)

;;; [FONTS]

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

(add-hook 'emacs-startup-hook 'my/fonts-enable-emojis)

;;; [THEME]
(setopt modus-themes-italic-constructs t)
(setopt modus-themes-bold-constructs t)
(setopt modus-themes-common-palette-overrides
        '((fg-line-number-inactive "gray50")
          (fg-line-number-active fg-main)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)))

;; set the theme after init
(defun my/theme-read-my-theme ()
  "Set/reset theme based on the value of `my/theme'."
  (interactive)
  (my/theme-set my/theme))
(add-hook 'after-init-hook #'my/theme-read-my-theme)

;; adwaita-dark
(add-hook 'enable-theme-functions
          (lambda (&optional _theme)
            (if (member 'adwaita-dark custom-enabled-themes)
                (custom-set-faces
                 '(completions-first-difference ((t :weight normal)))
                 '(highlight ((t :background "#64a6f6" :foreground "#303030" :distant-foreground "#111111"))))
              (custom-set-faces
               '(highlight (()))))))


;;; [KEYBINDINGS]
(keymap-global-set "<escape>" 'keyboard-escape-quit)
(keymap-global-set "M-[" 'backward-paragraph)
(keymap-global-set "M-]" 'forward-paragraph)
(keymap-global-set "M-s M-s" 'grep)
(keymap-global-set "C-z" nil)
(keymap-global-set "C-x C-k RET" nil)
(keymap-global-set "C-x C-z" nil)
(keymap-global-set "C-c m l" 'display-line-numbers-mode)
(keymap-global-set "C-c C-SPC" 'just-one-space)
(keymap-global-set "C-c j j" 'compile)
(keymap-global-set "C-c -" 'kill-buffer-and-window)

;; TODO: doesn't actually work as intended
(defun my/kill-region (start end)
  "Improved `kill-region' to prevent accidentally deleting text when there
is no region selected."
  (interactive "r")
  (if (not (eq start end))
      (progn
        (kill-ring-save start end)
        (set-mark start)
        (goto-char end)
        (delete-region start end))
    (message "No region selected.")))
(keymap-global-set "C-w" #'my/kill-region) ; better kill region so it doesn't delete half the buffer
(keymap-global-set "S-<delete>" #'my/kill-region)

(defun my/toggle-fundamental-mode ()
  (interactive)
  (cond
   ((eq major-mode 'fundamental-mode)
    (lisp-interaction-mode))
   ((eq major-mode 'lisp-interaction-mode)
    (fundamental-mode))
   (t
    (message "Command only works in `fundamental-mode' or `lisp-interaction-mode'"))))
(keymap-global-set "C-c m f" #'my/toggle-fundamental-mode)

;; Switch to new window on split
(advice-add #'split-window-below :after (lambda (&rest _) (other-window 1)))
(advice-add #'split-window-right :after (lambda (&rest _) (other-window 1)))

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

(keymap-global-set "C-g" #'my/keyboard-quit-dwim)

;;; [MODES]
(defun my/hook--after-init ()
  "Basic configuration on `after-init-hook'."
  (with-current-buffer (get-buffer-create "*scratch*")
    (insert (format ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Hello          ;;
;;                         ;;
;; ██╗   ██╗██╗███╗   ███╗ ;;
;; ██║   ██║██║████╗ ████║ ;;
;; ██║   ██║██║██╔████╔██║ ;;
;; ╚██╗ ██╔╝██║██║╚██╔╝██║ ;;
;;  ╚████╔╝ ██║██║ ╚═╝ ██║ ;;
;;    ╚══╝  ╚═╝╚═╝     ╚═╝ ;;
;;                         ;;
;;  Startup time :  %.2fs  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

"
                    (float-time
                     (time-subtract after-init-time before-init-time)))))

  (recentf-mode 1)
  (winner-mode 1)
  (electric-indent-mode -1)
  (blink-cursor-mode -1)
  (delete-selection-mode 1))
(add-hook 'after-init-hook #'my/hook--after-init)

;; Found in: https://github.com/LionyxML/emacs-solo/
;; A Protesilaos life savier HACK
;; Add option "d" to whenever using C-x s or C-x C-c, allowing a quick preview
;; of the diff (if you choose `d') of what you're asked to save.
(add-to-list 'save-some-buffers-action-alist
             (list "d"
                   (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                   "show diff between the buffer and its file"))

;;; [MODE-LINE]
(setopt mode-line-right-align-edge 'right-margin)
(setopt mode-line-percent-position nil)
(setopt mode-line-position-line-format '("L%l"))
(setopt mode-line-position-column-line-format '("%l:%c"))
(setopt display-time-format "[%-l:%M %p]")
(setopt display-time-default-load-average nil)

(add-hook 'emacs-startup-hook (lambda () (line-number-mode -1)))

;; Only show line and column numbers in `prog-mode'-derived modes
(add-hook 'prog-mode-hook #'line-number-mode)
(add-hook 'prog-mode-hook #'column-number-mode)

;; My custom mode line
(autoload #'my/mode-line-mode "my-mode-line"
  "Minor mode for enabling my custom mode-line." t)
(add-hook 'emacs-startup-hook #'my/mode-line-mode)

;;; [AUTO REVERT BUFFERS]
(setopt global-auto-revert-non-file-buffers t)
(add-hook 'after-init-hook #'global-auto-revert-mode)

;;; [COMPILE]
(setopt compilation-always-kill t)
(setopt compilation-scroll-output t)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;;; [COMPLETION AND MINIBUFFER]
(setopt completion-styles '(flex initials basic))
(setopt completion-category-overrides '((file (styles basic partial-completion))))

(defun my/minibuffer--backward-kill (arg)
  "When minibuffer is completing a file name, delete up to parent
folder, otherwise delete a word."
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (kill-word (- arg))))

(keymap-set minibuffer-local-map "C-<backspace>" #'my/minibuffer--backward-kill)
(keymap-set minibuffer-local-map "M-<backspace>" #'my/minibuffer--backward-kill)

(setopt icomplete-show-matches-on-no-input t)
(setopt icomplete-delay-completions-threshold 0)
(setopt icomplete-compute-delay 0)
(setopt icomplete-in-buffer t)
(setopt icomplete-max-delay-chars 0)
(setopt icomplete-scroll t)
(icomplete-vertical-mode 1)
(keymap-set icomplete-minibuffer-map "TAB" #'icomplete-force-complete)
(keymap-set icomplete-minibuffer-map "C-M-i" #'minibuffer-complete)
(advice-add 'completion-at-point :after #'minibuffer-hide-completions)

;;; [DIRED]
(setopt dired-hide-details-hide-symlink-targets nil)
(setopt dired-listing-switches "-al --group-directories-first")
(setopt dired-recursive-copies 'always)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;;;; [DIRED ICONS]
;; Credit: https://github.com/LionyxML/emacs-solo
;; TODO: Dired parent/current dir are not used
(defvar my/dired-icons-file-icons
  '(("zip"             . "📦")
    ("zipx"            . "📦")
    ("rar"             . "📦")
    ("xz"              . "📦")
    ("tar"             . "📦")
    ("tgz"             . "📦")
    ("gz"              . "📦")
    ("bz2"             . "📦")
    ("7z"              . "📦")
    ("lz"              . "📦")
    ("lzma"            . "📦")
    ("zstd"            . "📦")
    ("rpm"             . "📦")
    ("deb"             . "📦")
    ("direddir"        . "📁")
    ("diredfile"       . "📄")
    ("diredparentdir"  . "⬆️")
    ("diredcurrentdir" . "⤵️")))

(defun my/dired-icons-icon-for-file (filepath)
  "Get icon based on filepath extension"
  (if (file-directory-p filepath)
      (assoc-default "direddir" my/dired-icons-file-icons)
    (progn
      (let* ((ext (file-name-extension filepath))
             (icon (and ext (assoc-default (downcase ext) my/dired-icons-file-icons))))
        (or icon (assoc-default "diredfile" my/dired-icons-file-icons))))))

(defun my/dired-icons-icons-regexp ()
  "Return a regexp that matches any icon we use."
  (let ((icons (mapcar #'cdr my/dired-icons-file-icons)))
    (concat "^\\(" (regexp-opt (cons "📁" icons)) "\\) ")))

(defun my/dired-icons-add-icons ()
  "Add icons to filenames in Dired buffer."
  (when (derived-mode-p 'dired-mode)
    (let ((inhibit-read-only t)
          (icon-regex (my/dired-icons-icons-regexp)))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (condition-case nil
              (when-let* ((file (dired-get-filename nil t)))
                (dired-move-to-filename)
                (unless (looking-at-p icon-regex)
                  (insert (concat (my/dired-icons-icon-for-file file) " "))))
            (error nil))  ;; gracefully skip invalid lines
          (forward-line 1))))))

(add-hook 'dired-after-readin-hook #'my/dired-icons-add-icons)

;;;; [DIRED IMAGES / IMAGE-DIRED]
(setopt image-dired-db-file (my/get-var-file "image-dired/db.el"))
(setopt image-dired-dir (my/get-var-file "image-dired/"))
(setopt image-dired-gallery-dir (my/get-var-file "image-dired/gallery/"))
(setopt image-dired-temp-image-file (my/get-var-file "image-dired/temp-image"))
(setopt image-dired-temp-rotate-image-file (my/get-var-file "image-dired/temp-rotate-image"))

;;; [EGLOT LSP SERVER]
(setopt eglot-autoshutdown t)

;; Requires harper to be installed: https://github.com/Automattic/harper
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(text-mode . ("harper-ls" "--stdio"))))

;;; [EWW BROWSER]
(setopt url-cache-directory (my/get-var-file "url/cache/"))
(setopt url-configuration-directory (my/get-var-file "url/"))
(setopt url-cookie-file (my/get-var-file "url/cookies.el"))
(setopt url-history-file (my/get-var-file "url/history.el"))

;;; [IBUFFER BUFFER LIST]
(setopt ibuffer-saved-filter-groups
        '(("Default"
           ("Emacs" (or (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*Warnings\\*$")
                        (name . "^\\*Shell Command Output\\*$")
                        (name . "^\\*Async-native-compile-log\\*$")))
           ("Dired" (mode . dired-mode))
           ("Terminal" (or (mode . term-mode)
                           (mode . shell-mode)
                           (mode . eshell-mode)))
           ("Misc" (or (mode . compilation-mode)
                       (mode . special-mode)
                       (mode . change-log-mode)
                       (mode . vc-dir-mode)
                       (mode . diff-mode)
                       (name . "^\\*vc\\*$")))
           ("Help" (or (mode . help-mode)
                       (name . "^\\*Help\\*$")
                       (name . "^\\*info\\*$"))))))
(setopt ibuffer-show-empty-filter-groups nil)
(keymap-global-set "C-x C-b" 'ibuffer)

;; Highlight on copy
(defun my/pulse-region (orig-fn beg end &rest args)
  "Highlight region with `pulse'.

Credit: https://blog.meain.io/2020/emacs-highlight-yanked/"
  (pulse-momentary-highlight-region beg end)
  (apply orig-fn beg end args))

(advice-add 'kill-ring-save :around #'my/pulse-region)

;;; [PROJECTS]
(keymap-global-set "C-x C-p" 'project-find-file)
(with-eval-after-load 'project
  (setopt project-list-file (my/get-var-file "projects.el")))

;;; [RECENTF]
(setopt recentf-save-file "~/.config/emacs/var/recentf.el")

;;; [WHICH-KEY]
(setopt which-key-idle-delay 0.1)
(add-hook 'emacs-startup-hook #'which-key-mode)

;;; [WHITESPACE]

;; #x7C   = |
;; #x2502 = │
(setopt whitespace-display-mappings '((tab-mark 9 [#x2502 9] [92 9])))
(setopt whitespace-style '(face tabs tab-mark trailing))
(add-hook 'text-mode-hook #'whitespace-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)

(keymap-global-set "C-c m w" 'whitespace-mode)

;;; [OTHER LISP]
;;;; [SUBTLE MODE LINE COLORS]
(autoload #'my/subtle-mode-line-colors-mode "my-subtle-mode-line-colors-mode"
  "Minor mode for making mode line colors more subtle." t)
(defun my/subtle-mode-line-colors-mode-enable-and-refresh (&optional _theme)
  (interactive)
  (cond
   ((or (member 'modus-operandi        custom-enabled-themes)
        (member 'modus-operandi-tinted custom-enabled-themes)
        (member 'lambda-light          custom-enabled-themes)
        (member 'lambda-light-faded    custom-enabled-themes)
        (member 'standard-light        custom-enabled-themes)
        (member 'standard-light-tinted custom-enabled-themes))
    (setopt my/subtle-mode-line-colors-mode-color "#dddddd")
    (my/subtle-mode-line-colors-mode))
   ((or (member 'modus-vivendi        custom-enabled-themes)
        (member 'standard-dark        custom-enabled-themes)
        (member 'standard-dark-tinted custom-enabled-themes))
    (setopt my/subtle-mode-line-colors-mode-color "#444444")
    (my/subtle-mode-line-colors-mode))
   ((or (member 'adwaita-dark custom-enabled-themes))
    (setopt my/subtle-mode-line-colors-mode-color "#343434")
    (my/subtle-mode-line-colors-mode))
   (t
    (setopt my/subtle-mode-line-colors-mode-color (face-foreground 'shadow))
    (my/subtle-mode-line-colors-mode 1))))
(add-hook 'emacs-startup-hook #'my/subtle-mode-line-colors-mode-enable-and-refresh)
(add-hook 'enable-theme-functions #'my/subtle-mode-line-colors-mode-enable-and-refresh)

;;;; [TOGGLE MODE-LINE]
(autoload #'my/toggle-mode-line-mode "my-toggle-mode-line-mode"
  "Minor mode for toggling the visibility of the mode-line." t)
(autoload #'my/hide-mode-line "my-toggle-mode-line-mode"
  "Minor mode for hiding the mode-line." t)
(autoload #'my/show-mode-line "my-toggle-mode-line-mode"
  "Minor mode for showing the mode-line." t)

;;; [OTHER PACKAGES]
;;;; [ADAPTIVE WRAP]
;; Visually indents wrapped lines
(add-to-list 'load-path (my/get-packages-file "adaptive-wrap"))
(autoload #'adaptive-wrap-prefix-mode "adaptive-wrap"
  "Minor mode that visually indents wrapped lines." t)
(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)

;;;; [OLIVETTI]
(add-to-list 'load-path (my/get-packages-file "olivetti"))
(autoload #'olivetti-mode "olivetti"
  "Minor mode for providing a nice writing environment." t)

(defvar my/writeroom-mode)
(define-minor-mode my/writeroom-mode
  "Minor mode that toggles a nice writing environment"
  :global t
  (if my/writeroom-mode
      (progn
        (my/font-size-set (+ my/font-size-default 100))
        (whitespace-mode -1)
        (my/toggle-mode-line-mode 1) ; TODO: make this buffer-local action
        (olivetti-mode 1))
    (progn
      (my/font-size-set my/font-size-default)
      (whitespace-mode 1)
      (my/toggle-mode-line-mode -1)
      (olivetti-mode -1))))

(keymap-global-set "C-c m t w" #'my/writeroom-mode)

;;; [LANGUAGES]
(defun my/paragraph-default-movement-local ()
  (interactive)
  (setq-local paragraph-start (default-value 'paragraph-start))
  (setq-local paragraph-separate (default-value 'paragraph-separate)))

;;;; [TEXT MODES]
(defun my/hook--text-mode ()
  "Configuration for `text-mode' buffers."
  (interactive)
  (visual-line-mode 1))
(add-hook 'text-mode-hook #'my/hook--text-mode)

;;;; [ASCIIDOC]
(add-to-list 'load-path (my/get-packages-file "adoc-mode"))
(autoload #'adoc-mode "adoc-mode"
  "Major mode for editing AsciiDoc files." t)
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))

(with-eval-after-load 'adoc-mode
  (custom-set-faces
   '(adoc-title-0-face ((t :height 1.0 :inherit adoc-title-face)))
   '(adoc-title-1-face ((t :height 1.0 :inherit adoc-title-face)))
   '(adoc-title-2-face ((t :height 1.0 :inherit adoc-title-face)))
   '(adoc-title-3-face ((t :height 1.0 :inherit adoc-title-face)))
   '(adoc-title-4-face ((t :height 1.0 :inherit adoc-title-face)))
   '(adoc-title-5-face ((t :height 1.0 :inherit adoc-title-face)))))

;;;; [C / C++]
(defun my/hook--cc-mode ()
  "Settings for `c-mode' and `c++-mode'"
  (setopt compile-command "make ")
  (c-set-style "bsd")
  (my/lang-indent-set-local 'cc)
  (keymap-set c-mode-map "C-c C-c" 'compile)
  (keymap-set c++-mode-map "C-c C-c" 'compile)
  (setq-local c-basic-offset (my/lang-indent-size 'cc)))

(add-hook 'c-mode-hook #'my/hook--cc-mode)
(add-hook 'c++-mode-hook #'my/hook--cc-mode)

;;;; [CSS]
(setq-default css-indent-offset (my/lang-indent-size 'css))
(defun my/hook--css-mode ()
  "Configuration for `css-mode'."
  (my/lang-indent-set-local 'css)
  (setq-local css-indent-offset (my/lang-indent-size 'css)))
(add-hook 'css-mode-hook #'my/hook--css-mode)

;;;; [GO]
(add-to-list 'load-path (my/get-packages-file "go-mode"))
(autoload #'go-mode "go-mode"
  "Major mode for editing Go files." t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(with-eval-after-load 'go-mode
  (setopt gofmt-args '("-s")))

(defun my/hook--go-mode ()
  "Configuration for `go-mode'."
  (setopt compile-command "go build ")
  (keymap-set go-mode-map "C-c g" #'gofmt)
  (my/lang-indent-set-local 'go))
(add-hook 'go-mode-hook #'my/hook--go-mode)

;;;; [JAVA]
(defun my/java-mode-hook--setup ()
  "Configuration for `java-mode'"
  (my/lang-indent-set-local 'java)
  (setq-local c-basic-offset (my/lang-indent-size 'java)))
(add-hook 'java-mode-hook #'my/java-mode-hook--setup)

;;;; [JAVASCRIPT]
(setq-default js-indent-level (my/lang-indent-size 'js))
(defun my/hook--js-mode ()
  "Configuration for `js-mode' and `js-jsx-mode'."
  (my/lang-indent-set-local 'javascript)
  (setq-local js-indent-level (my/lang-indent-size 'javascript)))

(add-hook 'js-mode-hook #'my/hook--js-mode)
(add-hook 'js-jsx-mode-hook #'my/hook--js-mode)

;;;; [JSON]
(defun my/hook--js-json-mode ()
  "Configuration for `js-json-mode'"
  (setq-local js-indent-level (my/lang-indent-size 'json))
  (my/lang-indent-set-local 'json))

(add-hook 'js-json-mode-hook #'my/hook--js-json-mode)

;;;; [LISP]
(defun my/hook--lisp-mode ()
  "Configuration for lisp-like languages such as `lisp-mode' and
`emacs-lisp-mode'."
  (setq-default fill-column 70)
  (my/lang-indent-set-local 'lisp)
  (electric-pair-local-mode 1))

(add-hook 'emacs-lisp-mode-hook #'my/hook--lisp-mode)
(add-hook 'lisp-mode-hook #'my/hook--lisp-mode)

;;;; [MARKDOWN]
(add-to-list 'load-path (my/get-packages-file "markdown-mode"))
(autoload #'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files." t)
(add-to-list 'auto-mode-alist '("\\.\\(?:md\\|txt\\)\\'" . markdown-mode))

(defun my/hook--markdown-mode ()
  "Configuration for `markdown-mode'."
  (define-key markdown-mode-map [remap backward-paragraph] 'backward-paragraph)
  (define-key markdown-mode-map [remap forward-paragraph] 'forward-paragraph)
  (setq-local indent-line-function 'tab-to-tab-stop)
  (keymap-set markdown-mode-map "TAB" 'indent-for-tab-command)
  (my/lang-indent-set-local 'markdown)
  (my/paragraph-default-movement-local)
  (visual-line-mode 1))
(add-hook 'markdown-mode-hook #'my/hook--markdown-mode)

;;;; [RST: RESTRUCTURED TEXT]
(setq-default rst-indent-width (my/lang-indent-size 'rst))
(defun my/rst-mode-hook--setup ()
  "Configuration for `rst-mode'."
  (my/paragraph-default-movement-local)
  (setq-local indent-line-function #'tab-to-tab-stop)
  (setq-local rst-indent-width (my/lang-indent-size 'rst))
  (my/lang-indent-set-local 'rst)
  (visual-line-mode 1))
(add-hook 'rst-mode-hook #'my/rst-mode-hook--setup)

;;;; [TEX]
(defun my/tex-mode-hook--setup ()
  "Configuration for `tex-mode'"
  (setq-local indent-line-function 'tab-to-tab-stop)
  (my/lang-indent-set-local 'tex)
  (visual-line-mode 1))
(add-hook 'tex-mode-hook 'my/tex-mode-hook--setup)

;;;; [WEB HTML/CSS/JS]
(add-to-list 'load-path (my/get-packages-file "web-mode"))
(autoload 'web-mode "web-mode"
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
(add-hook 'web-mode-hook #'my/web-mode-hook--setup)

;;;; [XML / HTML]
(setq-default sgml-basic-offset (my/lang-indent-size 'html))
(defun my/hook--xml-mode ()
  "Configuration for `html-mode'."
  (my/paragraph-default-movement-local)
  (my/lang-indent-set-local 'html)
  (setq-local sgml-basic-offset (my/lang-indent-size 'html)))

(add-hook 'xml-mode-hook #'my/hook--xml-mode)
(add-hook 'html-mode-hook #'my/hook--xml-mode)

;;;; [YAML]
(add-to-list 'load-path (my/get-packages-file "yaml-mode"))
(autoload #'yaml-mode "yaml-mode"
  "Major mode for editing YAML files." t)
(add-to-list 'auto-mode-alist '("\\.\\(?:yml\\|yaml\\)\\'" . yaml-mode))

(defun my/hook--yaml-mode ()
  "Configuration for `yaml-mode'."
  (my/lang-indent-set-local 'yaml)
  (setq-local yaml-indent-offset (my/lang-indent-size 'yaml)))

;;; [END OF INIT.EL]
(load (my/get-etc-file "machine-init.el") :no-error-if-file-is-missing :nomessage)

;; ══════════════════════════════════════════════════════════════════════
;;
;;    _|_|_|                            _|  _|
;;  _|          _|_|      _|_|      _|_|_|  _|_|_|    _|    _|    _|_|
;;  _|  _|_|  _|    _|  _|    _|  _|    _|  _|    _|  _|    _|  _|_|_|_|
;;  _|    _|  _|    _|  _|    _|  _|    _|  _|    _|  _|    _|  _|
;;    _|_|_|    _|_|      _|_|      _|_|_|  _|_|_|    _|_|_|_|   _|_|_|
;;                                                          _|
;;                                                       _|_|
;; ══════════════════════════════════════════════════════════════════════
