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

(defconst my/lisp-modules-directory-list '("lisp" "modules")
  "Directories relative to `user-emacs-directory' containing lisp files to
load.")

(defconst my/var-directory (locate-user-emacs-file "var")
  "Directory to store emacs' variable data.")

(defconst my/etc-directory (locate-user-emacs-file "etc")
  "Directory to store other emacs files such as extra configuration.")

(defconst my/auto-save-files-directory (expand-file-name "auto-saves" my/var-directory)
  "Directory to store temporary auto-save files.")

(defconst my/packages-directory (locate-user-emacs-file "packages")
  "Directory to store external emacs packages that are locally installed.")

(defconst my/packages-load-list
  '("compat"
    "nerd-icons"
    "nerd-icons-dired"
    "markdown-mode"
    "olivetti")
  "Package directories to add to `load-path', found in
`my/packages-directory'.")

(defcustom my/lang-indent-settings
  '((cc   :size 4 :use-tabs nil)
    (css  :size 4 :use-tabs nil)
    (go   :size 8 :use-tabs   t)
    (js   :size 4 :use-tabs nil)
    (json :size 4 :use-tabs nil)
    (lisp :size 8 :use-tabs nil)
    (md   :size 2 :use-tabs nil)
    (org  :size 8 :use-tabs nil)
    (ts   :size 4 :use-tabs nil)
    (xml  :size 4 :use-tabs nil)
    (yaml :size 2 :use-tabs nil))
  "List of language-specific indentation settings. Access values using the
functions`my/lang-indent-size' and `my/lang-indent-use-tabs'.

Elements of this alist are of the form:

  (LANG-SYMBOL [:size SIZE] [:use-tabs USE-TABS])

where LANG-SYMBOL is a unique key name that represents a language, SIZE
is the width of each indent in columns, and USE-TABS is a boolean where
if non-nil, indentation will use tabs instead of spaces."
  :group 'indent)

(defcustom my/indent-size-default 4
  "Size of indentation, in columns. Wrapper around `tab-width'."
  :group 'indent
  :type '(natnum))

(defcustom my/indent-use-tabs-default nil
  "If non-nil, indentation will use tabs instead of spaces. Wrapper around
`indent-tabs-mode'."
  :group 'indent
  :type '(boolean))

(defcustom my/font-size-default 120
  "Default font size (height for the `default' face)"
  :group 'face
  :type '(natnum))

(defun my/lang-indent-size (lang)
  "Get the size of indentation, in columns, for LANG, where LANG is a
symbol and a key to the `my/lang-indent-settings' list.

If the key or the size property of the language does not exist, then
return the default indentation size defined in `my/indent-size-default'."
  (let ((val (cdr (assoc lang my/lang-indent-settings))))
    (plist-get val :size)))

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

(defun my/locate-user-var-file (path)
  "Return absolute file path of PATH relatie to `my/var-directory'"
  (expand-file-name (convert-standard-filename path) my/var-directory))

(defun my/locate-user-etc-file (path)
  "Return absolute file path of PATH relative to `my/etc-directory'"
  (expand-file-name (convert-standard-filename path) my/etc-directory))

(defun my/goto-config-init ()
  "Jump to user's init.el `user-init-file'"
  (interactive)
  (find-file user-init-file))

(defun my/open-in-terminal ()
  "Open the current directory in the terminal

Credit: http://xahlee.info/emacs/emacs/emacs_open_in_terminal.html"
  (interactive)
  (cond
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

(defun my/set-theme (theme)
  "Set the current emacs theme to THEME. Disables all other themes."
  (interactive (list (intern (completing-read "Theme: " (custom-available-themes)))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (enable-theme theme))

;;; [LISP MODULES]

(dolist (dir my/lisp-modules-directory-list)
  (add-to-list 'load-path (locate-user-emacs-file dir)))

;; Add packages to `load-path'
(dolist (package my/packages-load-list)
  (add-to-list 'load-path (expand-file-name package my/packages-directory)))

;;; [BASE CUSTOMIZATION VARIABLES]

;; Customization variables
(setopt ad-redefinition-action 'accept) ; disable warning about advice de/activation
(setopt backward-delete-char-untabify-method 'hungry)
(setopt bookmark-default-file (my/locate-user-etc-file "bookmarks"))
(setopt completion-ignore-case t)
(setopt delete-by-moving-to-trash t)
(setopt enable-recursive-minibuffers t)
(setopt fast-but-imprecise-scrolling t)
(setopt fill-column 70)
;(setopt grep-command "rg -nHS --no-heading --null ") ; errors out for some reason (on Windows)
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
(setopt tab-width my/indent-size-default)
(setopt undo-limit (* 13 160000))
(setopt undo-strong-limit (* 13 240000))
(setopt undo-outer-limit (* 13 24000000))
(setopt uniquify-buffer-name-style 'forward)
(setopt uniquify-ignore-buffers-re "^\\*")
(setopt uniquify-separator "/")
(setopt use-short-answers t)
(setopt visible-bell nil) ; disable visual indicator of invalid input
(setopt word-wrap t)

;; Non-customization variables
(setq auto-window-vscroll nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; [AUTOSAVES, BACKUPS AND LOCKFILES]
(setq auto-save-list-file-prefix (expand-file-name "sessions" my/auto-save-files-directory))
(setq auto-save-file-name-transforms `((".*" ,my/auto-save-files-directory t)))
(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)

;;; [FONTS]

;; 👋 Display emojis 🖥️⌨️🖱️
(defun my/emacs--set-emoji-font ()
  (set-fontset-font
   t 'emoji
   (cond
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Symbola" (font-family-list)) "Symbola"))))
(my/emacs--set-emoji-font)

;;; [THEME]
(setopt modus-themes-italic-constructs t)
(setopt modus-themes-bold-constructs t)
(setopt modus-themes-common-palette-overrides
        '((fg-line-number-inactive "gray50")
          (fg-line-number-active fg-main)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)))
(my/set-theme 'modus-operandi)

;;; [KEYBINDINGS]
(keymap-global-set "<escape>" 'keyboard-escape-quit)
(keymap-global-set "M-[" 'backward-paragraph)
(keymap-global-set "M-]" 'forward-paragraph)
(keymap-global-set "C-z" nil)
;(keymap-global-set "C-x C-k RET" nil)
(keymap-global-set "C-x C-z" nil)
(keymap-global-set "M-s M-s" 'grep)

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
;;  ██╗  ██╗██╗███╗   ███╗ ;;
;;  ██║  ██║██║████╗ ████║ ;;
;;  ██║  ██║██║██╔████╔██║ ;;
;;  ╚██╗██╔╝██║██║╚██╔╝██║ ;;
;;   ╚███╔╝ ██║██║ ╚═╝ ██║ ;;
;;    ╚══╝  ╚═╝╚═╝     ╚═╝ ;;
;;                         ;;
;;  Startup time :  %.2fs  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

"
                    (float-time
                     (time-subtract after-init-time before-init-time)))))

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
(setopt mode-line-right-align-edge 'window)
(setopt mode-line-percent-position nil)
(setopt mode-line-position-line-format '("L%l"))
(setopt mode-line-position-column-line-format '("%l:%c"))
(setopt mode-line-compact t)

(add-hook 'prog-mode-hook #'column-number-mode)

;;; [AUTO REVERT BUFFERS]
(setopt global-auto-revert-non-file-buffers t)
(add-hook 'after-init-hook #'global-auto-revert-mode)

;;; [COMPILE]
(setopt compilation-always-kill t)
(setopt compilation-scroll-output t)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;;; [COMPLETION AND MINIBUFFER]
(defun my/minibuffer--backward-kill (arg)
  "When minibuffer is completing a file name, delete up to parent
folder, otherwise delete a word."
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (kill-word (- arg))))

(let ((map minibuffer-local-map))
  (keymap-set map "C-<backspace>" #'my/minibuffer--backward-kill)
  (keymap-set map "M-<backspace>" #'my/minibuffer--backward-kill))

(setopt icomplete-show-matches-on-no-input t)
(setopt icomplete-delay-completions-threshold 0)
(setopt icomplete-compute-delay 0)
(setopt icomplete-max-delay-chars 0)
(setopt icomplete-scroll t)
(icomplete-vertical-mode 1)
(keymap-set icomplete-minibuffer-map "TAB" #'icomplete-force-complete)
(keymap-set icomplete-minibuffer-map "C-M-i" #'minibuffer-complete)
(advice-add 'completion-at-point :after #'minibuffer-hide-completions)

;;; [DIRED]
(setopt dired-hide-details-hide-symlink-targets nil)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;;; [IBUFFER BUFFER LIST]
(setopt ibuffer-saved-filter-groups
        '(("Default"
           ("Emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")
                     (name . "^\\*Warnings\\*$")
                     (name . "^\\*Shell Command Output\\*$")
                     (name . "^\\*Async-native-compile-log\\*$")))
           ("Dired" (mode . dired-mode))
           ("Terminal" (or
                        (mode . term-mode)
                        (mode . shell-mode)
                        (mode . eshell-mode)))
           ("Help" (or
                    (mode . help-mode)
                    (name . "^\\*Help\\*$")
                    (name . "^\\*info\\*$"))))))
(setopt ibuffer-show-empty-filter-groups nil)
(keymap-global-set "C-x C-b" 'ibuffer)

(defun my/hook--ibuffer-mode ()
  "Configuration for `ibuffer-mode'."
  (ibuffer-switch-to-saved-filter-groups "default"))
(add-hook 'ibuffer-mode-hook #'my/hook--ibuffer-mode)

;;; [WHITESPACE]
(setopt whitespace-display-mappings '((tab-mark 9 [#x21e5 9] [92 9])))
(setopt whitespace-style '(face tabs tab-mark trailing))
(add-hook 'text-mode-hook #'whitespace-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)

;;; [OTHER LISP]
;;;; [SUBTLE MODE LINE COLORS]
(autoload #'my/subtle-mode-line-colors-mode "my-subtle-mode-line-colors-mode"
  "Minor mode for making mode line colors more subtle." t)
(add-hook 'after-init-hook #'my/subtle-mode-line-colors-mode)

;;;; [TOGGLE MODE-LINE]
(autoload #'my/toggle-mode-line-mode "my-toggle-mode-line-mode"
  "Minor mode for toggling the visibility of the mode-line." t)
(autoload #'my/hide-mode-line "my-toggle-mode-line-mode"
  "Minor mode for hiding the mode-line." t)
(autoload #'my/show-mode-line "my-toggle-mode-line-mode"
  "Minor mode for showing the mode-line." t)

;;; [OTHER PACKAGES]
;;; [NERD ICONS DIRED]
(autoload #'nerd-icons-dired-mode "nerd-icons-dired"
  "Minor mode for adding nerd font icons in `dired-mode'." t)

(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;;;; [OLIVETTI]
(autoload #'olivetti-mode "olivetti"
  "Minor mode for providing a nice writing environment." t)

(defun my/writeroom-enable ()
  "Start nice writing environment."
  (interactive)
  (my/toggle-mode-line-mode 1) ; TODO: make this buffer-local action
  (olivetti-mode 1))

(defun my/writeroom-disable ()
  "Quit nice writing environment."
  (interactive)
  (my/toggle-mode-line-mode -1)
  (olivetti-mode -1))

;;; [LANGUAGES]
;;;; [C / C++]
(defun my/hook--cc-mode ()
  "Settings for `c-mode' and `c++-mode'"
  (c-set-style "bsd")
  (my/lang-indent-set-local 'cc)
  (setq-local c-basic-offset (my/lang-indent-size 'cc)))

(add-hook 'c-mode-hook #'my/hook--cc-mode)
(add-hook 'c++-mode-hook #'my/hook--cc-mode)

;;;; [CSS]
(defun my/hook--css-mode ()
  "Configuration for `css-mode'."
  (my/lang-indent-set-local 'css)
  (setq-local css-indent-offset (my/lang-indent-size 'css)))
(add-hook 'css-mode-hook #'my/hook--css-mode)

;;;; [JAVASCRIPT]
(defun my/hook--js-mode ()
  "Configuration for `js-mode' and `js-jsx-mode'."
  (my/lang-indent-set-local 'js)
  (setq-local js-indent-level (my/lang-indent-size 'js)))

(add-hook 'js-mode-hook #'my/hook--js-mode)
(add-hook 'js-jsx-mode-hook #'my/hook--js-mode)

;;;; [JSON]
(defun my/hook--js-json-mode ()
  "Configuration for `js-json-mode'"
  (my/lang-indent-set-local 'json))

(add-hook 'js-json-mode-hook #'my/hook--js-json-mode)

;;;; [LISP]
(defun my/hook--lisp-mode ()
  "Configuration for lisp-like languages such as `lisp-mode' and
`emacs-lisp-mode'."
  (my/lang-indent-set-local 'lisp)
  (electric-pair-local-mode 1))

(add-hook 'emacs-lisp-mode-hook #'my/hook--lisp-mode)
(add-hook 'lisp-mode-hook #'my/hook--lisp-mode)

;;;; [MARKDOWN]
(autoload #'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files." t)
(add-to-list 'auto-mode-alist '("\\.\\(?:md\\|txt\\)\\'" . markdown-mode))

(defun my/hook--markdown-mode ()
  "Configuration for `markdown-mode'."
  (my/lang-indent-set-local 'md)
  (visual-line-mode 1))
(add-hook 'markdown-mode-hook #'my/hook--markdown-mode)

;;;; [XML / HTML]
(defun my/hook--xml-mode ()
  "Configuration for `html-mode'."
  (my/lang-indent-set-local 'html)
  (setq-local sgml-basic-offset (my/lang-indent-size 'html)))

(add-hook 'xml-mode-hook #'my/hook--xml-mode)
(add-hook 'html-mode-hook #'my/hook--xml-mode)

;;; [END OF INIT.EL]
(load (locate-user-emacs-file (my/locate-user-etc-file "machine-init.el")) :noerror :nomessage)

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
