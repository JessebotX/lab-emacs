;;; -*- lexical-binding: t; -*-

(require 'my-core)

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

(defun my/open-terminal ()
  "Open the current dir in a new terminal window.

URL `http://xahlee.info/emacs/emacs/emacs_open_in_terminal.html'
Version: 2020-11-21 2022-08-04 2023-03-01 2023-06-26"
  (interactive)
  (let ((shell-dir (shell-quote-argument (expand-file-name default-directory))))
    (cond
     ((eq system-type 'windows-nt)
      (shell-command (format "wt -d \"%s\"" default-directory)))
     ((eq system-type 'darwin)
      (shell-command (concat "open -a terminal " shell-dir)))
     ((eq system-type 'gnu/linux)
      (call-process "setsid" nil 0 nil "x-terminal-emulator" (concat "--working-directory=" shell-dir)))
     ((eq system-type 'berkeley-unix)
      (call-process "setsid" nil 0 nil "x-terminal-emulator" (concat "--working-directory=" shell-dir))))))

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

;;; MINIBUFFER & COMPLETIONS

(defun my/minibuffer--backward-kill (arg)
  "When minibuffer is completing a file name, delete up to parent
folder, otherwise delete a word."
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (kill-word (- arg))))

(my/set completion-ignore-case t)
(my/set completions-detailed t)

;;; FONTS & THEMES

(defcustom my/font-family "Maple Mono"
  "Default font family.")

(defcustom my/font-size 140
  "Default font size.")

(defun my/font-size-set (value)
  "Set the base font size to VALUE (integer)."
  (interactive "nNew font size: ")
  (set-face-attribute 'default (selected-frame) :height value))

(defun my/font-size-decrement ()
  "Decrement base font size by 10."
  (interactive)
  (let* ((font-size (face-attribute 'default :height))
         (new-size  (- font-size 10)))
    (set-face-attribute 'default (selected-frame) :height new-size)
    (message "New font size %d" (face-attribute 'default :height))))

(defun my/font-size-increment ()
  "Increment base font size by 10."
  (interactive)
  (let* ((font-size (face-attribute 'default :height))
         (new-size  (+ font-size 10)))
    (set-face-attribute 'default (selected-frame) :height new-size)
    (message "New font size %d" (face-attribute 'default :height))))

(defun my/font-family-set (font)
  "Set emacs `default' face's font family."
  (interactive (list (completing-read "Font: " (font-family-list))))
  (set-face-attribute 'default nil :family font))

(defun my/font-family-variable-pitch-set (font)
  "Set emacs `variable-pitch' face's font family."
  (interactive (list (completing-read "Font: " (font-family-list))))
  (set-face-attribute 'variable-pitch nil :family font))

(defun my/font-load-my-font ()
  "Set `default' font face using `my/font-family' and `my/font-size'."
  (interactive)
  (set-face-attribute 'default nil :family my/font-family :height my/font-size))

(defun my/font-load-emoji-fonts ()
  "Enable fonts for emojis."
  (interactive)
  (set-fontset-font
   t 'emoji
   (cond
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Symbola" (font-family-list)) "Symbola"))))

;;; THEMES

(defcustom my/theme 'modus-operandi-tinted
  "Default Emacs theme.")

(defcustom my/theme-toggle-options '(modus-operandi-tinted modus-vivendi-tinted)
  "Two Emacs themes to toggle between that are available for
loading (`custom-available-themes').")

(defun my/theme-set (theme)
  "Set the current emacs theme to THEME. Disables all other themes."
  (interactive
   (list (intern (completing-read "Theme: " (custom-available-themes)))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (enable-theme theme))

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

(with-eval-after-load 'modus-themes
  (my/set modus-themes-italic-constructs t)
  (my/set modus-themes-bold-constructs t)
  (my/set modus-themes-common-palette-overrides
          '((fg-line-number-inactive "gray50")
            (fg-line-number-active fg-main)

            (bg-line-number-inactive unspecified)
            (bg-line-number-active unspecified)

            (bg-mode-line-active bg-main)
            ;; (bg-mode-line-active bg-dim)
            (fg-mode-line-active fg-main)

            (border-mode-line-active bg-dim)
            (border-mode-line-inactive bg-mode-line-inactive)))
  (my/set modus-vivendi-tinted-palette-overrides
          '((bg-main "#111111")
            (bg-dim "#333333")
            (fg-main "#dddddd")
            (border "#333333")
            (bg-paren-match "#454545")
            (cursor blue))))

;;; MODE-LINE

(with-eval-after-load 'time
  (my/set display-time-default-load-average nil))
(with-eval-after-load 'project
  (my/set project-mode-line t))

(autoload 'my/mode-line-mode "my-mode-line")

;;;; Buffer location display
(my/set mode-line-percent-position nil)
(my/set mode-line-position-line-format '("(%l:)"))
(my/set mode-line-position-column-format '("(:%c)"))
(my/set mode-line-position-column-line-format '("(%l:%c)"))

;;;###autoload
(define-minor-mode my/mode-line-display-position-mode
  "Toggle displaying local buffer position in the mode line."
  :group 'mode-line
  :global t
  (if my/mode-line-display-position-mode
      (progn
        (line-number-mode 1)
        (column-number-mode 1))
    (progn
      (line-number-mode -1)
      (column-number-mode -1))))

;;; OLIVETTI (WITH CUSTOM WRITE-ROOM FOCUS MODE)

(let* ((package-path (my/locate-user-lisp-file "olivetti"))
       (package-exists-p (file-directory-p package-path)))
  (when package-exists-p
    (autoload #'olivetti-mode "olivetti" nil t)

    (add-hook 'olivetti-mode-hook
              (defun my/--olivetti-no-newline-in-fringe ()
                "Hack to prevent cursor from going into the fringe."
                (setq-local overflow-newline-into-fringe nil)))

    ;; Disable some mouse keybinds
    (with-eval-after-load 'olivetti
      (define-key olivetti-mode-map [left-margin mouse-1] nil)
      (define-key olivetti-mode-map [right-margin mouse-1] nil)
      (define-key olivetti-mode-map [left-fringe mouse-1] nil)
      (define-key olivetti-mode-map [right-fringe mouse-1] nil)

      (define-key olivetti-mode-map [left-margin mouse-2] nil)
      (define-key olivetti-mode-map [right-margin mouse-2] nil)
      (define-key olivetti-mode-map [left-fringe mouse-2] nil)
      (define-key olivetti-mode-map [right-fringe mouse-2] nil)

      (define-key olivetti-mode-map [left-margin mouse-3] nil)
      (define-key olivetti-mode-map [right-margin mouse-3] nil)
      (define-key olivetti-mode-map [left-fringe mouse-3] nil)
      (define-key olivetti-mode-map [right-fringe mouse-3] nil)

      ;; This code is taken from https://github.com/joostkremers/visual-fill-column
      (when (and (bound-and-true-p mouse-wheel-mode)
                 (boundp 'mouse-wheel-down-event)
                 (boundp 'mouse-wheel-up-event))
        (define-key olivetti-mode-map (vector 'left-margin 'mouse-wheel-down-event) nil)
        (define-key olivetti-mode-map (vector 'left-margin 'mouse-wheel-up-event) nil)
        (define-key olivetti-mode-map (vector 'right-margin 'mouse-wheel-down-event) nil)
        (define-key olivetti-mode-map (vector 'right-margin 'mouse-wheel-up-event) nil)))

    ;;;###autoload
    (define-minor-mode my/focus-mode
      "Minor mode that toggles a nice writing environment."
      :init-value nil
      (if my/focus-mode
          (progn
            (mode-line-invisible-mode 1)
            (whitespace-mode -1)
            (olivetti-mode 1))
        (progn
          (mode-line-invisible-mode -1)
          (whitespace-mode 1)
          (olivetti-mode -1))))

    (make-variable-buffer-local 'my/focus-mode)))

;;; BASE CONFIGURATION

;;;; EMACS DATA FILES

(my/set auto-save-file-name-transforms `((".*" ,(my/locate-user-var-file "auto-saves") t)))
(my/set auto-save-list-file-prefix (expand-file-name "sessions" (my/locate-user-var-file "auto-saves")))
(my/set create-lockfiles nil)
(my/set make-backup-files nil)
(my/set kill-buffer-delete-auto-save-files t)

(my/set custom-file (my/locate-user-etc-file "emacs-custom.el"))
(my/set multisession-directory (my/locate-user-var-file "multisession"))
(my/set transient-history-file (my/locate-user-var-file "transient/history.el"))
(my/set treesit-extra-load-path (list (my/locate-user-var-file "tree-sitter")))

;;;; AUTO REFRESH UPDATED BUFFERS

(my/set global-auto-revert-non-file-buffers t)

;;;; BOOKMARKS, RECENTS, HISTORY

(my/set bookmark-default-file (my/locate-user-var-file "bookmarks.el"))
(my/set recentf-save-file (my/locate-user-var-file "recentf.el"))
(my/set save-place-file (my/locate-user-var-file "save-place.el"))
(my/set savehist-file (my/locate-user-var-file "savehist.el"))
(my/set history-length 300)
(my/set save-place-limit 600)

;;;; IBUFFER

(my/set ibuffer-human-readable-size t)

;;;; DELETE PAIRS

(my/set delete-pair-blink-delay 0)
(my/set delete-pair-push-mark t)

;;;; UNIQUE BUFFER NAMING

(my/set uniquify-buffer-name-style 'forward)
(my/set uniquify-ignore-buffers-re "^\\*")
(my/set uniquify-separator "/")

;;;; PROJECT MANAGEMENT

(my/set ansi-color-for-compilation-mode t)
(my/set comint-scroll-to-bottom-on-input t)
(my/set comint-scroll-to-bottom-on-output nil)
(my/set compilation-always-kill t)
(my/set compilation-scroll-output 'first-error)
(my/set project-list-file (my/locate-user-var-file "project.el"))

;;;; DOCUMENT VIEWER

(my/set doc-view-resolution 200)

;;;; DICTIONARY

(my/set dictionary-server "dict.org")
(my/set dictionary-default-strategy "prefix")

;;;; EDITING TEXT BASICS

(my/set backward-delete-char-untabify-method 'hungry)
(my/set tab-width 3)
(my/set indent-tabs-mode nil)
(my/set sentence-end-double-space nil)
(my/set kill-do-not-save-duplicates t)
(my/set kill-region-dwim (if (version< emacs-version "31") t 'emacs-word))

;;;; FILE MANAGEMENT

(my/set dired-kill-when-opening-new-dired-buffer t)
(my/set dired-listing-switches "-g -h -o -a --group-directories-first")
(my/set delete-by-moving-to-trash t)

;;;; FIND / SEARCH

(my/set isearch-lazy-count t)
(my/set isearch-allow-scroll 'unlimited)
(my/set lazy-count-prefix-format nil)
(my/set lazy-count-suffix-format "   (%s/%s)")

;;;; HIGHLIGHT MATCHING PARENS

(my/set show-paren-delay 0.1)
(my/set show-paren-highlighting-openparen t)
(my/set show-paren-when-point-inside-paren t)
(my/set show-paren-when-point-in-periphery t)

;;;; SCROLLING

(my/set auto-window-vscroll nil)
(my/set fast-but-imprecise-scrolling t)
(my/set scroll-conservatively 20)
(my/set scroll-margin 3)
(my/set scroll-preserve-screen-position t)
(my/set scroll-error-top-bottom t)
(my/set scroll-up-aggressively 0.01) ; keep point near edge of screen
(my/set scroll-down-aggressively 0.01) ; keep point near edge of screen

;;;; SHORT ANSWER PROMPTS

(my/set read-answer-short t)
(my/set use-short-answers t)

;;;; WHITESPACE

(my/set whitespace-display-mappings '((tab-mark 9 [#x7C 9] [92 9])))
(my/set whitespace-style '(face tabs tab-mark trailing))
(my/set whitespace-line-column nil)

;;;; LINE NUMBERS

(my/set display-line-numbers-width 4)
(my/set display-line-numbers-widen t)

(my/set undo-limit 2080000)
(my/set undo-strong-limit 3120000)
(my/set undo-outer-limit 312000000)

;;;; ERROR/WARNING BELLS

(my/set ring-bell-function #'ignore)
(my/set visible-bell nil)

;;;; MISCELLANEOUS

(my/set adaptive-fill-regexp "[-–!|#%;>*+·•‣⁃◦ 	]* +")
(my/set view-lossage-auto-refresh t)

(put 'narrow-to-region 'disabled nil)

;;; EDITING

(require 'my-config-editor-langs)
(with-eval-after-load 'my-config-editor-langs
  (global-set-key [remap delete-backward-char] #'my/editor-delete-to-tab-stop)
  (global-set-key [remap delete-backward-char-untabify] #'my/editor-delete-to-tab-stop))

;;;; KEYBINDINGS

(keymap-global-set "C-z" nil)
(keymap-global-set "C-x C-z" nil)
(keymap-global-set "C-x C-k RET" nil)
(keymap-global-set "C-c C-b" nil)
(keymap-global-set "<mouse-3>" nil)

(keymap-global-set "<f5>" #'project-compile)
(keymap-global-set "C-<f5>" #'compile)
(keymap-global-set "M-[" #'backward-paragraph)
(keymap-global-set "M-]" #'forward-paragraph)
(keymap-global-set "M-s M-s" #'grep)
(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "C-c d" #'dictionary-lookup-definition)
(keymap-global-set "C-c -" #'kill-buffer-and-window)
(keymap-global-set "C-c C-SPC" #'just-one-space)
(keymap-global-set "C-c f b" #'bookmark-jump)
(with-eval-after-load 'recentf
  (keymap-global-set "C-c f r" #'recentf))
(keymap-global-set "C-c f p" #'project-find-file)
(keymap-global-set "C-c m l" #'display-line-numbers-mode)
(keymap-global-set "C-c m w" #'whitespace-mode)
(keymap-global-set "C-c m t w" #'my/focus-mode)

(keymap-global-set "<escape>" #'my/keyboard-quit-dwim)
(keymap-global-set "C-g" #'my/keyboard-quit-dwim)
(keymap-global-set "C-c C-0" #'my/font-size-set)
(keymap-global-set "C-c C-1" #'my/font-family-set)
(keymap-global-set "C-c f f" #'my/switch-frame)

(keymap-set minibuffer-local-map "C-<backspace>" #'my/minibuffer--backward-kill)
(keymap-set minibuffer-local-map "M-<backspace>" #'my/minibuffer--backward-kill)

;;; HOOKS & OTHER

(add-hook 'emacs-startup-hook
          (defun my/--emacs-startup ()
            (my/theme-load-my-theme)
            (my/font-load-my-font)
            (my/font-load-emoji-fonts)))

(add-hook 'after-init-hook
          (defun my/--after-init ()
            (advice-add #'split-window-below :after (lambda (&rest _) (other-window 1)))
            (advice-add #'split-window-right :after (lambda (&rest _) (other-window 1)))

            (global-auto-revert-mode 1)
            (save-place-mode 1)
            (savehist-mode 1)
            (delete-selection-mode 1)
            (pixel-scroll-precision-mode 1)
            (winner-mode 1)
            (my/mode-line-mode 1)
            (my/mode-line-display-position-mode 1)

            (blink-cursor-mode -1)
            (electric-indent-mode -1)))

(add-hook 'enable-theme-functions
          (defun my/themes--bold-dired-directory (_theme)
            (with-eval-after-load 'dired
              (set-face-attribute 'dired-directory nil :weight 'bold))))

(add-hook 'prog-mode-hook
          (defun my/--prog-mode ()
            (whitespace-mode)))

(add-to-list 'save-some-buffers-action-alist
             (list "d"
                   (lambda (buffer)
                     (diff-buffer-with-file (buffer-file-name buffer)))
                   "show diff between the buffer and its file"))

;;; END

(load (my/locate-user-etc-file "local-init.el") :no-error-if-file-is-missing :nomessage)
