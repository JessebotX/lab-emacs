;;; -*- lexical-binding: t; -*-
;;
;; ═════════════════════════════[ Start ]═════════════════════════════
;;
;;              _|    _|              _|   _|
;;              _|    _|     _|_|     _|   _|     _|_|
;;              _|_|_|_|   _|_|_|_|   _|   _|   _|    _|
;;              _|    _|   _|         _|   _|   _|    _|
;;              _|    _|     _|_|_|   _|   _|     _|_|
;;
;; ═════════════════════════════[ Emacs ]═════════════════════════════

;;; PREFACE

(defvar my/lisp-modules-directory-names
  '("lisp"
    "modules")
  "Directory base names containing lisp files to load, found in
`user-emacs-directory'")

;; Load lisp directories found in `user-emacs-directory'
(dolist (lisp-directory my/lisp-modules-directory-names)
  (add-to-list 'load-path (locate-user-emacs-file lisp-directory)))

;;; PACKAGE MANAGEMENT

(require 'my-elpaca-pkgm-setup)

;;; MODULES
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

(defun my/lang-indent-size (lang)
  "Get the size of indentation, in columns, for LANG, where LANG is a
symbol and a key to the `my/lang-indent-settings' list.

If the key or the size property of the language does not exist, then
return the default indentation size defined in `my/indent-size-default'."
  (let ((val (cdr (assoc lang my/lang-indent-settings))))
    (if val
        (plist-get val :size)
      my/indent-size-default)))

(defun my/lang-indent-use-tabs (lang)
  "Get whether indentation will use tabs instead of spaces on
indent for LANG, where LANG is a symbol and a key to the
`my/lang-indent-settings' list.

If the key or the use-tabs property of the language does not exist
then return the default use-tabs value defined in
`my/indent-use-tabs-default'."
  (let ((val (cdr (assoc lang my/lang-indent-settings))))
    (if val
        (plist-get val :use-tabs)
      my/indent-use-tabs-default)))

(defun my/config-indent (size use-tabs)
  "Configure buffer-local indentation settings, where SIZE is the
indentation size in columns, and USE-TABS is a boolean where if non-nil,
tabs will be used instead of spaces.

Wrapper around `my/config-indent-spaces' and `my/config-indent-tabs'."
  (interactive
   (list
    (read-number "Indent size (# of columns): ")
    (y-or-n-p "Use tabs instead of spaces")))
  (if use-tabs
      (my/config-indent-tabs size)
    (my/config-indent-spaces size)))

(defun my/config-indent-tabs (size)
  "Set buffer-local indentation settings to tabs with a column width of
SIZE."
  (setq-local tab-width size
              indent-tabs-mode t))

(defun my/config-indent-spaces (size)
  "Set buffer-local indentation settings to expand tabs into SIZE spaces."
  (setq-local tab-width size
              indent-tabs-mode nil))

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

(setopt ad-redefinition-action 'accept) ; disable warning about advice de/activation
(setopt backward-delete-char-untabify-method 'hungry)
(setopt completion-ignore-case t)
(setopt delete-by-moving-to-trash t)
(setopt enable-recursive-minibuffers t)
(setopt fast-but-imprecise-scrolling t)
(setopt history-length 300)
(setopt indent-tabs-mode my/indent-use-tabs-default)
(setopt isearch-lazy-count t)
(setopt jit-lock-defer-time 0)
(setopt kill-do-not-save-duplicates t)
(setopt lazy-count-prefix-format nil)
(setopt lazy-count-suffix-format "   (%s/%s)")
(setopt read-answer-short t)
(setopt ring-bell-function #'ignore) ; disable sound on invalid input
(setopt scroll-conservatively 101)
(setopt scroll-perserve-screen-position t)
(setopt tab-width my/indent-size-default)
(setopt uniquify-buffer-name-style 'forward)
(setopt uniquify-ignore-buffers-re "^\\*")
(setopt uniquify-separator "/")
(setopt use-short-answers t)
(setopt visible-bell nil) ; disable visual indicator of invalid input

;; Non-customization variables

(setq auto-window-vscroll nil)
(setq-default create-lockfiles nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq-default make-backup-files nil)

(load custom-file :noerror :nomessage)

;; Modes
(blink-cursor-mode -1)
(delete-selection-mode 1)

;; Window split focus on new window
(advice-add #'split-window-below :after (lambda (&rest _) (other-window 1)))
(advice-add #'split-window-right :after (lambda (&rest _) (other-window 1)))

(with-current-buffer (get-buffer-create "*scratch*")
  (insert
   (format ";;
;;  ██╗   ██╗██╗███╗   ███╗
;;  ██║   ██║██║████╗ ████║
;;  ██║   ██║██║██╔████╔██║
;;  ╚██╗ ██╔╝██║██║╚██╔╝██║
;;   ╚████╔╝ ██║██║ ╚═╝ ██║
;;    ╚═══╝  ╚═╝╚═╝     ╚═╝
;;  ⦿ Loading time : %.2fs
;;
"
           (float-time (time-subtract after-init-time before-init-time)))))

(keymap-global-set "<escape>" 'keyboard-escape-quit)
(keymap-global-set "C-x C-b" 'ibuffer)
;(keymap-global-set "C-c C-s C-f" 'my/goto-config-init)
(keymap-global-set "M-]" 'forward-paragraph)
(keymap-global-set "M-[" 'backward-paragraph)
(keymap-global-set "C-c C-f" nil)

;;; AUTO REVERT MODE
(setopt global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;;; DISPLAY LINE NUMBERS MODE
(setopt display-line-numbers-type 'relative)
(setopt display-line-numbers-widen t)
(setopt display-line-numbers-width 3)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;; ICOMPLETE
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
  (define-key map (kbd "C-<backspace>") #'my/minibuffer--backward-kill)
  (define-key map (kbd "M-<backspace>") #'my/minibuffer--backward-kill))

(setopt icomplete-show-matches-on-no-input t)
(setopt icomplete-delay-completions-threshold 0)
;(icomplete-vertical-mode 1)

;;; WHITESPACE
(setopt whitespace-display-mappings '((tab-mark 9 [#x21e5 9] [92 9])))
(setopt whitespace-style '(face tabs tab-mark trailing))

(add-hook 'text-mode-hook #'whitespace-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)

;; FONT
(defun my/set-default-font-family (family)
  "Set the default face family to FAMILY, where FAMILY is a string
specifying the font family name. If font family does not exist, nothing
will occur."
  (interactive "sFont family: ")
  (set-face-attribute 'default nil :family family))

(set-face-attribute 'default nil :family "Maple Mono" :height 120)
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

;;; WHICH-KEY
(setopt which-key-idle-delay 0.1)
(which-key-mode 1)

;;; THEME
(defun my/set-theme (theme)
  "Set the current emacs theme to THEME. Disables all other themes."
  (interactive (list (intern (completing-read "Theme: " (custom-available-themes)))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (enable-theme theme))

;;;; MODUS THEMES
(setopt modus-themes-italic-constructs t)
(setopt modus-themes-bold-constructs t)
(setopt modus-themes-common-palette-overrides
        '((fg-line-number-inactive "gray50")
          (fg-line-number-active fg-main)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)))
(my/set-theme 'modus-operandi)

;;; DUMB JUMP
(use-package dumb-jump
  :ensure t
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

;;; SMART HUNGRY DELETE
(use-package smart-hungry-delete
  :ensure t
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
         ([remap delete-backward-char] . smart-hungry-delete-backward-char)
         ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

;;; VERTICO, CONSULT, MARGINALIA
(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :hook
  (emacs-startup-hook . vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :hook (vertico-mode-hook . marginalia-mode))

(use-package consult
  :ensure t
  :defer t
  :bind (("C-c f"   . consult-line)
         ("C-c r"   . nil)
         ("C-c r b" . consult-bookmark)
         ("C-c g g" . consult-ripgrep)))

;;; OLIVETTI
(use-package olivetti
  :ensure t
  :commands (olivetti-mode))

;;; WRITEROOM
(use-package writeroom-mode
  :ensure t
  :commands (writeroom-mode global-writeroom-mode))

;;; TREE-SITTER LANGUAGE SUPPORT
(use-package tree-sitter
  :ensure t
  :commands (tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :commands (tree-sitter-mode))

(use-package tsi
  :ensure (:host github :repo "orzechowskid/tsi.el")
  :commands (tsi-typescript-mode))

;;; MAGIT
(use-package magit
  :ensure t
  :commands (magit-status magit))

;;; MEOW KEYMAP
(use-package meow
  :ensure t
  :custom
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-use-clipboard t)
  :config
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("," . beginning-of-buffer)
   '("." . end-of-buffer))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("{" . meow-beginning-of-thing)
   '("}" . meow-end-of-thing)
   '("[" . backward-paragraph)
   '("]" . forward-paragraph)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
  (meow-global-mode 1))

;;; LANGUAGES

;;;; C/C++
(defun my/hook--cc-mode ()
  "Settings for `c-mode' and `c++-mode'"
  (c-set-style "bsd")
  (let ((size (my/lang-indent-size 'cc))
        (use-tabs (my/lang-indent-use-tabs 'cc)))
  (my/config-indent size use-tabs)
  (setq-local c-basic-offset size)))

(add-hook 'c-mode-hook #'my/hook--cc-mode)
(add-hook 'c++-mode-hook #'my/hook--cc-mode)

;;;; CSS
(defun my/hook--css-mode ()
  "Settings for `css-mode'."
  (let ((size (my/lang-indent-size 'css))
        (use-tabs (my/lang-indent-use-tabs 'css)))
    (my/config-indent size use-tabs)
    (setq-local css-indent-offset size)))

(add-hook 'css-mode-hook #'my/hook--css-mode)

;;;; GO
(use-package go-mode
  :ensure t
  :mode "\\.\\(?:go\\)\\'"
  :preface
  (defun my/hook--go-mode ()
    "Settings for `go-mode'."
    (let ((size (my/lang-indent-size 'go))
          (use-tabs (my/lang-indent-use-tabs 'go)))
    (my/config-indent size use-tabs)))
  :config
  (add-hook 'go-mode-hook #'my/hook--go-mode))

;;;; JAVASCRIPT
(defun my/hook--js-mode ()
  "Settings for `js-mode' and `js-jsx-mode'."
  (let ((size (my/lang-indent-size 'js))
        (use-tabs (my/lang-indent-use-tabs 'js)))
    (my/config-indent size use-tabs)
    (setq-local js-indent-level size)))

(add-hook 'js-mode-hook #'my/hook--js-mode)
(add-hook 'js-jsx-mode-hook #'my/hook--js-mode)

;;;; JSON
(defun my/hook--js-json-mode ()
  "Settings for `js-json-mode'"
  (let ((size (my/lang-indent-size 'json))
        (use-tabs (my/lang-indent-use-tabs 'json)))
    (my/config-indent size use-tabs)))

(add-hook 'js-json-mode-hook #'my/hook--js-json-mode)

;;;; LISP
(defun my/hook--lisp-modes ()
  "Settings for lisp language modes such as `emacs-lisp-mode'."
  (electric-pair-local-mode 1)
  (let ((size (my/lang-indent-size 'md))
    (use-tabs (my/lang-indent-use-tabs 'md)))
  (my/config-indent size use-tabs)))

(add-hook 'lisp-mode-hook #'my/hook--lisp-modes)
(add-hook 'emacs-lisp-mode-hook #'my/hook--lisp-modes)

;;;; MARKDOWN
(use-package markdown-mode
  :ensure t
  :mode "\\.\\(?:md\\|txt\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'"
  :preface
  (defun my/hook--markdown-mode ()
    "Settings for `markdown-mode'."
    (olivetti-mode 1)
    (visual-line-mode 1)

    (let ((size (my/lang-indent-size 'md))
          (use-tabs (my/lang-indent-use-tabs 'md)))
      (my/config-indent size use-tabs)))
  :custom
  (markdown-max-image-size '(512 . 512))
  :config
  (add-hook 'markdown-mode-hook #'my/hook--markdown-mode))

;;;; ORG
(use-package org
  :ensure nil
  :preface
  (defun my/hook--org-mode ()
    "Settings for `org-mode-hook'"
    (let ((size (my/lang-indent-size 'org))
          (use-tabs (my/lang-indent-use-tabs 'org)))
      (my/config-indent size use-tabs))
    (setq paragraph-start "\\|[    ]*$")
    (setq paragraph-separate "[     ]*$")
    (visual-line-mode 1)
    (display-line-numbers-mode -1))
  :hook (org-mode-hook . my/hook--org-mode)
  :custom
  (org-hide-leading-stars t)
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
  (with-eval-after-load 'evil
    (evil-define-key '(normal emacs) org-agenda-mode-map
      (kbd "j") 'org-agenda-next-item
      (kbd "k") 'org-agenda-previous-item
      (kbd "N") 'org-agenda-goto-date
      (kbd "P") 'org-agenda-capture))

  ;; Commands/functions/macros
  (defun my/org-toggle-markup ()
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
    (keymap-set map "C-c C-x RET" #'my/org-toggle-markup)))

(use-package org-modern
  :ensure t
  :hook ((org-mode-hook . global-org-modern-mode)
         (org-agenda-mode-hook . global-org-modern-mode))
  :custom
  (org-modern-keyword "» ")
  (org-modern-star '("◉" "●" "○" "◈" "◇"))
  (org-modern-block-fringe nil)
  (org-modern-hide-stars t)
  :config
  (defun my/org-modern-hide-star-headings ()
    "Remove all stars from a heading, increase heading sizes and disable `org-indent-mode'."
    (interactive)
    (setopt org-modern-hide-stars t)
    (setopt org-hide-leading-stars t)
    (org-mode-restart)
    (org-indent-mode -1))

  (defun my/org-modern-star-headings ()
    "Enable stars and `org-indent-mode'."
    (interactive)
    (setopt org-modern-hide-stars nil)
    (setopt org-hide-leading-stars nil)
    (org-mode-restart)
    (org-indent-mode 1)))

(use-package org-wc
  :ensure t
  :commands org-wc-display)

(use-package org-appear
  :ensure t
  :hook (org-mode-hook . org-appear-mode))

(use-package ox-epub
  :ensure t
  :commands (org-export-dispatch))

;;;; TYPESCRIPT
(use-package typescript-mode
  :ensure t
  :mode "\\.tsx?\\'"
  :preface
  (defun my/hook--typescript-mode ()
    "Settings for `typescript-mode'."
    (tree-sitter-mode 1)
    (tsi-typescript-mode 1)

    (let ((size (my/lang-indent-size 'ts))
          (use-tabs (my/lang-indent-use-tabs 'ts)))
      (my/config-indent size use-tabs)
      (setq-local typescript-indent-level size)))
  :config
  (add-hook 'typescript-mode-hook #'my/hook--typescript-mode)
  (with-eval-after-load 'tree-sitter
    (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . tsx))))

;;;; XML
(defun my/hook--xml-modes ()
  "Settings for xml modes like `html-mode' and `nxml-mode'."
  (let ((size (my/lang-indent-size 'xml))
        (use-tabs (my/lang-indent-use-tabs 'xml)))
    (my/config-indent size use-tabs)
    (setq-local sgml-basic-offset size)))

(add-hook 'html-mode-hook #'my/hook--xml-modes)

;;;; YAML
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'"
  :preface
  (defun my/hook--yaml-mode ()
    "Settings for `yaml-mode'."
    (let ((size (my/lang-indent-size 'yaml))
          (use-tabs (my/lang-indent-use-tabs 'yaml)))
      (my/config-indent size use-tabs)
      (setq-local yaml-indent-offset size)))
  :config
  (add-hook 'yaml-mode-hook #'my/hook--yaml-mode))

;; ═════════════════════════════[ Emacs ]═══════════════════════════════
;;
;;   _|_|_|                            _|  _|
;; _|          _|_|      _|_|      _|_|_|  _|_|_|    _|    _|    _|_|
;; _|  _|_|  _|    _|  _|    _|  _|    _|  _|    _|  _|    _|  _|_|_|_|
;; _|    _|  _|    _|  _|    _|  _|    _|  _|    _|  _|    _|  _|
;;   _|_|_|    _|_|      _|_|      _|_|_|  _|_|_|    _|_|_|_|   _|_|_|
;;                                                         _|
;;                                                      _|_|
;;
;; ══════════════════════════════[ End ]════════════════════════════════
