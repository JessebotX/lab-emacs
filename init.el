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
  '(
    (css  :size 4 :use-tabs nil)
    (go   :size 8 :use-tabs t)
    (html :size 4 :use-tabs nil)
    (js   :size 4 :use-tabs nil)
    (md   :size 2 :use-tabs nil)
    (ts   :size 4 :use-tabs nil)
    (xml  :size 2 :use-tabs nil)
    )
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

(defcustom my/indent-use-tabs-default t
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

(setopt ad-redefinition-action 'accept) ; disable warning about advice de/activation
(setopt backward-delete-char-untabify-method 'hungry)
(setopt completion-ignore-case t)
(setopt delete-by-moving-to-trash t)
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

;(setq auto-window-vscroll nil)
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
(keymap-global-set "C-c C-s C-f" 'my/goto-config-init)
(keymap-global-set "M-]" 'forward-paragraph)
(keymap-global-set "M-[" 'backward-paragraph)

;;; AUTO REVERT MODE
(setopt global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;;; DISPLAY LINE NUMBERS MODE
(setopt display-line-numbers-type 'relative)
(setopt display-line-numbers-widen t)
(setopt display-line-numbers-width 3)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; FONT
(set-face-attribute 'default nil :family "Maple Mono" :height 105)

;;; ICOMPLETE
(icomplete-vertical-mode 1)

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

;;; SMART HUNGRY DELETE
(use-package smart-hungry-delete
  :ensure t
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
	     ([remap delete-backward-char] . smart-hungry-delete-backward-char)
	     ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

;;; OLIVETTI
(use-package olivetti
  :ensure t
  :commands (olivetti-mode))

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
