;;; -*- lexical-binding: t; -*-

(require 'my-core)

;;; BASIC
;;;; BASIC KEYBINDINGS

(keymap-global-set "C-z" nil)
(keymap-global-set "C-x C-z" nil)
(keymap-global-set "C-x C-k RET" nil)
(keymap-global-set "C-c C-b" nil)
(keymap-global-set "<mouse-3>" nil)

(keymap-global-set "M-[" #'backward-paragraph)
(keymap-global-set "M-]" #'forward-paragraph)
(keymap-global-set "M-s M-s" #'grep)
(keymap-global-set "C-c -" #'kill-buffer-and-window)
(keymap-global-set "C-c C-SPC" #'just-one-space)

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

(add-hook 'after-init-hook #'global-auto-revert-mode)

;;;; BOOKMARKS, RECENTS, HISTORY

(my/set bookmark-default-file (my/locate-user-var-file "bookmarks.el"))
(my/set recentf-save-file (my/locate-user-var-file "recentf.el"))
(my/set save-place-file (my/locate-user-var-file "save-place.el"))
(my/set savehist-file (my/locate-user-var-file "savehist.el"))
(my/set history-length 300)
(my/set save-place-limit 600)

(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'after-init-hook #'savehist-mode)

(keymap-global-set "C-c f b" #'bookmark-jump)
(with-eval-after-load 'recentf
  (keymap-global-set "C-c f r" #'recentf))

;;;; IBUFFER

(my/set ibuffer-human-readable-size t)

(keymap-global-set "C-x C-b" #'ibuffer)

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

(keymap-global-set "C-c f p" #'project-find-file)

(keymap-global-set "<f5>" #'project-compile)
(keymap-global-set "C-<f5>" #'compile)

;;;; DOCUMENT VIEWER

(my/set doc-view-resolution 200)

;;;; DICTIONARY

(my/set dictionary-server "dict.org")
(my/set dictionary-default-strategy "prefix")

(keymap-global-set "C-c d" #'dictionary-lookup-definition)

;;;; EDITING TEXT BASICS

(my/set backward-delete-char-untabify-method 'hungry)
(my/set tab-width 3)
(my/set indent-tabs-mode nil)
(my/set sentence-end-double-space nil)
(my/set kill-do-not-save-duplicates t)
(my/set kill-region-dwim (if (version< emacs-version "31") t 'emacs-word))

(add-hook 'after-init-hook #'delete-selection-mode)

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

(add-hook 'after-init-hook #'pixel-scroll-precision-mode)

;;;; SHORT ANSWER PROMPTS

(my/set read-answer-short t)
(my/set use-short-answers t)

;;;; WHITESPACE

(my/set whitespace-display-mappings '((tab-mark 9 [#x7C 9] [92 9])))
(my/set whitespace-style '(face tabs tab-mark trailing))
(my/set whitespace-line-column nil)

(add-hook 'prog-mode-hook #'whitespace-mode)

(keymap-global-set "C-c m w" #'whitespace-mode)

;;;; LINE NUMBERS

(my/set display-line-numbers-width 4)
(my/set display-line-numbers-widen t)

(my/set undo-limit 2080000)
(my/set undo-strong-limit 3120000)
(my/set undo-outer-limit 312000000)

(keymap-global-set "C-c m l" #'display-line-numbers-mode)

;;;; ERROR/WARNING BELLS

(my/set ring-bell-function #'ignore)
(my/set visible-bell nil)

;;;; MISCELLANEOUS

(my/set adaptive-fill-regexp "[-–!|#%;>*+·•‣⁃◦ 	]* +")
(my/set view-lossage-auto-refresh t)

(put 'narrow-to-region 'disabled nil)

(add-hook 'after-init-hook #'winner-mode)
(add-hook 'after-init-hook
          (defun my/--after-init--disable-default-modes ()
              (blink-cursor-mode -1)
              (electric-indent-mode -1)))
(add-hook 'after-init-hook
          (defun my/--after-init--split-window-direction ()
            (advice-add #'split-window-below :after (lambda (&rest _) (other-window 1)))
            (advice-add #'split-window-right :after (lambda (&rest _) (other-window 1)))))

;;; MODULES

(require 'my-config-utils)
(with-eval-after-load 'my-config-utils
  (keymap-global-set "<escape>" #'my/keyboard-quit-dwim)
  (keymap-global-set "C-g" #'my/keyboard-quit-dwim)
  (keymap-global-set "C-c f f" #'my/switch-frame)
  (add-to-list 'save-some-buffers-action-alist
               (list "d"
                     (lambda (buffer)
                       (diff-buffer-with-file (buffer-file-name buffer)))
                     "show diff between the buffer and its file")))

(require 'my-config-completions-minibuffer)
(with-eval-after-load 'my-config-completions-minibuffer
  (keymap-set minibuffer-local-map "C-<backspace>" #'my/minibuffer--backward-kill)
  (keymap-set minibuffer-local-map "M-<backspace>" #'my/minibuffer--backward-kill))

(require 'my-config-mode-line)
(with-eval-after-load 'my-config-mode-line
  (add-hook 'after-init-hook #'my/mode-line-mode)
  (add-hook 'after-init-hook #'my/mode-line-display-position-mode))

(require 'my-config-fonts-themes)
(with-eval-after-load 'my-config-fonts-themes
  (keymap-global-set "C-c C-0" 'my/font-size-set)
  (keymap-global-set "C-c C-1" 'my/font-family-set)
  (add-hook 'emacs-startup-hook #'my/theme-load-my-theme)
  (add-hook 'emacs-startup-hook #'my/font-load-my-font)
  (add-hook 'emacs-startup-hook #'my/font-load-emoji-fonts))

(require 'my-config-editor-langs)
(with-eval-after-load 'my-config-editor-langs
  (global-set-key [remap delete-backward-char] #'my/editor-delete-to-tab-stop)
  (global-set-key [remap delete-backward-char-untabify] #'my/editor-delete-to-tab-stop))

;;; END

(load (my/locate-user-etc-file "local-init.el") :no-error-if-file-is-missing :nomessage)
