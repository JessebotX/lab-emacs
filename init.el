;;; init.el -*- lexical-binding: t; -*-

;;; ├─ PREFACE

(defconst my/auto-save-files-directory (locate-user-emacs-file "var/auto-saves")
  "Directory to store temporary auto-save files.")

;;; ├─ GENERAL CONFIGURATION

(setq-default display-line-numbers-width 4)
(setq-default display-line-numbers-widen t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 3)

(setq adaptive-fill-regexp "[-–!|#%;>*+·•‣⁃◦ 	]* +")
(setq ansi-color-for-compilation-mode t)
(setq auto-save-file-name-transforms `((".*" ,my/auto-save-files-directory t)))
(setq auto-save-list-file-prefix (expand-file-name "sessions" my/auto-save-files-directory))
(setq auto-window-vscroll nil)
(setq backward-delete-char-untabify-method 'hungry)
(setq bookmark-default-file (locate-user-emacs-file "var/bookmarks.el"))
(setq create-lockfiles nil)
(setq compilation-always-kill t)
(setq compilation-scroll-output 'first-error)
(setq completion-ignore-case t)
(setq completions-detailed t)
(setq custom-file (locate-user-emacs-file "etc/emacs-custom.el"))
(setq delete-by-moving-to-trash t)
(setq delete-pair-blink-delay 0)
(setq delete-pair-push-mark t)
(setq dired-listing-switches "-g -h -o -a --group-directories-first")
(setq dictionary-server "dict.org")
(setq dictionary-default-strategy "prefix")
(setq dired-kill-when-opening-new-dired-buffer t)
(setq display-time-default-load-average nil)
(setq doc-view-resolution 200)
(setq global-auto-revert-non-file-buffers t)
(setq history-length 300)
(setq fast-but-imprecise-scrolling t)
(setq ibuffer-human-readable-size t)
(setq isearch-lazy-count t)
;;(setq jit-lock-defer-time nil)
(setq kill-buffer-delete-auto-save-files t)
(setq kill-do-not-save-duplicates t)
(setq kill-region-dwim (if (version< emacs-version "31") t 'emacs-word))
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")
(setq make-backup-files nil)
(setq multisession-directory (locate-user-emacs-file "var/multisession"))
(setq project-list-file (locate-user-emacs-file "var/projects.el"))
(setq read-answer-short t)
(setq recentf-save-file (locate-user-emacs-file "var/recentf.el"))
(setq redisplay-skip-fontification-on-input t)
(setq ring-bell-function 'ignore)
(setq save-place-file (locate-user-emacs-file "var/places.el"))
(setq save-place-limit 600)
(setq savehist-file (locate-user-emacs-file "var/savehist.el"))
(setq scroll-conservatively 8)
(setq scroll-preserve-screen-position t)
(setq sentence-end-double-space nil)
(setq show-paren-delay 0.1)
(setq show-paren-highlighting-openparen t)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)
(setq transient-history-file (locate-user-emacs-file "var/transient/history.el"))
(setq treesit-extra-load-path (list (locate-user-emacs-file "var/tree-sitter")))
(setq undo-limit (* 13 160000))
(setq undo-strong-limit (* 13 240000))
(setq undo-outer-limit (* 13 24000000))
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "^\\*")
(setq uniquify-separator "/")
(setq use-short-answers t)
(setq view-lossage-auto-refresh t)
(setq visible-bell nil)
(setq which-key-idle-delay 0.1)
(setq whitespace-display-mappings '((tab-mark 9 [#x7C 9] [92 9])))
(setq whitespace-style '(face tabs tab-mark trailing))
(setq whitespace-line-column nil)

(put 'narrow-to-region 'disabled nil)

;;; ├─ MODULES

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(require 'my-config-utils)
(keymap-global-set "C-c f f" #'my/switch-frame)
(keymap-global-set "<escape>" #'my/keyboard-quit-dwim)
(keymap-global-set "C-g" #'my/keyboard-quit-dwim)

(require 'my-config-fonts)
(keymap-global-set "C-c C-0" 'my/font-size-set)
(keymap-global-set "C-c C-1" 'my/font-family-set)
(add-hook 'after-init-hook #'my/font-load-my-font)
(add-hook 'after-init-hook #'my/font-load-emoji-fonts)

(require 'my-config-themes)
(keymap-global-set "C-<f5>" #'my/theme-set)
(add-hook 'after-init-hook #'my/theme-load-my-theme)

(require 'my-config-mode-line)
(add-hook 'enable-theme-functions #'my/subtle-mode-line-set-faces)
(add-hook 'after-init-hook #'my/mode-line-display-position-mode)

(require 'my-config-minibuffer)
(keymap-set minibuffer-local-map "C-<backspace>" #'my/minibuffer--backward-kill)
(keymap-set minibuffer-local-map "M-<backspace>" #'my/minibuffer--backward-kill)
(add-hook 'after-init-hook #'icomplete-vertical-mode)

(require 'my-config-editor-languages)
(global-set-key [remap delete-backward-char] #'my/editor-delete-to-tab-stop)
(global-set-key [remap delete-backward-char-untabify] #'my/editor-delete-to-tab-stop)
(keymap-global-set "C-c m t w" #'my/editor-writeroom-mode)
(keymap-global-set "C-S-c C-S-c" 'mc/edit-lines)
(keymap-global-set "C-M-n" 'mc/mark-next-like-this)
(keymap-global-set "C-M-p" 'mc/mark-previous-like-this)
(keymap-global-set "C->" 'mc/mark-next-like-this)
(keymap-global-set "C-<" 'mc/mark-previous-like-this)
(keymap-global-set "C-c C-<" 'mc/mark-all-like-this)

;;; ├─ KEYBINDINGS

(keymap-global-set "C-z" nil)
(keymap-global-set "C-x C-k RET" nil)
(keymap-global-set "C-x C-z" nil)
(keymap-global-set "C-c C-b" nil)
(keymap-global-set "<mouse-3>" nil)

(keymap-global-set "M-[" 'backward-paragraph)
(keymap-global-set "M-]" 'forward-paragraph)
(keymap-global-set "M-s M-s" 'grep)
(keymap-global-set "C-x C-b" 'ibuffer)
(keymap-global-set "C-c -" 'kill-buffer-and-window)
(keymap-global-set "C-c C-SPC" 'just-one-space)
(keymap-global-set "C-c d" 'dictionary-lookup-definition)
(keymap-global-set "C-c m l" 'display-line-numbers-mode)
(keymap-global-set "C-c m w" 'whitespace-mode)

(keymap-global-set "C-c f b" 'bookmark-jump)
(keymap-global-set "C-c f p" 'project-find-file)
(keymap-global-set "<f8>" 'project-compile)
(with-eval-after-load 'recentf
  (keymap-global-set "C-c f r" 'recentf))

;;; ├─ HOOKS

(add-hook 'after-init-hook
          (defun my/emacs-after-init-hook ()
            (advice-add #'split-window-below :after (lambda (&rest _) (other-window 1)))
            (advice-add #'split-window-right :after (lambda (&rest _) (other-window 1)))

            (blink-cursor-mode -1)
            (delete-selection-mode 1)
            (electric-indent-mode -1)
            (global-auto-revert-mode 1)
            (pixel-scroll-precision-mode 1)
            (save-place-mode 1)
            (savehist-mode 1)
            (which-key-mode 1)
            (winner-mode 1)))

(add-hook 'prog-mode-hook #'whitespace-mode)

;;; ├─ END

(load (locate-user-emacs-file "etc/local-init.el") :no-error-if-file-is-missing :nomessage) ; should be ignored in version control
