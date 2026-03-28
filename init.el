;;; init.el -*- lexical-binding: t; -*-

;;; ├─ PREFACE

(defconst my/auto-save-files-directory (expand-file-name "var/auto-saves" user-emacs-directory)
  "Directory to store temporary auto-save files.")

;;; ├─ GENERAL CONFIGURATION

(setq-default display-line-numbers-width 4
              display-line-numbers-widen t
              indent-tabs-mode nil
              tab-width 4)

(setq ansi-color-for-compilation-mode t
      auto-save-file-name-transforms `((".*" ,my/auto-save-files-directory t))
      auto-save-list-file-prefix (expand-file-name "sessions" my/auto-save-files-directory)
      ;; auto-window-vscroll nil
      backward-delete-char-untabify-method 'hungry
      bookmark-default-file (expand-file-name "etc/bookmarks.el" user-emacs-directory)
      create-lockfiles nil
      compilation-always-kill t
      compilation-scroll-output 'first-error
      completion-ignore-case t
      completions-detailed t
      custom-file (expand-file-name "etc/custom.el" user-emacs-directory)
      delete-by-moving-to-trash t
      delete-pair-blink-delay 0
      delete-pair-push-mark t
      dictionary-server "dict.org"
      dictionary-default-strategy "prefix"
      dired-kill-when-opening-new-dired-buffer t
      display-time-default-load-average nil
      doc-view-resolution 200
      global-auto-revert-non-file-buffers t
      history-length 300
      ;; fast-but-imprecise-scrolling t
      ibuffer-human-readable-size t
      isearch-lazy-count t
      jit-lock-defer-time 0
      kill-buffer-delete-auto-save-files t
      kill-do-not-save-duplicates t
      kill-region-dwim (if (version< emacs-version "31") t 'emacs-word)
      lazy-count-prefix-format nil
      lazy-count-suffix-format "   (%s/%s)"
      make-backup-files nil
      mode-line-compact t
      mode-line-percent-position nil
      multisession-directory (expand-file-name "var/multisession" user-emacs-directory)
      project-list-file (expand-file-name "var/projects.el" user-emacs-directory)
      read-answer-short t
      redisplay-skip-fontification-on-input t
      ring-bell-function 'ignore
      save-place-file (expand-file-name "var/places.el" user-emacs-directory)
      save-place-limit 600
      savehist-file (expand-file-name "var/savehist.el" user-emacs-directory)
      scroll-conservatively 8
      scroll-preserve-screen-position t
      sentence-end-double-space nil
      show-paren-delay 0.1
      show-paren-highlighting-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t
      undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000)
      uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*"
      uniquify-separator "/"
      use-short-answers t
      visible-bell nil
      which-key-idle-delay 0.1
      whitespace-display-mappings '((tab-mark 9 [#x7C 9] [92 9]))
      whitespace-style '(face tabs tab-mark trailing)
      whitespace-line-column nil)

(put 'narrow-to-region 'disabled nil)

;;; ├─ MODULES

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'my-config-utils)
(keymap-global-set "<escape>" #'my/keyboard-quit-dwim)
(keymap-global-set "C-g" #'my/keyboard-quit-dwim)

(require 'my-config-fonts)
(keymap-global-set "C-<f5>" #'my/theme-set)
(keymap-global-set "<f5>" #'my/theme-toggle)
(keymap-global-set "<f8>" 'variable-pitch-mode)
(add-hook 'after-init-hook #'my/font-load-my-font)
(add-hook 'after-init-hook #'my/font-load-emoji-fonts)

(require 'my-config-themes)
(add-hook 'after-init-hook #'my/theme-load-my-theme)

(require 'my-config-mode-line)

(require 'my-config-minibuffer)
(keymap-set minibuffer-local-map "C-<backspace>" #'my/minibuffer--backward-kill)
(keymap-set minibuffer-local-map "M-<backspace>" #'my/minibuffer--backward-kill)
(add-hook 'after-init-hook #'icomplete-vertical-mode)

(require 'my-config-editor-languages)
(global-set-key [remap delete-backward-char] #'my/editor-delete-to-tab-stop)
(global-set-key [remap delete-backward-char-untabify] #'my/editor-delete-to-tab-stop)
(keymap-global-set "C-c m t w" #'my/editor-writeroom-mode)

;;; ├─ HOOKS

(add-hook 'after-init-hook
          (lambda ()
            (advice-add #'split-window-below :after (lambda (&rest _) (other-window 1)))
            (advice-add #'split-window-right :after (lambda (&rest _) (other-window 1)))

            (blink-cursor-mode -1)
            (column-number-mode 1)
            (delete-selection-mode 1)
            (electric-indent-mode -1)
            (global-auto-revert-mode 1)
            (pixel-scroll-precision-mode 1)
            (save-place-mode 1)
            (savehist-mode 1)
            (which-key-mode 1)
            (winner-mode 1)))

;;; ├─ KEYBINDINGS

(keymap-global-set "C-z" nil)
(keymap-global-set "C-x C-k RET" nil)
(keymap-global-set "C-x C-z" nil)
(keymap-global-set "C-c C-b" nil)

(keymap-global-set "M-[" 'backward-paragraph)
(keymap-global-set "M-]" 'forward-paragraph)
(keymap-global-set "M-s M-s" 'grep)
(keymap-global-set "C-x C-b" 'ibuffer)
(keymap-global-set "C-c -" 'kill-buffer-and-window)
(keymap-global-set "C-c C-SPC" 'just-one-space)
(keymap-global-set "C-c d" 'dictionary-lookup-definition)
(keymap-global-set "C-c m l" 'display-line-numbers-mode)
(keymap-global-set "C-c m w" 'whitespace-mode)

;;; ├─ END

(load (expand-file-name "local-init.el" user-emacs-directory) :no-error-if-file-is-missing :nomessage) ; should be ignored in version control
